Games <- R6::R6Class(
  classname = "Games",
  cloneable = FALSE,
  class = FALSE,
  public = list(
    rvs = "<reactiveValues>",
    count = 0,
    fcount = 0, # folder count
    vendors = list(),
    player = NULL,
    initialize = function(vendors) {
      self$rvs <- reactiveValues()
      self$vendors <- vendors
      self$remove_dead_dirs() # For safety.

      path <- app_file("data", "games")
      dirs <- list.dirs(path, recursive = FALSE)
      if (length(dirs) != 0) {
        self$rvs$games <- lapply(dirs, read_game, vendors = vendors)
        nms <- vapply(self$rvs$games, function(x) x$name(), character(1))
        names(self$rvs$games) <- nms
      } else {
        self$rvs$games <- list()
      }

      path <- file.path(app_file("data", "games"), "fcount.rds")
      if (file.exists(path)) {
        self$fcount <- readRDS(path)
      }
    },
    remove_dead_dirs = function() {
      path <- app_file("data", "games")
      dirs <- list.dirs(path, recursive = FALSE)
      empty_dirs <- vapply(dirs, function(x) length(list.files(x)), numeric(1), USE.NAMES = FALSE)
      empty_dirs <- dirs[empty_dirs == 0]
      if (length(empty_dirs) > 0) {
        purrr::walk(empty_dirs, unlink, recursive = TRUE, force = TRUE)
        msg <- paste(
          format(Sys.time(), "%X -"),
          message = paste("Deleted dead folders:", paste(empty_dirs, collapse = ", "))
        )
        file <- file.path(app_file("log"), paste0(Sys.Date(), ".log"))
        write(msg, file, append = TRUE)
      }
    },
    save_fcount = function() {
      path <- file.path(app_file("data", "games"), "fcount.rds")
      saveRDS(self$fcount, path, compress = FALSE)
    },
    new_game = function(name, date, tiras_n) {
      if (!name %in% self$names()) {
        # TODO: check name is appropiate
        self$count <- self$count + 1
        self$fcount <- self$fcount + 1
        self$rvs$games[[name]] <- Game$new(
          vendors = self$vendors, type = "new",
          name = name, folder_id = self$fcount,
          date = date, cards_n = tiras_n * 6
        )
        self$save_fcount()
      } else {
        stop2(paste0("Ya existe una partida con el nombre '", name, "'"))
      }
    },
    remove_game = function(name) {
      if (name %in% self$names()) {
        delete_dir(self$rvs$games[[name]]$path())
        self$rvs$games[[name]] <- NULL
        self$count <- self$count - 1
        self$save_fcount()
      } else {
        stop2("No se puede eliminar una partida que no existe.")
      }
    },
    return_game = function(game) {
      self$rvs$games[[game]]
    },
    names = function(type = c("all", "played", "unplayed", "cards_generated")) {
      type <- match.arg(type)
      nms <- names(self$rvs$games)
      if (type == "all") {
        return(nms)
      }
      if (type == "cards_generated") {
        lgl <- vapply(self$rvs$games, function(x) x$rvs$meta$cards_generated, logical(1))
        return(nms[lgl])
      }
      lgl <- vapply(self$rvs$games, function(x) x$rvs$meta$played, logical(1))
      if (type == "played") {
        return(nms[lgl])
      } else {
        return(nms[!lgl])
      }
    },
    sales = function(game) {
      self$rvs$games[[game]]$sales()
    },
    sales_by_vendor = function(game = NULL, vendor_id = NULL) {
      if (is.null(game)) {
        games <- self$names("unplayed")
        out_list <- lapply(games, self$sales_by_vendor, vendor_id = vendor_id)
        return(do.call(rbind, out_list))
      } else {
        df <- self$sales(game)
        df <- df[df$id == vendor_id, ]
        if (nrow(df) != 0) {
          df <- cbind(df, list("partida" = game))
        }
        return(df)
      }
    },
    sales_count = function(game) {
      self$rvs$games[[game]]$sales_count()
    },
    add_sales = function(game, institucion, desde, hasta) {
      self$rvs$games[[game]]$add_sales(institucion, desde, hasta)
    },
    remove_sales = function(game, desde, hasta) {
      self$rvs$games[[game]]$remove_sales(desde, hasta)
    },
    remove_sales_by_vendor = function(game = NULL, vendor_id) {
      if (is.null(game)) {
        games <- self$names("unplayed")
        purrr::walk(games, self$remove_sales_by_vendor, vendor_id = vendor_id)
      } else {
        self$rvs$games[[game]]$remove_sales_by_vendor(vendor_id)
      }
    },
    print_sales_summary = function(game) {
      data <- self$sales(game)
      if (nrow(data) == 0) {
        "No se registran ventas para esta partida."
      } else {
        imprimir_ventas(data, self$cards_n(game))
      }
    },
    date = function(game) {
      self$rvs$games[[game]]$date()
    },
    cards_n = function(game) {
      self$rvs$games[[game]]$cards_n()
    },
    serie = function(game) {
      self$rvs$games[[game]]$serie()
    },
    cards_status = function(game, lgl) {
      self$rvs$games[[game]]$cards_status(lgl)
    },
    path = function(game) {
      self$rvs$games[[game]]$path()
    },
    modify_game = function(game, tiras_n, date) {
      cards_n <- tiras_n * 6
      self$rvs$games[[game]]$modify_game(cards_n, date)
    },
    set_game = function(game, settings = NULL) {
      self$rvs$games[[game]]$set_game(settings)
    },
    finalize_game = function(game, info) {
      self$rvs$games[[game]]$finalize_game(info)
    },
    results = function(game) {
      self$rvs$games[[game]]$results()
    }
  )
)

Game <- R6::R6Class(
  classname = "Game",
  cloneable = FALSE,
  class = FALSE,
  public = list(
    rvs = "<reactiveValues>",
    vendors = list(),
    initialize = function(vendors, type = c("new", "load"), ...) {
      type <- match.arg(type)
      self$rvs <- reactiveValues()
      self$vendors <- vendors
      switch(type,
        "new" = self$new(...),
        "load" = self$load(...)
      )
      self$save()
    },
    load = function(...) {
      args <- list(...)
      self$rvs$sales <- args$sales
      self$rvs$meta <- args$meta
    },
    new = function(...) {
      args <- list(...)
      self$rvs$meta <- list(
        "played" = FALSE, "folder_id" = args$folder_id,
        "serie" = generate_serie(args$folder_id),
        "name" = args$name, "date" = args$date,
        "cards_n" = args$cards_n, "cards_generated" = FALSE
      )

      path <- app_file("data", "games")
      path <- file.path(path, paste0("game_", self$rvs$meta$folder_id))
      self$rvs$meta$path <- path
      self$rvs$sales <- data.frame(matrix(nrow = 0, ncol = 4), stringsAsFactors = FALSE)
      colnames(self$rvs$sales) <- c("id", "institucion", "desde", "hasta")
    },
    save = function() {
      folder <- app_file("data", "games")
      folder <- file.path(folder, paste0("game_", self$rvs$meta$folder_id))
      if (!dir.exists(folder)) dir.create(folder)
      saveRDS(self$rvs$sales, file.path(folder, "sales.rds"), compress = FALSE)
      saveRDS(self$rvs$meta, file.path(folder, "meta.rds"), compress = FALSE)
    },
    name = function() {
      self$rvs$meta$name
    },
    path = function() {
      self$rvs$meta$path
    },
    sales = function() {
      self$rvs$sales
    },
    sales_count = function() {
      if (nrow(self$rvs$sales) == 0) {
        return(0)
      } else {
        cartones_cargados <- Map(`:`, self$rvs$sales$desde, self$rvs$sales$hasta)
        return(length(unlist(cartones_cargados)))
      }
    },
    check_sales_range = function(desde, hasta) {
      if (nrow(self$rvs$sales) == 0) {
        return(TRUE)
      }
      cartones_cargados <- Map(`:`, self$rvs$sales$desde, self$rvs$sales$hasta)
      if (any(desde:hasta %in% unlist(cartones_cargados))) {
        return(FALSE)
      }
      return(TRUE)
    },
    add_sales = function(institucion, desde, hasta) {
      range_check <- self$check_sales_range(desde, hasta)
      if (range_check) {
        self$rvs$sales[nrow(self$rvs$sales) + 1, ] <- list(
          self$vendors$get_institutions()[institucion],
          institucion,
          desde,
          hasta
        )
        self$sort_sales()
        self$save()
      } else {
        stop2("Al menos uno de los cartones que intentas cargar ya fue cargado.")
      }
    },
    remove_sales = function(desde, hasta) {
      self$rvs$sales <- eliminar_cartones(self$rvs$sales, desde:hasta)
      self$save()
    },
    remove_sales_by_vendor = function(vendor_id) {
      self$rvs$sales <- self$rvs$sales[self$rvs$sales$id != vendor_id, ]
      self$save()
    },
    sort_sales = function() {
      self$rvs$sales <- self$rvs$sales[order(self$rvs$sales$desde), ]
    },
    date = function() {
      self$rvs$meta$date
    },
    cards_n = function() {
      self$rvs$meta$cards_n
    },
    serie = function() {
      self$rvs$meta$serie
    },
    cards_status = function(lgl) {
      if (lgl) {
        self$rvs$meta$cards_generated <- TRUE
        self$save()
      }
    },
    modify_game = function(cards_n, date) {
      self$rvs$meta$cards_n <- cards_n
      self$rvs$meta$date <- date
      self$save()
    },
    finalize_game = function(info) {
      self$generate_report(info)
      self$rvs$meta$finalized_info <- info
      self$rvs$meta$played <- TRUE
      self$save()
    },
    generate_report = function(info) {
      file_path <- file.path(self$path(), "report.pdf")
      grDevices::pdf(file_path, width = REPORT_WIDTH, height = REPORT_HEIGHT)
      print_report(info)
      grDevices::dev.off()
    },
    results = function() {
      # FIXME
      if (self$rvs$meta$played) {
        serie <- self$rvs$meta$finalized_info$parameters$serie
        date_start <- self$rvs$meta$finalized_info$parameters$date_start
        date_end <- self$rvs$meta$finalized_info$parameters$date_end
        cards_n <- self$rvs$meta$finalized_info$parameters$cards_n
        balls_n <- length(self$rvs$meta$finalized_info$parameters$sequence)
        winners <- self$rvs$meta$finalized_info$winners

        winners <- vapply(winners, function(x) x$prize, character(1))
        winners_names <- unique(winners)
        winners_count <- numeric(length(winners_names))

        for (i in seq_along(winners_count)) {
          winners_count[i] <- sum(winners == winners_names[i])
        }

        return(list(
          "serie" = serie, "cards_n" = cards_n, "balls_n" = balls_n,
          "date_start" = date_start, "date_end" = date_end,
          "winners_names" = winners_names,
          "winners_count" = winners_count
        ))
      }
    }
  )
)


# Utils -------------------------------------------------------------------
read_game <- function(path, vendors) {
  meta <- readRDS(file.path(path, "meta.rds"))
  if (!meta$played) {
    sales <- readRDS(file.path(path, "sales.rds"))
  } else {
    sales <- NULL
  }
  Game$new(vendors = vendors, type = "load", meta = meta, sales = sales)
}

generate_serie <- function(id) {
  serie <- stringr::str_pad(id, width = 4, side = "left", pad = "0")
  paste(unclass(format(Sys.Date(), "%y")), "-", serie)
}


# Imprimir reportes ------------------------------------------------------------
print_header <- function() {
  grid::grid.text(
    label = "BONO INSTITUCIONAL",
    x = 0.5,
    y = 0.75,
    just = "center",
    gp = grid::gpar(fontsize = 16, fontface = "bold")
  )
  grid::grid.text(
    label = "Secuencia de sorteo y listado de ganadores",
    x = 0.5,
    y = 0.3,
    just = "center",
    gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
}

print_parameters <- function(parameters) {
  name <- parameters$name
  serie <- parameters$serie
  cards_n <- parameters$cards_n
  date_start <- parameters$date_start
  date_end <- parameters$date_end
  grid::grid.text(
    label = "Parametros",
    x = grid::unit(0.025, "npc"),
    y = grid::unit(0.9, "npc"),
    just = "left",
    gp = grid::gpar(fontsize = 12, fontface = "bold")
  )
  grid::grid.text(
    label = paste(
      "Nombre", "Serie", "Cantidad de cartones en juego",
      "Inicio", "Finalizacion",
      sep = "\n"
    ),
    x = grid::unit(0.025, "npc"),
    y = grid::unit(0.8, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontsize = 12)
  )
  grid::grid.text(
    label = paste(
      name,
      serie,
      cards_n,
      format(date_start, "%m/%d/%Y %H:%M:%S"),
      format(date_end, "%m/%d/%Y %H:%M:%S"),
      sep = "\n"
    ),
    x = grid::unit(0.975, "npc"),
    y = grid::unit(0.8, "npc"),
    just = c("right", "top"),
    gp = grid::gpar(fontsize = 12)
  )
}

print_results <- function(prizes) {
  prize_label <- character(length(prizes))
  prize_ball_n <- numeric(length(prizes))
  for (i in seq_along(prizes)) {
    prize <- prizes[[i]]
    prize_label[i] <- paste("Ganador", toupper(prize$name), "en bolilla")
    prize_ball_n[i] <- length(prize$draws)
  }
  grid::grid.text(
    label = "Resultados del sorteo",
    x = grid::unit(0.025, "npc"),
    y = grid::unit(0.9, "npc"),
    just = "left",
    gp = grid::gpar(fontsize = 12, fontface = "bold")
  )
  grid::grid.text(
    label = paste(prize_label, collapse = "\n"),
    x = grid::unit(0.025, "npc"),
    y = grid::unit(0.8, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontsize = 12)
  )
  grid::grid.text(
    label = paste(prize_ball_n, collapse = "\n"),
    x = grid::unit(0.975, "npc"),
    y = grid::unit(0.8, "npc"),
    just = c("right", "top"),
    gp = grid::gpar(fontsize = 12)
  )
}

print_sequence <- function(sequence) {
  grid::grid.text(
    label = "Secuencia de bolillas",
    x = grid::unit(0.025, "npc"),
    y = grid::unit(0.925, "npc"),
    just = "left",
    gp = grid::gpar(fontsize = 12, fontface = "bold")
  )
  sequence <- paste(strwrap(paste0(sequence, collapse = ", "), width = 98),
    collapse = "\n"
  )
  grid::grid.text(
    label = sequence,
    x = grid::unit(0.025, "npc"),
    y = grid::unit(0.775, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontsize = 12)
  )
}

print_winner_card <- function(numbers) {
  rows <- 3
  cols <- 9
  g <- make_grid(3, 9, "grey")
  centers_rows <- g$row[-1] - grid::unit(1 / (rows * 2), "npc")
  centers_cols <- g$col[-1] - grid::unit(1 / (cols * 2), "npc")
  x_coords <- rep(centers_cols, each = rows)
  y_coords <- rep(rev(centers_rows), cols)

  grid::pushViewport(grid::viewport(0.5, 0.5, 0.95, 0.80))
  grid::grid.grill(h = g$row, v = g$col, gp = grid::gpar(col = "grey"))

  lgl <- ifelse(numbers == 0, FALSE, TRUE)

  grid::grid.text(
    label = numbers[lgl],
    x = x_coords[lgl],
    y = y_coords[lgl],
    gp = grid::gpar(fontsize = 12)
  )
  grid::grid.rect(
    x = x_coords[!lgl],
    y = y_coords[!lgl],
    height = grid::unit(1 / rows, "npc"),
    width = grid::unit(1 / cols, "npc"),
    gp = grid::gpar(
      col = NA,
      fill = farver::encode_colour(farver::decode_colour("grey"), 0.7)
    )
  )
  grid::popViewport()
}

print_winner_info <- function(card_id, seller, prize) {
  grid::grid.text(
    label = paste("Premio", "Numero de carton", "Vendedor", sep = "\n"),
    x = grid::unit(0.025, "npc"),
    y = grid::unit(0.925, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontsize = 11)
  )
  grid::grid.text(
    label = paste(prize, card_id, seller, sep = "\n"),
    x = grid::unit(0.975, "npc"),
    y = grid::unit(0.925, "npc"),
    just = c("right", "top"),
    gp = grid::gpar(fontsize = 11)
  )
}

print_winner <- function(winner) {
  lyt <- grid::grid.layout(ncol = 2, widths = c(0.55, 0.45))
  grid::pushViewport(grid::viewport(0.5, 0.5, 1, 0.975, layout = lyt))

  grid::pushViewport(grid::viewport(layout.pos.col = 1))
  print_winner_card(winner$numbers)
  grid::popViewport()

  grid::pushViewport(grid::viewport(layout.pos.col = 2))
  print_winner_info(winner$id, winner$seller, winner$prize)
  grid::popViewport(2)
}

make_grid <- function(rows, cols, col = "grey") {
  lines_rows <- grid::unit((0:rows) / rows, "npc")
  lines_cols <- grid::unit((0:cols) / cols, "npc")
  return(list("row" = lines_rows, "col" = lines_cols))
}

print_report <- function(game_info) {
  # Primera carilla, siempre se imprime.
  lyt <- grid::grid.layout(nrow = 2, heights = c(0.4, 0.6))
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(col = NULL, fill = "white"))

  grid::pushViewport(grid::viewport(0.5, 0.5, 0.85, 0.9, layout = lyt))

  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))

  lyt <- grid::grid.layout(nrow = 3, heights = c(0.2, 0.4, 0.4))
  grid::pushViewport(grid::viewport(0.5, 0.5, 1, 1, layout = lyt))

  # Encabezado
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print_header()
  grid::popViewport()

  # Parametros
  grid::pushViewport(grid::viewport(layout.pos.row = 2))
  lyt <- grid::grid.layout(ncol = 2, widths = c(0.5, 0.5))
  grid::pushViewport(grid::viewport(0.5, 0.5, 1, 1, layout = lyt))

  grid::pushViewport(grid::viewport(layout.pos.col = 1))
  print_parameters(game_info$parameters)
  grid::popViewport()

  # Resultados
  grid::pushViewport(grid::viewport(layout.pos.col = 2))
  print_results(game_info$prizes)
  grid::popViewport(3)

  # Secuencia
  grid::pushViewport(grid::viewport(layout.pos.row = 3))
  print_sequence(game_info$parameters$sequence)
  grid::popViewport(3)

  # Cartones ganadores - primera pagina
  grid::pushViewport(grid::viewport(layout.pos.row = 2))

  lyt <- grid::grid.layout(nrow = 6)
  grid::pushViewport(grid::viewport(0.5, 0.5, 1, 1, layout = lyt))

  winners <- list()
  for (prize in game_info$prizes) {
    winners_ <- list()
    for (i in seq_along(prize$winners)) {
      winner <- prize$winners[[i]]
      winners_[[i]] <- list(
        numbers = winner$card,
        id = winner$id,
        seller = winner$seller,
        prize = prize$name
      )
    }
    winners <- c(winners, winners_)
  }
  winners_n <- length(winners)

  if (winners_n > 6) {
    rows <- seq_len(6)
  } else {
    rows <- seq_len(winners_n)
  }

  for (i in rows) {
    grid::pushViewport(grid::viewport(layout.pos.row = i))
    grid::grid.lines(x = c(0.01, 0.99), y = c(0.99, 0.99))
    print_winner(winners[[i]])
    grid::popViewport()
  }
  grid::popViewport(2)

  # Cartones ganadores - segunda pagina y mas (si es necesario)
  if (length(winners) > 6) {
    winners <- winners[-c(1:6)]
  } else {
    return(NULL)
  }
  extra_sheets <- ((length(winners) - 1) %/% 10) + 1

  for (i in seq_len(extra_sheets)) {
    if (length(winners) > 10) {
      winners2 <- winners[1:10]
      winners <- winners[-c(1:10)]
    } else {
      winners2 <- winners
    }
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(col = NULL, fill = "white"))

    lyt <- grid::grid.layout(nrow = 10)
    grid::pushViewport(grid::viewport(0.5, 0.5, 0.85, 0.9, layout = lyt))

    for (i in seq_along(winners2)) {
      grid::pushViewport(grid::viewport(layout.pos.row = i))
      grid::grid.lines(x = c(0.01, 0.99), y = c(0.99, 0.99))
      print_winner(winners2[[i]])
      grid::popViewport()
    }
    grid::popViewport()
  }
}
