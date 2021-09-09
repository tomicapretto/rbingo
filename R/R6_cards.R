Cards <- R6::R6Class(
  classname = "Cards",
  cloneable = FALSE,
  class = FALSE,
  public = list(
    strips_print = NULL,
    strips_nums = NULL,
    row_pos = NULL,
    card_pos = NULL,
    strips_path = NULL,
    loaded = FALSE,
    initialize = function() {
      path <- file.path(app_file("data", "cards"), "tiras.rds")
      if (file.exists(path)) {
        self$strips_path <- path
      } else {
        stop2("No hay tiras disponibles!")
      }
    },
    load_strips = function() {
      tiras <- readRDS(self$strips_path)
      self$strips_print <- tiras$imprimir
      self$strips_nums <- tiras$numeros
      self$row_pos <- tiras$fila_pos
      self$card_pos <- tiras$carton_pos
      self$loaded <- TRUE
    },
    print_pdf = function(cards_n, serie, color) {
      tiras_n <- cards_n / 6
      tiras <- self$strips_print[seq_len(tiras_n)]
      graficar_tiras(tiras, serie, color)
    },

    get_card_by_id = function(id) {
      strip_id <- ((id - 1) %/% 6) + 1
      idx <- ((id - 1) %% 6) + 1
      strip <- self$strips_print[[strip_id]]
      card <- as.vector(strip[(3 * idx - 2):(3 * idx), ])
      return(card)
    }
  )
)

# Utils ------------------------------------------------------------------------
make_grid <- function(rows, cols, col = "grey") {
  lines_rows <- grid::unit((0:rows) / rows, "npc")
  lines_cols <- grid::unit((0:cols) / cols, "npc")
  return(list("row" = lines_rows, "col" = lines_cols))
}

graficar_tiras <- function(tiras, serie, col = "#2980B9",
                           width_row = 0.925, width_col = 0.975) {
  rows <- 3
  cols <- 9
  g <- make_grid(rows, cols, col = col)
  centers_rows <- g$row[-1] - grid::unit(1 / (rows * 2), "npc")
  centers_cols <- g$col[-1] - grid::unit(1 / (cols * 2), "npc")
  x_coords <- rep(centers_cols, each = rows)
  y_coords <- rep(rev(centers_rows), cols)

  CARTONES_N <- paste(paste0("CARTON N", intToUtf8(176)), seq_len(length(tiras) * 6))

  hojas_n <- ceiling(length(tiras) / 2)
  carton_idx <- 0
  tira_idx <- 0
  tira_checkers <- round(seq(1, length(tiras), length.out = 10))
  completed_pct <- 0

  for (hoja_idx in seq_len(hojas_n)) {
    l <- grid::grid.layout(nrow = 6, ncol = 3, widths = c(48.75, 2.5 + 3.75, 48.75))
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(col = NULL, fill = "white"))

    vp_mid <- grid::viewport(0.5, 0.5, width_row, width_col, layout = l)
    grid::pushViewport(vp_mid)

    for (j in c(1, 3)) {
      tira_idx <- tira_idx + 1
      tira <- tiras[[tira_idx]]

      if (tira_idx %in% tira_checkers) {
        completed_pct <- completed_pct + 10
        incProgress(0.1, detail = paste0(completed_pct, "% completado"))
      }

      for (i in 1L:l$nrow) {
        carton_idx <- carton_idx + 1
        vp_inner <- grid::viewport(layout.pos.row = i, layout.pos.col = j)
        grid::pushViewport(vp_inner)

        grid::grid.text(
          label = CARTONES_N[carton_idx],
          x = 0,
          y = 0.96,
          just = "left",
          gp = grid::gpar(fontsize = 9)
        )
        grid::grid.text(
          label = serie,
          x = 1,
          y = 0.96,
          just = "right",
          gp = grid::gpar(fontsize = 9)
        )

        vp_mid_inner <- grid::viewport(0.5, 0.5, 1, 0.80)
        grid::pushViewport(vp_mid_inner)
        grid::grid.grill(h = g$row, v = g$col, gp = grid::gpar(col = col))

        labels <- as.vector(tira[(3 * i - 2):(3 * i), ])
        lgl <- ifelse(labels == 0, FALSE, TRUE)

        grid::grid.text(
          label = labels[lgl],
          x = x_coords[lgl],
          y = y_coords[lgl],
          gp = grid::gpar(fontsize = 18)
        )
        grid::grid.rect(
          x = x_coords[!lgl],
          y = y_coords[!lgl],
          height = grid::unit(1 / rows, "npc"),
          width = grid::unit(1 / cols, "npc"),
          gp = grid::gpar(
            col = NA,
            fill = farver::encode_colour(farver::decode_colour(col), 0.7)
          )
        )
        # End
        grid::popViewport()
        grid::popViewport()
      }
    }
    grid::popViewport()
  }
}
