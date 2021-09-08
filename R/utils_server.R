showError <- function(msg) {
  shinypop::nx_notify_error(msg)
  req(FALSE)
}

showInfo <- function(msg) {
  shinypop::nx_notify_info(msg)
}

showSuccess <- function(msg) {
  shinypop::nx_notify_success(msg)
}

stop2 <- function(message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c("bingo.error", "error", "condition")
  )
  stop(err)
}

eliminar_cartones <- function(df, eliminar) {
  df <- df[order(df$desde), ]
  cartones_cargados <- Map(`:`, df$desde, df$hasta)

  eliminar <- eliminar[eliminar <= max(df$hasta) & eliminar >= min(df$desde)]
  df$lgl <- sapply(cartones_cargados, function(x) any(eliminar %in% x))

  df_modificar <- df[df$lgl, c("id", "institucion", "desde", "hasta")]
  cartones_cargados_mod <- cartones_cargados[df$lgl]
  df_modificar2 <- df_modificar[0, ]

  for (i in seq_len(nrow(df_modificar))) {
    cartones_fila <- cartones_cargados_mod[[i]]
    cartones_diff <- setdiff(cartones_fila, eliminar)
    id <- df_modificar$id[[i]]
    institucion <- df_modificar$institucion[[i]]

    if (length(cartones_diff) != 0) {
      incrementos <- diff(cartones_diff)
      if (all(incrementos == 1)) {
        df_aux <- data.frame(
          id = id,
          institucion = institucion,
          desde = cartones_diff[1],
          hasta = tail(cartones_diff, 1),
          stringsAsFactors = FALSE
        )
        df_modificar2 <- rbind(df_modificar2, df_aux)
      } else {
        corte <- which(incrementos != 1)
        df_aux <- data.frame(
          id = rep(id, 2),
          institucion = rep(institucion, 2),
          desde = c(cartones_diff[1], cartones_diff[corte + 1]),
          hasta = c(cartones_diff[corte], tail(cartones_diff, 1))
        )
        df_modificar2 <- rbind(df_modificar2, df_aux)
      }
    }
  }
  df <- df[!df$lgl, c("id", "institucion", "desde", "hasta")]
  df_output <- rbind(df, df_modificar2)
  df_output <- df_output[order(df_output$desde), ]
  rownames(df_output) <- NULL
  return(df_output)
}

appCatch <- function(expr) {
  tryCatch(
    expr,
    shiny.silent.error = function(cnd) {
      # This is the error class signalled by `req()`
      NULL
    },
    bingo.error = function(cnd) {
      showError(cnd$message)
    },
    error = function(cnd) {
      msg <- paste(format(Sys.time(), "%X -"), message = cnd$message)
      file <- file.path(app_file("log"), paste0(Sys.Date(), ".log"))
      write(msg, file, append = TRUE)
      print(msg)
      showError("Ocurrio un error inesperado.")
    }
  )
}

collapse <- function(x, sep = ", ", end = " y ") {
  x_length <- length(x)
  if (x_length == 1) {
    return(as.character(x))
  } else {
    return(paste0(paste0(x[1:(x_length - 1)], collapse = sep), end, x[x_length]))
  }
}

imprimir_ventas_core <- function(desde, hasta) {
  valores <- unlist(Map(`:`, desde, hasta))
  incrementos <- diff(valores)
  # desde == hasta y de longitud 1.
  if (length(incrementos) == 0) {
    return(as.character(valores))
  }
  if (all(incrementos == 1)) {
    return(paste(valores[1], "a", valores[length(valores)]))
  } else {
    corte <- which(incrementos != 1)
    desde <- c(valores[1], valores[corte + 1])
    hasta <- c(valores[corte], valores[length(valores)])

    chr <- purrr::map2_chr(desde, hasta, function(x, y) {
      if (x == y) {
        return(as.character(x))
      } else {
        return(paste(x, "a", y))
      }
    })
    return(collapse(chr))
  }
}

imprimir_ventas <- function(data, cards_n) {
  data <- data[order(data$desde), ]
  instituciones <- unique(data$institucion)
  ventas_agg <- vector("numeric", length(instituciones))
  ventas_des <- vector("character", length(instituciones))
  for (idx in seq_along(instituciones)) {
    institucion <- instituciones[idx]
    lgl <- data$institucion == institucion
    desde <- data$desde[lgl]
    hasta <- data$hasta[lgl]
    ventas_agg[idx] <- length(unique(unlist(Map(`:`, desde, hasta))))
    ventas_des[idx] <- imprimir_ventas_core(desde, hasta)
  }
  encabezado <- paste0(
    "Cantidad de cartones: ", cards_n, "\n",
    "Cantidad de cartones vendidos: ", sum(ventas_agg)
  )
  paste0(
    encabezado, "\n\n",
    paste0(
      "* ", instituciones, " (", ventas_agg, " ventas)\n",
      "  Cartones: ", ventas_des, ".",
      collapse = "\n\n"
    )
  )
}

enable_play_mode <- function(session) {
  shinydashboard::updateTabItems(session, "tabs", "board")
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
}

disable_play_mode <- function(session) {
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
  shinydashboard::updateTabItems(session, "tabs", "play")
}

app_file <- function(...) {
  system.file(..., package = "rbingo", mustWork = TRUE)
}

delete_dir <- function(dir) {
  unlink(dir, recursive = TRUE, force = TRUE)
}

DT_SPANISH <- list(
  "sProcessing" = "Procesando...",
  "sLengthMenu" = "Mostrar _MENU_ registros",
  "sZeroRecords" = "No se encontraron resultados",
  "sEmptyTable" = "Ningún dato disponible en esta tabla",
  "sInfo" = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
  "sInfoEmpty" = "Mostrando registros del 0 al 0 de un total de 0 registros",
  "sInfoFiltered" = "(filtrado de un total de _MAX_ registros)",
  "sInfoPostFix" = "",
  "sSearch" = "Buscar:",
  "sUrl" = "",
  "sInfoThousands" = ",",
  "sLoadingRecords" = "Cargando...",
  "oPaginate" = list(
    "sFirst" = "Primero",
    "sLast" = "Último",
    "sNext" = "Siguiente",
    "sPrevious" = "Anterior"
  ),
  "oAria" = list(
    "sSortAscending" = ": Activar para ordenar la columna de manera ascendente",
    "sSortDescending" = ": Activar para ordenar la columna de manera descendente"
  )
)

link_remove <- function(id) {
  glue::glue(
    "<button id='{id}' ",
    "type='button\' ",
    "class='btn btn-link link_remove' ",
    "onclick='Shiny.onInputChange(\"{id}\", Math.random())' ",
    "><i class='fa fa-times'></i></button> "
  )
}

buttonColumn <- function(len, id) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(link_remove(id))
  }
  inputs
}
