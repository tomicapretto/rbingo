boardServer <- function(id, store, games, cards, parent_session) {
  moduleServer(id, function(input, output, session) {
    isolate({
      appCatch({
        mod_store <- reactiveValues()
        mod_store$nums <- numeric()
        mod_store$partida <- NULL
        mod_store$cards_playing <- NULL
        mod_store$player <- NULL
        mod_store$playing_time <- 0
        mod_store$last <- NULL

        # Almacena las coincidencias
        matches <- reactiveValues(
          line_3 = numeric(0), line_4 = numeric(0),
          line_5 = numeric(0), card_13 = numeric(0),
          card_14 = numeric(0), card_15 = numeric(0)
        )

        # Booleanos que indican si ya se jugaron todos los sorteos para
        # cada uno de los premios
        finished <- reactiveValues(
          line_3 = FALSE, line_4 = FALSE, line_5 = FALSE,
          card = FALSE, card_2 = FALSE, looser = FALSE
        )

        # Booleanos que indican si se esta jugando cada premio
        playing <- reactiveValues(
          line_3 = FALSE, line_4 = FALSE, line_5 = FALSE,
          card = FALSE, card_2 = FALSE, looser = FALSE
        )

        # Listas/vectores con los cartones (y filas) ganadoras para cada sorteo
        # y premio
        winners <- reactiveValues(
          line_3 = numeric(), line_4 = numeric(),
          line_5 = numeric(), card = numeric(),
          card_2 = numeric(),
          looser = list("cards" = numeric(), "hits" = numeric())
        )

        # El estado en que se encontraba `mod_store$nums` al momento de que
        # finalice cada uno de los premios
        win_state <- reactiveValues(
          line_3 = NULL, line_4 = NULL, line_5 = NULL,
          card = NULL, card_2 = NULL, looser = NULL
        )
      })
    })

    observeEvent(store$playing, {
      appCatch({
        req(store$playing)

        mod_store$partida <- store$partida_info$partida
        mod_store$player <- Player$new(games$return_game(mod_store$partida), cards)
        mod_store$cards_playing <- mod_store$player$sales_count
        mod_store$playing_time <- 0

        # Deshabilito premios y anticipos si no se juegan
        if (!"Terno" %in% store$partida_info$prizes) {
          shinyjs::hide("wrow_line3")
          playing$line_3 <- FALSE
        } else {
          shinyjs::show("wrow_line3")
          playing$line_3 <- TRUE
        }

        if (!"Cuaterno" %in% store$partida_info$prizes) {
          shinyjs::hide("wrow_line4")
          playing$line_4 <- FALSE
        } else {
          shinyjs::show("wrow_line4")
          playing$line_4 <- TRUE
        }

        if (!"Linea" %in% store$partida_info$prizes) {
          shinyjs::hide("wrow_line5")
          playing$line_5 <- FALSE
        } else {
          shinyjs::show("wrow_line5")
          playing$line_5 <- TRUE
        }

        # NOTA: Siempre se juega al carton
        playing$card <- TRUE

        if (!"Bingo consuelo" %in% store$partida_info$prizes) {
          playing$card_2 <- FALSE
          shinyjs::hide("wrow_card2")
        } else {
          playing$card_2 <- TRUE
          shinyjs::show("wrow_card2")
        }

        if (!"Menor acierto" %in% store$partida_info$prizes) {
          shinyjs::hide("wrow_looser")
          playing$looser <- FALSE
        } else {
          shinyjs::show("wrow_looser")
          playing$looser <- TRUE
        }
      })
    })

    observe({
      appCatch({
        req(store$playing)
        invalidateLater(1000, session)
        isolate({
          mod_store$playing_time <- mod_store$playing_time + 1
          min <- mod_store$playing_time %/% 60
          sec <- mod_store$playing_time %% 60
          text <- paste0(min, " min. ", sec, " seg.")
          shinyjs::html("time_played", paste("Tiempo de juego: ", text))
        })
      })
    })

    output$board_header <- renderUI({
      appCatch({
        req(store$playing)
        req(mod_store$cards_playing)
        HTML(paste0(
          "Jugando la partida '", mod_store$partida, "'", "<br/>",
          "Cartones en juego: ", mod_store$cards_playing
        ))
      })
    })

    # Titulo que indica el proximo premio
    output$next_prize <- renderUI({
      appCatch({
        req(store$playing)
        if (playing$line_3 && !finished$line_3) {
          mod_store$next_prize <- "Terno"
          shinyjs::show("irow_match2")
        } else if (playing$line_4 && !finished$line_4) {
          mod_store$next_prize <- "Cuaterno"
          shinyjs::show("irow_match3")
        } else if (playing$line_5 && !finished$line_5) {
          mod_store$next_prize <- "Linea"
          shinyjs::show("irow_match4")
        } else if (playing$card && !finished$card) {
          mod_store$next_prize <- "Carton lleno"
          shinyjs::show("irow_match13")
          shinyjs::show("irow_match14")
        } else if (playing$card_2 && !finished$card_2) {
          mod_store$next_prize <- "Bingo consuelo"
          shinyjs::show("irow_match13")
          shinyjs::show("irow_match14")
        } else if (playing$looser && !finished$looser) {
          mod_store$next_prize <- "Menor acierto"
        } else {
          mod_store$next_prize <- "DONE"
        }
        content <- if (mod_store$next_prize == "DONE") {
          tags$div("Juego finalizado", class = "next-prize")
        } else {
          tags$div(
            "Proximo premio",
            tags$br(),
            mod_store$next_prize,
            class = "next-prize"
          )
        }
        content
      })
    })

    observeEvent(input$btn_stop, {
      req(store$playing)
      appCatch({
        if (mod_store$next_prize == "DONE") {
          msg <- "Podra visualizar el informe de la partida en la solapa 'Informes'."
        } else {
          msg <- paste(
            "Esta partida aun cuenta con sorteos por finalizar.",
            "Si decide confirmar, la partida continuara disponible para",
            "ser jugada en el futuro."
          )
        }
        shinypop::nx_confirm(
          inputId = "confirm",
          title = "Finalizar partida?",
          message = msg,
          button_ok = "Confirmar",
          button_cancel = "Cancelar"
        )
        observeEvent(input$confirm,
          {
            appCatch({
              if (input$confirm) {
                if (mod_store$next_prize == "DONE") {
                  winner_cards <- get_winner_cards(winners)
                  winners2 <- mapply(get_winner, winner_cards$id, winner_cards$prize,
                    MoreArgs = list(strip = cards$strips_print),
                    SIMPLIFY = FALSE
                  )
                  game_info <- list(
                    "parameters" = list(
                      name = mod_store$partida,
                      date = format(games$date(mod_store$partida), "%d-%m-%y"),
                      serie = games$serie(mod_store$partida),
                      cards_n = mod_store$cards_playing
                    ),
                    "results" = list(
                      line_3 = if (!is.null(win_state$line_3)) length(win_state$line_3) else "",
                      line_4 = if (!is.null(win_state$line_4)) length(win_state$line_4) else "",
                      line_5 = if (!is.null(win_state$line_5)) length(win_state$line_5) else "",
                      card = if (!is.null(win_state$card)) length(win_state$card) else "",
                      card_2 = if (!is.null(win_state$card_2)) length(win_state$card_2) else ""
                    ),
                    "sequence" = mod_store$nums,
                    "winners" = winners2
                  )
                  games$finalize_game(mod_store$partida, game_info)
                }
                # Luego de imprimir reporte, reinicio todo lo relacionado al juego.
                disable_play_mode(parent_session)
                reset_rvs(matches, finished, playing, winners, win_state)
                purrr::walk(paste0("num_", seq(90)), shinyjs::enable)
                mod_store$nums <- numeric()
                store$playing <- FALSE
                shinyjs::js$hideHeader("")
                shinyjs::html("ball", "")
              }
            })
          },
          once = TRUE,
          ignoreInit = TRUE
        )
      })
    })

    # Number observers ---------------------------------------------------------
    lapply(seq(90), function(num) {
      id <- paste0("num_", num)
      observeEvent(input[[id]], {
        appCatch({
          req(store$playing)
          mod_store$player$add_ball(as.numeric(num))
          mod_store$nums <- c(mod_store$nums, as.numeric(num))
          mod_store$last <- "add"
          shinyjs::disable(id)
        })
      })
    })

    observeEvent(input$delete_last,
      {
        appCatch({
          req(store$playing, length(mod_store$nums) > 0)
          # Elimino el ultimo numero y activo el boton correspondiente
          last <- mod_store$nums[length(mod_store$nums)]
          mod_store$nums <- mod_store$nums[-length(mod_store$nums)]
          mod_store$player$remove_ball(as.numeric(last))
          mod_store$last <- "remove"
          shinyjs::enable(paste0("num_", last))

          # Devuelvo booleanos a FALSE si las bolillas sorteadas no contienen
          # a alguna de las bolillas determinaron que el premio se gane
          reset_line(mod_store, win_state, finished, winners)
          reset_card(mod_store, win_state, finished, winners)
          reset_looser(mod_store, win_state, finished, winners)
        })
      },
      priority = 1
    )

    observe({
      appCatch({
        req(store$playing, mod_store$player)
        text <- paste("Pozo acumulado en bolilla:", store$partida_info$pozo_acumulado)
        shinyjs::html("pozo_acumulado", text)
      })
    })

    observe({
      appCatch({
        req(store$playing, mod_store$player)
        text <- paste("Bolillas jugadas:", length(mod_store$nums))
        shinyjs::html("balls_played", text)
      })
    })

    observeEvent(mod_store$nums, {
      appCatch({
        req(store$playing, mod_store$player)
        shinyjs::html("ball", tail(mod_store$nums, 1))
        play_line(mod_store, matches, playing, finished, winners)
        play_card(mod_store, matches, playing, finished, winners)
      })
    })
    # El looser se juega inmediatamente despues de que se gane el ultimo carton.
    # Es decir, no es necesario tirar una nueva bolilla.
    observe({
      appCatch({
        if (playing$card && playing$card_2) req(finished$card, finished$card_2)
        if (playing$card) req(finished$card)
        req(playing$looser, !finished$looser, mod_store$last == "add")
        play_looser(mod_store, finished, winners, win_state)
      })
    })

    # Premios en la linea ------------------------------------------------------
    # Terno
    observe({
      appCatch({
        req(store$playing, playing$line_3, !finished$line_3)
        shinyjs::html("match2", length(matches$line_2))
      })
    })

    observeEvent(winners$line_3, {
      appCatch({
        req(store$playing, !finished$line_3, length(winners$line_3) > 0)
        report_winners(winners$line_3, "Terno!")
        finished$line_3 <- TRUE
        win_state$line_3 <- mod_store$nums
        shinyjs::hide("irow_match2")
        shinyjs::html("match2", "Finalizado")
      })
    })

    # Cuaterno
    observe({
      appCatch({
        req(store$playing, playing$line_4, !finished$line_4)
        shinyjs::html("match3", length(matches$line_3))
      })
    })

    observeEvent(winners$line_4, {
      appCatch({
        req(store$playing, !finished$line_4, length(winners$line_4) > 0)
        report_winners(winners$line_4, "Cuaterno!")
        finished$line_4 <- TRUE
        win_state$line_4 <- mod_store$nums
        shinyjs::hide("irow_match3")
        shinyjs::html("match3", "Finalizado")
      })
    })

    # Linea
    observe({
      appCatch({
        req(store$playing, playing$line_5, !finished$line_5)
        shinyjs::html("match4", length(matches$line_4))
      })
    })

    observeEvent(winners$line_5, {
      appCatch({
        req(store$playing, !finished$line_5, length(winners$line_5) > 0)
        report_winners(winners$line_5, "Linea completa!")
        finished$line_5 <- TRUE
        win_state$line_5 <- mod_store$nums
        shinyjs::hide("irow_match4")
        shinyjs::html("match4", "Finalizado")
      })
    })

    # Imprimo ganadores
    observe({
      appCatch({
        req(store$playing, playing$line_3)
        if (length(winners$line_3) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$line_3, collapse = ", ")
          text <- stringr::str_trunc(text, 34, side = "right")
          text2 <- if (length(winners$line_3) == 1) {
            paste(length(winners$line_3), "ganador")
          } else {
            paste(length(winners$line_3), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners3", text)
        shinyjs::html("winners3n", text2)
      })
    })

    observe({
      appCatch({
        req(store$playing, playing$line_4)
        if (length(winners$line_4) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$line_4, collapse = ", ")
          text <- stringr::str_trunc(text, 34, side = "right")
          text2 <- if (length(winners$line_4) == 1) {
            paste(length(winners$line_4), "ganador")
          } else {
            paste(length(winners$line_4), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners4", text)
        shinyjs::html("winners4n", text2)
      })
    })

    observe({
      appCatch({
        req(store$playing, playing$line_5)
        if (length(winners$line_5) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$line_5, collapse = ", ")
          text <- stringr::str_trunc(text, 34, side = "right")
          text2 <- if (length(winners$line_5) == 1) {
            paste(length(winners$line_5), "ganador")
          } else {
            paste(length(winners$line_5), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners5", text)
        shinyjs::html("winners5n", text2)
      })
    })

    # Premios al carton --------------------------------------------------------
    # Carton lleno
    observeEvent(winners$card, {
      appCatch({
        req(store$playing, !finished$card, length(winners$card) > 0)
        text <- "Carton lleno!"
        if (length(mod_store$nums) <= store$partida_info$pozo_acumulado) {
          text <- "Carton lleno y pozo acumulado!"
        }
        report_winners(winners$card, text)
        finished$card <- TRUE
        win_state$card <- mod_store$nums
      })
    })

    observeEvent(winners$card_2, {
      appCatch({
        req(store$playing, !finished$card_2, length(winners$card_2) > 0)
        report_winners(winners$card_2, "Premio consuelo!")
        finished$card_2 <- TRUE
        win_state$card_2 <- mod_store$nums
      })
    })

    observe({
      appCatch({
        req(store$playing)
        if ((playing$card || playing$card_2) && (!(finished$card && finished$card_2))) {
          shinyjs::html("match13", length(matches$card_13))
          shinyjs::html("match14", length(matches$card_14))
        } else {
          shinyjs::hide("irow_match13")
          shinyjs::hide("irow_match14")
          shinyjs::html("match13", "Finalizado")
          shinyjs::html("match14", "Finalizado")
        }
      })
    })

    observe({
      appCatch({
        req(store$playing)
        if (length(winners$card) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$card, collapse = ", ")
          text <- stringr::str_trunc(text, 32, side = "right")
          text2 <- if (length(winners$card) == 1) {
            paste(length(winners$card), "ganador")
          } else {
            paste(length(winners$card), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners15", text)
        shinyjs::html("winners15n", text2)

        if (length(winners$card_2) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$card_2, collapse = ", ")
          text <- stringr::str_trunc(text, 32, side = "right")
          text2 <- if (length(winners$card_2) == 1) {
            paste(length(winners$card_2), "ganador")
          } else {
            paste(length(winners$card_2), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners16", text)
        shinyjs::html("winners16n", text2)
      })
    })

    # Menor acierto
    observeEvent(winners$looser, {
      appCatch({
        req(store$playing)
        winners_n <- length(winners$looser$cards)
        if (winners_n > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$looser$cards,
            collapse = ", "
          )
          text <- stringr::str_trunc(text, 32, side = "right")
          text2 <- if (winners_n == 1) "1 ganador" else paste(winners_n, "ganadores")
          text3 <- paste(winners$looser$hits, "aciertos")
        } else {
          text <- text2 <- text3 <- ""
        }
        shinyjs::html("winners0", text)
        shinyjs::html("winners0n", text2)
        shinyjs::html("winners0hits", text3)
      })
    })

    output$balls_draw <- renderUI({
      appCatch({
        HTML(paste(mod_store$nums, collapse = ", "))
      })
    })
  })
}

# Helpers ----------------------------------------------------------------------
reset_card <- function(store, win_state, finished, winners) {
  if (!is.null(win_state$card)) {
    if (any(!(win_state$card %in% store$nums))) {
      finished$card <- FALSE
      win_state$card <- NULL
      winners$card <- numeric()
    }
  }

  if (!is.null(win_state$card_2)) {
    if (any(!(win_state$card_2 %in% store$nums))) {
      finished$card_2 <- FALSE
      win_state$card_2 <- NULL
      winners$card_2 <- numeric()
    }
  }
}

reset_line <- function(store, win_state, finished, winners) {
  if (!is.null(win_state$line_3)) {
    if (any(!(win_state$line_3 %in% store$nums))) {
      finished$line_3 <- FALSE
      win_state$line_3 <- NULL
      winners$line_3 <- numeric()
    }
  }
  if (!is.null(win_state$line_4)) {
    if (any(!(win_state$line_4 %in% store$nums))) {
      finished$line_4 <- FALSE
      win_state$line_4 <- NULL
      winners$line_4 <- numeric()
    }
  }
  if (!is.null(win_state$line_5)) {
    if (any(!(win_state$line_5 %in% store$nums))) {
      finished$line_5 <- FALSE
      win_state$line_5 <- NULL
      winners$line_5 <- numeric()
    }
  }
}

reset_looser <- function(store, win_state, finished, winners) {
  if (!is.null(win_state$looser)) {
    if (any(!(win_state$looser %in% store$nums))) {
      finished$looser <- FALSE
      win_state$looser <- NULL
      winners$looser <- list("cards" = numeric(), "hits" = numeric())
      shinyjs::html("match0", "")
    }
  }
}


play_line <- function(store, matches, playing, finished, winners) {
  # Pass by reference
  player <- store$player

  # Match 2
  if (playing$line_3 && !finished$line_3) {
    matches$line_2 <- player$row_matches(2)
  }

  # Match 3
  if ((playing$line_3 && !finished$line_3) || (playing$line_4 && !finished$line_4)) {
    matches$line_3 <- player$row_matches(3)
    if (playing$line_3 && !finished$line_3) {
      winners$line_3 <- matches$line_3
    }
  }

  # Match 4
  if ((playing$line_4 && !finished$line_4) || (playing$line_5 && !finished$line_5)) {
    matches$line_4 <- player$row_matches(4)
    if (playing$line_4 && !finished$line_4) {
      winners$line_4 <- matches$line_4
    }
  }

  # Match 5
  # Solo cuando el cuaterno no este en juego o ya se haya ganado
  if ((playing$line_5 && !finished$line_5) && (finished$line_4 || !playing$line_4)) {
    matches$line_5 <- player$row_matches(5)
    if (playing$line_5 && !finished$line_5) {
      winners$line_5 <- matches$line_5
    }
  }
}

play_card <- function(store, matches, playing, finished, winners) {
  # Pass by reference
  player <- store$player

  # Match 13
  # Solo cuando el carton esta en juego y no se haya ganado
  if ((playing$card || playing$card_2) && (!(finished$card && finished$card_2))) {
    matches$card_13 <- player$card_matches(13, "ge")
  } else {
    matches$card_13 <- numeric()
  }
  # Match 14
  # Carton en juego, no ganado, y hay matches 13.
  if (playing$card && (!(finished$card && finished$card_2)) && length(matches$card_13) > 0) {
    matches$card_14 <- player$card_matches(14, "ge")
    matches$card_13 <- setdiff(matches$card_13, matches$card_14)
  } else {
    matches$card_14 <- numeric()
  }

  # Match 15
  # Carton en juego, no ganado, y hay matches 14.
  if ((playing$card || playing$card_2) && (!(finished$card && finished$card_2)) && length(matches$card_14) > 0) {
    matches$card_15 <- player$card_matches(15, "eq")
    matches$card_14 <- setdiff(matches$card_14, matches$card_15)
    if (playing$card && !finished$card) {
      winners$card <- matches$card_15
    }
    if ((playing$card_2 && !finished$card_2) && finished$card) {
      winners$card_2 <- setdiff(matches$card_15, winners$card)
    }
  } else {
    matches$card_15 <- numeric()
  }
}


play_looser <- function(store, finished, winners, win_state) {
  found <- FALSE
  match_n <- 0
  while (!found) {
    matches <- store$player$card_matches(match_n, "eq")
    if (length(matches) != 0) {
      found <- TRUE
      winners$looser$cards <- matches
      winners$looser$hits <- match_n
      finished$looser <- TRUE
      win_state$looser <- store$nums
    } else {
      match_n <- match_n + 1
    }
  }
}

# Me falta la lista donde emparejo al numero de carton con el nombre
# del vendedor y el nombre de la institucion. Eso lo hago para el juego
# y para imprimir los resultados, pero no lo guardo en `games` porque seria bardo
# O bueno, si no es bardo, lo guardo en `games` (pero ya significa guardar)
# un archivo local mas

report_winners <- function(winners, title) {
  cards <- paste("N", intToUtf8(176), " ", winners, collapse = "<br/>")
  text <- if (length(winners) > 1) {
    paste0("Los cartones ganadores son<br/>", cards)
  } else {
    paste0("El carton ganador es<br/>", cards)
  }
  if (nchar(text) > 600) {
    text <- stringr::str_trunc(text, 600, "right")
    text <- paste0(text, "<br/>Informacion completa en el reporte")
  }
  shinypop::nx_report_info(title, HTML(text))
}


reset_rvs <- function(matches, finished, playing, winners, win_state) {
  matches$line_2 <- numeric(0)
  matches$line_3 <- numeric(0)
  matches$line_4 <- numeric(0)
  matches$line_5 <- numeric(0)
  matches$card_13 <- numeric(0)
  matches$card_14 <- numeric(0)
  matches$card_15 <- numeric(0)

  finished$line_3 <- FALSE
  finished$line_4 <- FALSE
  finished$line_5 <- FALSE
  finished$card <- FALSE
  finished$card_2 <- FALSE
  finished$looser <- FALSE

  playing$line_3 <- FALSE
  playing$line_4 <- FALSE
  playing$line_5 <- FALSE
  playing$card <- FALSE
  finished$card_2 <- FALSE
  playing$looser <- FALSE

  winners$line_3 <- numeric()
  winners$line_4 <- numeric()
  winners$line_5 <- numeric()
  winners$card <- numeric()
  winners$card_2 <- numeric()
  winners$looser <- list("cards" = numeric(), "hits" = numeric())

  win_state$line_3 <- NULL
  win_state$line_4 <- NULL
  win_state$line_5 <- NULL
  win_state$card <- NULL
  win_state$card_2 <- NULL
  win_state$looser <- NULL
}

get_winner <- function(card_id, prize, strips) {
  strip_id <- ((card_id - 1) %/% 6) + 1
  idx <- ((card_id - 1) %% 6) + 1
  strip <- strips[[strip_id]]
  card_numbers <- as.vector(strip[(3 * idx - 2):(3 * idx), ])
  list(
    numbers = card_numbers,
    card = card_id,
    name = "",
    city = "",
    prize = prize
  )
}

get_winner_cards <- function(winners) {
  ids <- numeric()
  prizes <- character()

  if (length(winners$line_3) > 0) {
    ids <- c(ids, winners$line_3)
    prizes <- c(prizes, rep("TERNO", length(winners$line_3)))
  }

  if (length(winners$line_4) > 0) {
    ids <- c(ids, winners$line_4)
    prizes <- c(prizes, rep("CUATERNO", length(winners$line_4)))
  }

  if (length(winners$line_5) > 0) {
    ids <- c(ids, winners$line_5)
    prizes <- c(prizes, rep("LINEA", length(winners$line_5)))
  }

  if (length(winners$card) > 0) {
    ids <- c(ids, winners$card)
    prizes <- c(prizes, rep("BINGO", length(winners$card)))
  }

  if (length(winners$card_2) > 0) {
    ids <- c(ids, winners$card_2)
    prizes <- c(prizes, rep("BINGO CONSUELO", length(winners$card_2)))
  }

  if (length(winners$looser$cards) > 0) {
    ids <- c(ids, winners$looser$cards)
    prizes <- c(prizes, rep("MENOR ACIERTO", length(winners$looser$cards)))
  }
  return(list("id" = ids, "prize" = prizes))
}


boardUI <- function(id) {
  tagList(
    bingoBall(NS(id, "ball")),
    fluidRow(
      style = "margin-top:-20px",
      shinydashboard::box(
        height = "auto",
        width = 12,
        title = NULL,
        fluidRow(
          column(
            width = 8,
            tags$span(htmlOutput(NS(id, "board_header")),
              style = "font-size: 20px; text-align: center"
            )
          ),
          column(
            width = 4,
            tags$span(htmlOutput(NS(id, "next_prize")),
              style = "font-size: 20px; text-align: center; font-weight:bold"
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            tags$div(
              style = "text-align:center;margin-top:-10px",
              id = NS(id, "board_div"),
              tags$br(),
              lapply(0:8, function(dec) {
                tagList(
                  tags$div(
                    class = "board-row",
                    lapply(seq(10), function(num) {
                      value <- num + 10 * dec
                      id <- NS(id, paste0("num_", value))
                      actionLink(inputId = id, label = value, class = "board-cell")
                    })
                  )
                )
              }),
              tags$br(),
              fluidRow(
                column(
                  width = 12,
                  tags$span(htmlOutput(NS(id, "balls_draw")), style = "font-size:28px;")
                )
              )
            )
          ),
          column(
            width = 4,
            tags$div(
              tagList(
                tags$p(
                  id = NS(id, "balls_played"), "Bolillas jugadas: 0",
                  style = "font-size:20px"
                ),
                tags$p(
                  id = NS(id, "time_played"), "Tiempo de juego: 0 min. 0 seg.",
                  style = "font-size:20px"
                ),
                tags$p(
                  id = NS(id, "pozo_acumulado"), "Pozo acumulado en bolilla:",
                  style = "font-size:20px"
                )
              ),
              style = "margin-top:10px"
            ),
            tags$hr(style = "border-top: 3px solid #d2d6de; border-radius:10px"),
            tags$p("Adelantos",
              style = "font-size:20px;text-align:center;font-weight:bold"
            ),
            shinyjs::hidden(
              tags$p(
                id = NS(id, "irow_match2"),
                "Cartones con 2 aciertos en linea",
                tags$span(id = NS(id, "match2"), class = "board-counter"),
                style = "font-size:20px"
              )
            ),
            shinyjs::hidden(
              tags$p(
                id = NS(id, "irow_match3"),
                "Cartones con 3 aciertos en linea",
                tags$span(id = NS(id, "match3"), class = "board-counter"),
                style = "font-size:20px"
              )
            ),
            shinyjs::hidden(
              tags$p(
                id = NS(id, "irow_match4"),
                "Cartones con 4 aciertos en linea",
                tags$span(id = NS(id, "match4"), class = "board-counter"),
                style = "font-size:20px"
              )
            ),
            shinyjs::hidden(
              tags$p(
                id = NS(id, "irow_match13"),
                "Cartones con 13 aciertos",
                tags$span(id = NS(id, "match13"), class = "board-counter"),
                style = "font-size:20px"
              )
            ),
            shinyjs::hidden(
              tags$p(
                id = NS(id, "irow_match14"),
                "Cartones con 14 aciertos",
                tags$span(id = NS(id, "match14"), class = "board-counter"),
                style = "font-size:20px"
              )
            ),
            tags$hr(style = "border-top: 3px solid #d2d6de; border-radius:10px "),
            tags$p("Cartones ganadores",
              style = "font-size:20px;text-align:center;font-weight:bold"
            ),
            tags$div(
              fluidRow(
                id = NS(id, "wrow_line3"),
                column(
                  width = 8,
                  tags$p(
                    "Terno",
                    tags$span(id = NS(id, "winners3"), class = "board-winner")
                  )
                ),
                column(
                  width = 4,
                  tags$p(
                    tags$span(id = NS(id, "winners3n"), class = "board-prize"),
                    style = "text-align:right;"
                  )
                )
              ),
              fluidRow(
                id = NS(id, "wrow_line4"),
                column(
                  width = 8,
                  tags$p(
                    "Cuaterno",
                    tags$span(id = NS(id, "winners4"), class = "board-winner")
                  )
                ),
                column(
                  width = 4,
                  tags$p(
                    tags$span(id = NS(id, "winners4n"), class = "board-prize"),
                    style = "text-align:right;"
                  )
                )
              ),
              fluidRow(
                id = NS(id, "wrow_line5"),
                column(
                  width = 8,
                  tags$p(
                    "Linea",
                    tags$span(id = NS(id, "winners5"), class = "board-winner")
                  )
                ),
                column(
                  width = 4,
                  tags$p(
                    tags$span(id = NS(id, "winners5n"), class = "board-prize"),
                    style = "text-align:right;"
                  )
                )
              ),
              fluidRow(
                id = NS(id, "wrow_card"),
                column(
                  width = 8,
                  tags$p(
                    "Carton lleno",
                    tags$span(id = NS(id, "winners15"), class = "board-winner")
                  )
                ),
                column(
                  width = 4,
                  tags$p(
                    tags$span(id = NS(id, "winners15n"), class = "board-prize"),
                    style = "text-align:right;"
                  )
                )
              ),
              fluidRow(
                id = NS(id, "wrow_card2"),
                column(
                  width = 8,
                  tags$p(
                    "Bingo consuelo",
                    tags$span(id = NS(id, "winners16"), class = "board-winner")
                  )
                ),
                column(
                  width = 4,
                  tags$p(
                    tags$span(id = NS(id, "winners16n"), class = "board-prize"),
                    style = "text-align:right;"
                  )
                )
              ),
              fluidRow(
                id = NS(id, "wrow_looser"),
                column(
                  width = 8,
                  tags$p(
                    "Menor acierto",
                    tags$span(id = NS(id, "winners0"), class = "board-winner")
                  ),
                  tags$span(id = NS(id, "winners0hits"), style = "color:#c0392b;")
                ),
                column(
                  width = 4,
                  tags$p(
                    tags$span(id = NS(id, "winners0n"), class = "board-prize"),
                    style = "text-align:right;"
                  )
                )
              ),
              style = "font-size:20px"
            ),
            tags$hr(style = "border-top: 3px solid #d2d6de; border-radius:10px "),
            actionButton(NS(id, "delete_last"), "Eliminar ultimo",
              width = "100%",
              style = "height:45px;font-size:22px"
            ),
            tags$hr(style = "border-top: 3px solid #d2d6de; border-radius:10px "),
            actionButton(NS(id, "btn_stop"), "Finalizar partida",
              width = "100%",
              style = "height:45px;font-size:22px"
            )
          )
        )
      )
    )
  )
}