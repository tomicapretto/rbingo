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

        playing$line_3 <- "Terno" %in% store$partida_info$prizes
        playing$line_4 <- "Cuaterno" %in% store$partida_info$prizes
        playing$line_5 <- "Linea" %in% store$partida_info$prizes
        playing$card <- "Carton lleno" %in% store$partida_info$prizes
        playing$card_2 <- "Bingo consuelo" %in% store$partida_info$prizes
        playing$looser <- "Menor acierto" %in% store$partida_info$prizes
      })
    })

    # Crea div con los ganadores
    output$winners <- renderUI({
      lapply(
        store$partida_info$prizes,
        function(x) displayWinners(id, x)
      )
    })

    # Ocultar adelantos cuando se haya sorteado todo.
    observe({
      req(mod_store$next_prize)
      if (mod_store$next_prize == "DONE") {
        shinyjs::hide("adelantos")
      } else {
        shinyjs::show("adelantos")
      }
    })

    # Temporizador que corre en JS.
    observe({
      appCatch({
        req(store$playing)
        shinyjs::runjs(sprintf("timer('#%s');", NS(id, "time_played")))
      })
    })


    observe({
      appCatch({
        req(store$playing)
        shinyjs::html("partida_en_juego", mod_store$partida)
        shinyjs::html("cartones_en_juego", mod_store$cards_playing)
      })
    })

    updateAdvances <- function(label, value) {
      shinyjs::html("adelanto-label", label)
      shinyjs::html("adelanto-value", value)
    }

    # Titulo que indica el proximo premio
    output$next_prize <- renderUI({
      appCatch({
        req(store$playing)
        if (playing$line_3 && !finished$line_3) {
          mod_store$next_prize <- "Terno"
          updateAdvances(
            "Cartones con 2 aciertos en linea", length(matches$line_2)
          )
        } else if (playing$line_4 && !finished$line_4) {
          mod_store$next_prize <- "Cuaterno"
          updateAdvances(
            "Cartones con 3 aciertos en linea", length(matches$line_3)
          )
        } else if (playing$line_5 && !finished$line_5) {
          mod_store$next_prize <- "Linea"
          updateAdvances(
            "Cartones con 4 aciertos en linea", length(matches$line_4)
          )
        } else if (playing$card && !finished$card) {
          mod_store$next_prize <- "Carton lleno"
          updateAdvances("Cartones con 14 aciertos", length(matches$card_14))
        } else if (playing$card_2 && !finished$card_2) {
          mod_store$next_prize <- "Bingo consuelo"
          updateAdvances("Cartones con 14 aciertos", length(matches$card_14))
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
          msg <- HTML(
            "Podra visualizar el informe de la partida en la solapa <strong>Reportes</strong>."
          )
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
                      serie = games$serie(mod_store$partida),
                      cards_n = mod_store$cards_playing,
                      date_start = store$partida_info$date_start,
                      date_end = Sys.time()
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
                shinyjs::runjs("clearInterval(countdown)")
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
          req(store$playing, mod_store$next_prize != "DONE")
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
        shinyjs::html("pozo_acumulado", store$partida_info$pozo_acumulado)
      })
    })

    observe({
      appCatch({
        req(store$playing, mod_store$player)
        shinyjs::html("balls_played", length(mod_store$nums))
      })
    })

    observe({
      appCatch({
        req(store$playing, mod_store$player)
        req(length(mod_store$nums) == store$partida_info$pozo_acumulado)
        title <- "Pozo acumulado vacante!"
        msg <- paste(
          "Nadie ha obtenido carton lleno luego de haber sorteado",
          store$partida_info$pozo_acumulado,
          "bolillas."
        )
        if (finished$card) {
          shinypop::nx_report_info(title, msg)
        }
      })
    },
    priority = -10 # Run after other observers to wait for finished$card being updated
  )

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
    observeEvent(winners$line_3, {
      appCatch({
        req(store$playing, !finished$line_3, length(winners$line_3) > 0)
        report_winners(winners$line_3, "Terno!")
        finished$line_3 <- TRUE
        win_state$line_3 <- mod_store$nums
        shinyjs::html("match2", "Finalizado")
      })
    })

    observeEvent(winners$line_4, {
      appCatch({
        req(store$playing, !finished$line_4, length(winners$line_4) > 0)
        report_winners(winners$line_4, "Cuaterno!")
        finished$line_4 <- TRUE
        win_state$line_4 <- mod_store$nums
        shinyjs::html("match3", "Finalizado")
      })
    })

    # Linea
    observeEvent(winners$line_5, {
      appCatch({
        req(store$playing, !finished$line_5, length(winners$line_5) > 0)
        report_winners(winners$line_5, "Linea completa!")
        finished$line_5 <- TRUE
        win_state$line_5 <- mod_store$nums
        shinyjs::html("match4", "Finalizado")
      })
    })

    # Imprimo ganadores
    observe({
      appCatch({
        req(store$playing, playing$line_3)
        if (length(winners$line_3) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$line_3, collapse = ", ")
          text <- stringr::str_trunc(text, 45, side = "right")
          text2 <- if (length(winners$line_3) == 1) {
            paste(length(winners$line_3), "ganador")
          } else {
            paste(length(winners$line_3), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners-terno", text)
        shinyjs::html("n-winners-terno", text2)
      })
    })

    observe({
      appCatch({
        req(store$playing, playing$line_4)
        if (length(winners$line_4) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$line_4, collapse = ", ")
          text <- stringr::str_trunc(text, 45, side = "right")
          text2 <- if (length(winners$line_4) == 1) {
            paste(length(winners$line_4), "ganador")
          } else {
            paste(length(winners$line_4), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners-cuaterno", text)
        shinyjs::html("n-winners-cuaterno", text2)
      })
    })

    observe({
      appCatch({
        req(store$playing, playing$line_5)
        if (length(winners$line_5) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$line_5, collapse = ", ")
          text <- stringr::str_trunc(text, 45, side = "right")
          text2 <- if (length(winners$line_5) == 1) {
            paste(length(winners$line_5), "ganador")
          } else {
            paste(length(winners$line_5), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners-linea", text)
        shinyjs::html("n-winners-linea", text2)
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
        if (length(winners$card) > 0) {
          text <- paste0("N", intToUtf8(176), " ", winners$card, collapse = ", ")
          text <- stringr::str_trunc(text, 45, side = "right")
          text2 <- if (length(winners$card) == 1) {
            paste(length(winners$card), "ganador")
          } else {
            paste(length(winners$card), "ganadores")
          }
        } else {
          text <- text2 <- ""
        }
        shinyjs::html("winners-carton-lleno", text)
        shinyjs::html("n-winners-carton-lleno", text2)

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
        shinyjs::html("winners-bingo-consuelo", text)
        shinyjs::html("n-winners-bingo-consuelo", text2)
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
          text <- stringr::str_trunc(text, 45, side = "right")
          text2 <- if (winners_n == 1) "1 ganador" else paste(winners_n, "ganadores")
          text3 <- paste(winners$looser$hits, "aciertos")
        } else {
          text <- text2 <- text3 <- ""
        }
        shinyjs::html("winners-menor-acierto", text)
        shinyjs::html("n-winners-menor-acierto", text2)
        #shinyjs::html("winners0hits", text3)
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



board <- function(id) {
  add_cell <- function(unit, ten) {
    value <- unit + 10 * ten
    id <- NS(id, paste0("num_", value))
    actionLink(
      id,
      value,
      class = "board-cell",
      onclick = "console.log('You clicked!');",
    )
  }

  add_row <- function(ten) {
    tags$div(class = "board-row", lapply(seq(10), add_cell, ten = ten))
  }

  add_board <- function(tens) {
    lapply(tens, add_row)
  }

  tags$div(
    class = "board-container",
    tags$div(
      class = "board",
      id = NS(id, "board_div"),
      add_board(0:8)
    ),
    tags$div(
      class = "board-numbers",
      htmlOutput(NS(id, "balls_draw"))
    )
  )
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
            tags$div(
              class = "board-header",
              tags$p(
                "Jugando la partida",
                tags$span(
                  id = NS(id, "partida_en_juego"),
                  style = "font-weight: bold"
                )
              ),
              tags$p(
                "Cartones en juego",
                tags$span(
                  id = NS(id, "cartones_en_juego"),
                  style = "font-weight: bold"
                )
              )
            )
          ),
          column(
            width = 4,
            tags$div(
              htmlOutput(NS(id, "next_prize")),
              style = "font-size: 20px; text-align: center; font-weight:bold"
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            board(id)
          ),
          column(
            width = 4,
            tags$div(
              tags$p(
                "Bolillas jugadas",
                tags$span(id = NS(id, "balls_played"), style = "font-weight: bold"),
                class = "board-adelanto"
              ),
              tags$p(
                "Tiempo de juego",
                tags$span(id = NS(id, "time_played"), style = "font-weight: bold"),
                class = "board-adelanto"
              ),
              tags$p(
                "Pozo acumulado en bolilla",
                tags$span(id = NS(id, "pozo_acumulado"), style = "font-weight: bold"),
                class = "board-adelanto"
              )
            ),
            horizontal_line(),
            tags$div(
              id = NS(id, "adelantos"),
              tags$div("Adelantos", class = "board-info-header"),
              tags$div(
                tags$p(
                  tags$span(id = NS(id, "adelanto-label")),
                  tags$span(id = NS(id, "adelanto-value"), class = "board-counter"),
                  class = "board-adelanto"
                )
              ),
              horizontal_line()
            ),
            tags$div("Cartones ganadores", class = "board-info-header"),
            uiOutput(NS(id, "winners")),
            horizontal_line(),
            actionButton(
              NS(id, "delete_last"), "Eliminar ultimo", class = "board-btn"
            ),
            horizontal_line(),
            actionButton(
              NS(id, "btn_stop"), "Finalizar partida", class = "board-btn"
            )
          )
        )
      )
    )
  )
}


displayWinners <- function(id, prize) {
  name <- paste0(tolower(unlist(strsplit(prize, split = " "))), collapse = "-")
  tags$div(
    class = "board-winners",
    tags$div(
      tags$p(
        prize,
        tags$span(
          id = NS(id, paste0("winners-", name)),
          class = "board-winner"
        )
      ),
    ),
    tags$div(
      tags$p(
        tags$span(
          id = NS(id, paste0("n-winners-", name)),
          class = "board-prize"
        ),
        style = "text-align: right;"
      )
    )
  )
}


