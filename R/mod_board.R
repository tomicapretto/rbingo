boardServer <- function(id, store, games, cards, parent_session) {
  moduleServer(id, function(input, output, session) {

    isolate({
      appCatch({
        rvs <- reactiveValues()
        rvs$nums <- numeric()
        rvs$partida <- NULL
        rvs$cards_playing <- NULL
        rvs$player <- NULL
        rvs$playing_time <- 0
        rvs$last <- NULL
      })
    })

    observeEvent(store$playing, {
      appCatch({
        req(store$playing)
        rvs$partida <- store$partida_info$partida

        prizes <- PRIZES[store$partida_info$prizes]
        prizes <- lapply(prizes, function(prize) do.call(new_prize, prize))
        rvs$player <- Player$new(
          games$return_game(rvs$partida),
          cards,
          prizes
        )
        # Actualizar siguiente premio apenas se crea el Player
        shinyjs::html("siguiente-premio", rvs$player$get_header())

        rvs$cards_playing <- rvs$player$sales_count
        rvs$playing_time <- 0

        # Escribe bolilla del pozo acumulado
        shinyjs::html("pozo_acumulado", store$partida_info$pozo_acumulado)

        # Temporizador que corre en JS
        shinyjs::runjs(sprintf("timer('#%s');", NS(id, "time_played")))

        # Informacion sobre la partida y los cartones
        shinyjs::html("partida_en_juego", rvs$partida)
        shinyjs::html("cartones_en_juego", rvs$cards_playing)
      })
    }, )

    # Crea div con los ganadores
    output$winners <- renderUI({
      prizes <- PRIZES[store$partida_info$prizes]
      lapply(prizes, function(x) display_winners(id, x$name))
    })


# Observers para agregar/eliminar bolillas --------------------------------
    lapply(seq(90), function(num) {
      ball_id <- paste0("num_", num)
      observeEvent(input[[ball_id]], {
        appCatch({
          req(store$playing)
          if (isTruthy(rvs$player$get_next_prize())) {
            rvs$nums <- c(rvs$nums, as.numeric(num))
            rvs$last <- "add"
            shinyjs::disable(ball_id)
            prize <- rvs$player$add_ball(as.numeric(num))
            if (!is.null(prize)) {
              write_winners(prize)
              report_winners(id, prize)
              shinyjs::html("siguiente-premio", rvs$player$get_header())
            }
          } else {
            shinyjs::hide("adelantos")
          }
        })
      })
    })

    observeEvent(input$close_modal, ignoreInit = TRUE, {
      shiny::removeModal()
      next_prize = rvs$player$get_next_prize()
      if (is(next_prize, "SmallestMatchPrize")) {
        rvs$player$check_prize_forward()
        shinyjs::delay(
          1500, {
            write_winners(next_prize)
            report_winners(id, next_prize)
            shinyjs::hide("adelantos")
          }
        )
      }
    })

    observeEvent(input$delete_last, {
      appCatch({
        req(length(rvs$nums) > 0)
        last <- rvs$nums[length(rvs$nums)]
        rvs$nums <- rvs$nums[-length(rvs$nums)]
        rvs$last <- "remove"
        shinyjs::enable(paste0("num_", last))
        prize <- rvs$player$remove_ball(as.numeric(last))
        if (!is.null(prize)) {
          shinyjs::show("adelantos")
          remove_winners(prize)
          shinyjs::html("siguiente-premio", rvs$player$get_header())
        }
      })
    })


    observe({
      appCatch({
        req(rvs$player)
        update_balls(rvs$nums)
        update_advances(rvs$player$get_advances())
        req(rvs$last == "add", !rvs$player$is_full_card_won())
        check_cumulated(rvs$nums, store$partida_info$pozo_acumulado)
      })
    })

    update_advances <- function(advances) {
      if (!is.null(advances)) {
        shinyjs::html("adelanto-label", advances$label)
        shinyjs::html("adelanto-value", advances$value)
      }
    }

    update_balls <- function(nums) {
      shinyjs::html("ball", tail(nums, 1))
      shinyjs::html("balls_played", length(nums))
      shinyjs::html("balls_drawn", paste(nums, collapse = ", "))
    }

    check_cumulated <- function(bolillas, bolillas_acumulado) {
      if (length(bolillas) == bolillas_acumulado) {
        msg <- paste(
          "Nadie ha obtenido carton lleno luego de haber sorteado",
          bolillas_acumulado,
          "bolillas."
        )
        shinypop::nx_report_info("Pozo acumulado vacante", msg)
      }
    }

    observeEvent(input$btn_stop, {
      req(rvs$player)
      appCatch({
        if (is.null(rvs$player$get_next_prize())) {
          msg <- HTML(
            paste(
              "Podra visualizar el informe de la partida en la solapa",
              "<strong>Reportes</strong>."
            )
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
                if (is.null(rvs$player$get_next_prize())) {
                  winner_cards <- get_winner_cards(winners)
                  winners2 <- mapply(get_winner, winner_cards$id, winner_cards$prize,
                    MoreArgs = list(strip = cards$strips_print),
                    SIMPLIFY = FALSE
                  )
                  game_info <- list(
                    "parameters" = list(
                      name = rvs$partida,
                      serie = games$serie(rvs$partida),
                      cards_n = rvs$cards_playing,
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
                    "sequence" = rvs$nums,
                    "winners" = winners2
                  )
                  games$finalize_game(rvs$partida, game_info)
                }
                # Luego de imprimir reporte, reinicio todo lo relacionado al juego.
                disable_play_mode(parent_session)
                purrr::walk(paste0("num_", seq(90)), shinyjs::enable)
                rvs$nums <- numeric()
                rvs$player <- NULL
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
  })
}

# Helpers ----------------------------------------------------------------------
# Me falta la lista donde emparejo al numero de carton con el nombre
# del vendedor y el nombre de la institucion. Eso lo hago para el juego
# y para imprimir los resultados, pero no lo guardo en `games` porque seria bardo
# O bueno, si no es bardo, lo guardo en `games` (pero ya significa guardar)
# un archivo local mas

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
      tags$p(id = NS(id, "balls_drawn"))
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
              tags$p(id = NS(id, "siguiente-premio"),  class = "next-prize"),
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


make_prize_name <- function(name) {
  paste0(tolower(unlist(strsplit(name, split = " "))), collapse = "-")
}

display_winners <- function(id, prize) {
  name <- make_prize_name(prize)
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

report_winners <- function(id, prize) {
  winners <- prize$winners
  ids <- vapply(winners, function(x) x$id, FUN.VALUE = numeric(1))
  sellers <- vapply(winners, function(x) x$seller, FUN.VALUE = character(1))

  ids <- paste0("N", intToUtf8(176), ids)
  sellers <- paste0("(", sellers, ")")

  f <- function(x, y) {
    tags$div(
      style = "display: flex; justify-content: space-between",
      tags$div(x, style = "font-weight: bold; font-size: 22px;"),
      tags$div(y, style = "font-size: 18px;")
    )
  }

  content <- tags$div(
    tags$div(
      tags$p(prize$name),
      style = paste(
        "text-align:center",
        "font-size: 26px",
        "color: #2980b9",
        "font-weight: bold",
        sep = ";"
      )
    ),
    tags$hr(),
    tags$p("Los ganadores son", style = paste(
      "text-align:center",
      "font-size: 22px",
      sep = ";")
    ),
    mapply(f, ids, sellers, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
  shiny::showModal(
    shiny::modalDialog(
      content,
      title = NULL,
      footer = actionButton(NS(id, "close_modal"), "Aceptar")
    )
  )
}

write_winners <- function(prize) {
  winners <- prize$winners
  name <- make_prize_name(prize$name)
  ids <- vapply(winners, function(x) x$id, FUN.VALUE = numeric(1))
  ids <- paste0("N", intToUtf8(176), " ", ids, collapse = ", ")
  ids <- substr(ids, 1, 45)
  if (length(winners) == 1) {
    count <- "1 ganador"
  } else {
    count <- paste(length(winners), "ganadores")
  }
  shinyjs::html(paste0("winners-", name), ids)
  shinyjs::html(paste0("n-winners-", name), count)
}

remove_winners <- function(prize) {
  name <- make_prize_name(prize$name)
  shinyjs::html(paste0("winners-", name), "")
  shinyjs::html(paste0("n-winners-", name), "")
}
