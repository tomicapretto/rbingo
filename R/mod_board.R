boardServer <- function(id, store, games, cards, parent_session) {
  moduleServer(id, function(input, output, session) {

    isolate({
      rvs <- reactiveValues()
      rvs$nums <- numeric()
      rvs$player <- NULL
      rvs$game <- NULL
      rvs$last <- NULL
    })

    # Este dummy es una especie de hack para que los valores se actualizen
    # DESPUES de que se crean los html donde van los mensajes de los premios.
    output$winners <- renderUI({
      req(store$playing)
      prizes <- PRIZES[store$partida_info$prizes]
      tagList(
        shinyjs::hidden(numericInput(NS(id, "dummy"), "no-label", value = 0)),
        lapply(prizes, function(x) display_winners(id, x$name))
      )
    })

    observeEvent(c(input$dummy, store$playing), {
      appCatch({
        req(store$playing, input$dummy)
        prizes <- PRIZES[store$partida_info$prizes]
        prizes <- lapply(prizes, function(prize) {
          if (prize$name == "Carton lleno") {
            prize$cumulated = store$partida_info$pozo_acumulado
          }
          do.call(new_prize, prize)
        })
        rvs$game <- games$return_game(store$partida_info$partida)
        rvs$player <- Player$new(rvs$game, cards, prizes)
        rvs$nums <- isolate(rvs$game$sequence)

        if (length(rvs$nums)) {
          showInfo("Recuperando bolillas sorteadas...")
          for (num in rvs$nums) play_forward(num, FALSE)
        }

        # Actualizar siguiente premio apenas se crea el Player
        shinyjs::html("siguiente-premio", rvs$player$get_header())

        # Escribe bolilla del pozo acumulado
        shinyjs::html("pozo_acumulado", store$partida_info$pozo_acumulado)

        # Temporizador que corre en JS
        shinyjs::runjs(sprintf("timer('#%s');", NS(id, "time_played")))

        # Informacion sobre la partida y los cartones
        shinyjs::html("partida_en_juego", store$partida_info$partida)
        shinyjs::html("cartones_en_juego", rvs$player$sales_count)
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    # Observers para agregar/eliminar bolillas ----------------------------
    play_forward <- function(num, add_winners_modal = TRUE) {
      appCatch({
        ball_id <- paste0("num_", num)
        rvs$last <- "add"
        shinyjs::disable(ball_id)
        prize <- rvs$player$add_ball(as.numeric(num))
        if (!is.null(prize)) {
          write_winners(prize)
          shinyjs::html("siguiente-premio", rvs$player$get_header())
          if (add_winners_modal) {
            report_winners(id, prize)
          }
        }
      })
    }
    observeEvent(rvs$nums, {
      # Para el backup... y no perder el sorteo!
      isolate({
        rvs$game$sequence <- rvs$nums
        rvs$game$save_sequence()
      })
    }, ignoreInit = TRUE)

    lapply(seq(90), function(num) {
      ball_id <- paste0("num_", num)
      observeEvent(input[[ball_id]], {
        appCatch({
          req(store$playing)
          if (isTruthy(rvs$player$get_next_prize())) {
            rvs$nums <- c(rvs$nums, as.numeric(num))
            play_forward(num)
          } else {
            shinyjs::hide("adelantos")
          }
        })
      }, ignoreInit = TRUE)
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
          remove_winners(prize)
          shinyjs::show("adelantos")
          shinyjs::html("siguiente-premio", rvs$player$get_header())

          if (is(prize, "SmallestMatchPrize")) {
            prize2 <- rvs$player$check_prize_backward()
            if (!is.null(prize2)) {
              remove_winners(prize2)
              shinyjs::html("siguiente-premio", rvs$player$get_header())
            }
          }
        }
      })
    })

    observeEvent(input$close_modal, {
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
    }, ignoreInit = TRUE)


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
          "Nadie ganó el carton lleno luego de sortear",
          bolillas_acumulado,
          "bolillas."
        )
        content <- tags$div(
          tags$div("Pozo acumulado vacante", class = "modal-title"),
          tags$hr(),
          tags$p(msg, class = "modal-title-lower")
        )
        shiny::showModal(
          shiny::modalDialog(
            content,
            title = NULL,
            footer = modalButton("Aceptar"),
          )
        )
      }
    }


    observeEvent(input$btn_stop, {
      req(rvs$player)
      appCatch({
        if (is.null(rvs$player$get_next_prize())) {
          msg <- tags$p(
              "Podra visualizar el informe de la partida en la solapa",
              tags$strong("Reportes")
            )
        } else {
          msg <- tags$p(
            paste(
              "Esta partida aun cuenta con sorteos por finalizar.",
              "Si decide confirmar, la partida continuara disponible para",
              "ser jugada en el futuro."
            )
          )
        }
        content <- tags$div(
          tags$div("Finalizar partida?", class = "modal-title"),
          tags$hr(),
          tags$div(msg, style = "font-size: 22px")
        )

        shiny::showModal(
          shiny::modalDialog(
            content,
            title = NULL,
            footer = tagList(
              actionButton(NS(id, "cancel"), "Cancelar"),
              actionButton(NS(id, "confirm"), "Confirmar"),
            )
          )
        )
        observeEvent(input$cancel, {shiny::removeModal()},
          once = TRUE,
          ignoreInit = TRUE
        )
        observeEvent(input$confirm, {
            appCatch({
              if (is.null(rvs$player$get_next_prize())) {
                game_info <- list(
                  "parameters" = list(
                    "name" = store$partida_info$partida,
                    "serie" = games$serie(store$partida_info$partida),
                    "cards_n" = rvs$player$sales_count,
                    "date_start" = store$partida_info$date_start,
                    "date_end" = Sys.time(),
                    "sequence" = rvs$nums
                  ),
                  "prizes" = rvs$player$rvs$prizes_played
                )
                games$finalize_game(store$partida_info$partida, game_info)
              }
              # Reiniciar todo lo relacionado al juego.
              shiny::removeModal()
              disable_play_mode(parent_session)
              purrr::walk(paste0("num_", seq(90)), shinyjs::enable)
              rvs$nums <- numeric(0)
              rvs$player <- NULL
              store$playing <- FALSE
              shinyjs::js$hideHeader("")
              shinyjs::html("ball", "")
              shinyjs::runjs("clearInterval(countdown)")
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
get_winner <- function(card_id, prize, strips) {
  strip_id <- ((card_id - 1) %/% 6) + 1
  idx <- ((card_id - 1) %% 6) + 1
  strip <- strips[[strip_id]]
  card_numbers <- as.vector(strip[(3 * idx - 2):(3 * idx), ])
  list(
    numbers = card_numbers,
    card = card_id,
    prize = prize
  )
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
    ),
    tags$div(
      id = NS(id, paste0("winners-extra", name)),
      style = "font-size: 15px; font-weight: bold;"
    )
  )

}

add_single_winner <- function(x, y) {
  tags$div(
    class = "modal-winner-container",
    tags$div(x, class = "modal-winner-id"),
    tags$div(y, class = "modal-winner-seller")
  )
}

report_winners <- function(id, prize) {
  winners <- prize$winners
  ids <- vapply(winners, function(x) x$id, FUN.VALUE = numeric(1))
  sellers <- vapply(winners, function(x) x$seller, FUN.VALUE = character(1))

  ids <- paste0("N", intToUtf8(176), ids)
  sellers <- paste0("(", sellers, ")")

  name <- prize$name
  if (prize$cumulated) name <- paste(name, "(Pozo acumulado!)")

  extra <- ""
  if (is(prize, "SmallestMatchPrize")) {
    extra <- paste(prize$actual_matches, "aciertos")
  }

  content <- tags$div(
    tags$div(name, class = "modal-title"),
    tags$p(extra, class = "modal-aciertos-n"),
    tags$hr(),
    tags$p("Los ganadores son", class = "modal-title-lower"),
    mapply(add_single_winner, ids, sellers, SIMPLIFY = FALSE, USE.NAMES = FALSE)
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

  if (is(prize, "SmallestMatchPrize")) {
    shinyjs::html(
      paste0("winners-extra", name),
      paste(prize$actual_matches, "aciertos")
    )
  }
}

remove_winners <- function(prize) {
  name <- make_prize_name(prize$name)
  shinyjs::html(paste0("winners-", name), "")
  shinyjs::html(paste0("n-winners-", name), "")
  if (is(prize, "SmallestMatchPrize")) {
    shinyjs::html(paste0("winners-extra", name), "")
  }
}
