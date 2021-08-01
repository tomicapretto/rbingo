cardsServer <- function(id, store, games, cards) {
  moduleServer(id, function(input, output, session) {
    observe({
      appCatch({
        updateSelectInput(
          session = session,
          inputId = "partida_gen",
          choices = games$names("unplayed")
        )
      })
    })

    observeEvent(input$btn_generate, {
      appCatch({
        req(input$partida_gen)
        shinyjs::disable("btn_generate")
        shinyjs::disable("download")
        cards_n <- games$cards_n(input$partida_gen)
        serie <- paste("Serie", games$serie(input$partida_gen))

        message <- HTML(paste0(
          "Partida: ", input$partida_gen, ".<br>",
          "Numero de serie: ", serie, ".<br>",
          "Cantidad de tiras: ", cards_n / 6, ".<br>",
          "Cantidad de cartones: ", cards_n, "."
        ))

        shinypop::nx_confirm(
          inputId = "confirm",
          title = "Confirmar operacion?",
          message = message,
          button_ok = "Confirmar",
          button_cancel = "Cancelar"
        )
        observeEvent(input$confirm,
          {
            if (input$confirm) {
              if (!cards$loaded) {
                withProgress(
                  message = "Leyendo cartones...",
                  expr = cards$load_strips()
                )
              }

              withProgress(
                message = "Generando cartones...",
                expr = {
                  path <- games$path(input$partida_gen)
                  file <- file.path(path, "tiras.pdf")
                  grDevices::pdf(file, SHEET_WIDTH, SHEET_HEIGHT)
                  cards$print_pdf(cards_n, serie, input$color)
                  grDevices::dev.off()
                }
              )

              games$cards_status(input$partida_gen, TRUE)
            }
            shinyjs::enable("btn_generate")
          },
          once = TRUE,
          ignoreInit = TRUE
        )
      })
    })

    observe({
      appCatch({
        if (isTruthy(input$partida_dwnld)) {
          shinyjs::enable("download")
        } else {
          shinyjs::disable("download")
        }
      })
    })

    observe({
      appCatch({
        updateSelectInput(
          session = session,
          inputId = "partida_dwnld",
          choices = games$names("cards_generated")
        )
      })
    })

    output$download <- downloadHandler(
      filename = function() {
        appCatch(paste0("tiras_", input$partida_dwnld, ".pdf"))
      },
      content = function(file) {
        appCatch({
          path <- file.path(games$path(input$partida_dwnld), "tiras.pdf")
          file.copy(path, file)
        })
      }
    )
  })
}

cardsUI <- function(id) {
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "",
        tabPanel(
          title = "Generar",
          fluidRow(
            colInput(
              f = selectInput,
              colwidth = 6,
              inputId = NS(id, "partida_gen"),
              label = "Partida",
              choices = ""
            ),
            colInput(
              f = colourpicker::colourInput,
              colwidth = 6,
              inputId = NS(id, "color"),
              value = "#8e44ad",
              label = "Color",
              palette = "limited",
              allowedCols = c(
                "#8e44ad", "#2980b9", "#27ae60",
                "#c0392b", "#d35400", "#f39c12"
              )
            )
          ),
          fluidRow(
            colInput(
              f = actionButton,
              colwidth = 6,
              inputId = NS(id, "btn_generate"),
              label = "Generar",
              width = "100%"
            )
          )
        )
      )
    ),
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "",
        tabPanel(
          title = "Descargar",
          fluidRow(
            colInput(
              f = selectInput,
              colwidth = 6,
              inputId = NS(id, "partida_dwnld"),
              label = "Partida",
              choices = ""
            )
          ),
          fluidRow(
            column(
              width = 6,
              downloadButton(
                outputId = NS(id, "download"),
                label = "Descargar",
                style = "width:100%;"
              )
            )
          )
        )
      )
    )
  )
}