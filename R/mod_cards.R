# Server
cardsServer <- function(id, store, games, cards) {
  moduleServer(id, function(input, output, session) {
    cardsGenerateServer("generate", store, games, cards)
    cardsDownloadServer("download", store, games, cards)
  })
}

cardsGenerateServer <- function(id, store, games, cards) {
  moduleServer(id, function(input, output, session) {
    observe({
      appCatch({
        updateSelectInput(
          session = session,
          inputId = "partida",
          choices = games$names("unplayed")
        )
      })
    })

    observeEvent(input$btn_generar, {
      appCatch({
        req(input$partida)
        shinyjs::disable("btn_generar")

        cards_n <- games$cards_n(input$partida)
        serie <- paste("Serie", games$serie(input$partida))

        message <- HTML(paste0(
          "Partida: ", input$partida, ".<br>",
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
                  file <- file.path(games$path(input$partida), "tiras.pdf")
                  grDevices::pdf(file, SHEET_WIDTH, SHEET_HEIGHT)
                  cards$print_pdf(cards_n, serie, input$color)
                  grDevices::dev.off()
                }
              )
              games$cards_status(input$partida, TRUE)
            }
            shinyjs::enable("btn_generar")
          },
          once = TRUE,
          ignoreInit = TRUE
        )
      })
    })
  })
}

cardsDownloadServer <- function(id, store, games, cards) {
  moduleServer(id, function(input, output, session) {
    observe({
      appCatch({
        if (isTruthy(input$partida)) {
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
          inputId = "partida",
          choices = games$names("cards_generated")
        )
      })
    })
    output$btn_descargar <- downloadHandler(
      filename = function() {
        appCatch(paste0("tiras_", input$partida, ".pdf"))
      },
      content = function(file) {
        req(input$partida)
        appCatch({
          path <- file.path(games$path(input$partida), "tiras.pdf")
          file.copy(path, file)
        })
      }
    )
  })
}

# UI
cardsGenerateUI <- function(id) {
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
            inputId = NS(id, "partida"),
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
            inputId = NS(id, "btn_generar"),
            label = "Generar",
            width = "100%"
          )
        )
      )
    )
  )
}

cardsDownloadUI <- function(id) {
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
            inputId = NS(id, "partida"),
            label = "Partida",
            choices = ""
          )
        ),
        fluidRow(
          column(
            width = 6,
            downloadButton(
              outputId = NS(id, "btn_descargar"),
              label = "Descargar",
              style = "width: 100%;"
            )
          )
        )
      )
    )
  )
}

cardsUI <- function(id) {
  tagList(
    cardsGenerateUI(NS(id, "generate")),
    cardsDownloadUI(NS(id, "download"))
  )
}