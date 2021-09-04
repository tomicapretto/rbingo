playServer <- function(id, store, games, cards, parent_session) {
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

    observe({
      appCatch({
        if (isTruthy(input$partida)) {
          shinyjs::enable("btn_jugar")
        } else {
          shinyjs::disable("btn_jugar")
        }
      })
    })

    observeEvent(input$btn_jugar, {
      appCatch({
        if (!isTruthy(input$prizes)) {
          showError("Se debe seleccionar al menos un premio")
        }
        if (!"Carton lleno" %in% input$prizes) {
          showError("No se puede jugar sin carton lleno")
        }
        # Chequear que hay cartones vendidos (no se juega sin cartones vendidos)
        if (games$sales_count(input$partida) == 0) {
          showError("No se puede jugar sin cartones vendidos")
        }
        store$partida_info <- list(
          "partida" = input$partida,
          "prizes" = input$prizes,
          "pozo_acumulado" = input$pozo_acumulado,
          "date_start" = Sys.time()
        )
        store$playing <- TRUE
        if (!cards$loaded) {
          withProgress(
            message = "Leyendo cartones...",
            expr = cards$load_strips()
          )
        }
        enable_play_mode(parent_session)
        shinyjs::js$hideHeader("none")
      })
    })
  })
}

playUI <- function(id) {
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "",
        tabPanel(
          title = "Configurar",
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = NS(id, "partida"),
                label = "Partida",
                choices = ""
              ),
              numericInput(
                inputId = NS(id, "pozo_acumulado"),
                label = "Pozo acumulado en bolilla",
                value = 20, min = 15, max = 90
              ),
              actionButton(
                inputId = NS(id, "btn_jugar"),
                label = "Jugar",
                width = "100%"
              )
            ),
            column(
              width = 6,
              shinyWidgets::checkboxGroupButtons(
                inputId = NS(id, "prizes"),
                label = "Seleccione los premios a sortear",
                choices = c(
                  "Terno", "Cuaterno", "Linea", "Carton lleno",
                  "Bingo consuelo", "Menor acierto"
                ),
                selected = c(
                  "Terno", "Cuaterno", "Linea", "Carton lleno",
                  "Bingo consuelo", "Menor acierto"
                ),
                status = "primary", justified = TRUE, individual = TRUE,
                width = "100%"
              )
            )
          )
        )
      )
    )
  )
}
