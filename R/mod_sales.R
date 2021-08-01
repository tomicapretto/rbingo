salesServer <- function(id, store, games, vendors) {
  moduleServer(id, function(input, output, session) {
    isolate({
      appCatch({
        mod_store <- reactiveValues(row = NULL)
      })
    })

    output$table <- DT::renderDT({
      appCatch({
        if (length(games$names("unplayed")) == 0) req(FALSE)
        data <- games$sales(input$partida)
        data$` ` <- buttonColumn(nrow(data), NS(id, "remove_row"))
        id_column <- which(colnames(data) == "id")
        if (length(id_column)) data <- data[-id_column]
        DT::datatable(
          data = data,
          rownames = FALSE,
          colnames = stringr::str_to_title(colnames(data)),
          escape = FALSE,
          selection = "single",
          options = list(
            pageLength = 10,
            searching = FALSE,
            lengthChange = FALSE,
            language = DT_SPANISH,
            columnDefs = list(
              list(width = "10%", class = "dt-right", targets = ncol(data))
            )
          )
        )
      })
    })

    observeEvent(input$remove_row, {
      appCatch({
        row <- shiny::isolate(input$table_rows_selected)
        data <- games$sales(input$partida)
        req(nrow(data) > 0)
        games$remove_sales(
          game = input$partida,
          desde = data[[row, "desde"]],
          hasta = data[[row, "hasta"]]
        )
      })
    })

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
        updateSelectInput(
          session = session,
          inputId = "institucion",
          choices = vendors$get_var("institucion")
        )
      })
    })

    output$text_summary <- renderText({
      appCatch({
        if (isTruthy(input$partida)) {
          games$print_sales_summary(input$partida)
        } else {
          "No hay partidas disponibles."
        }
      })
    })

    observeEvent(input$btn_load, {
      appCatch({
        inputs <- c(input$partida, input$institucion, input$from, input$to)
        if (any(!unlist(lapply(inputs, isTruthy)))) {
          showError("No se pueden agregar ventas si hay campos vacios.")
        }
        if (input$from > input$to) {
          showError("'Desde' tiene que ser menor que 'Hasta'.")
        }
        if (input$from <= 0 || input$to <= 0) {
          showError("'Desde' y 'Hasta' tienen que ser mayor a 1.")
        }
        cards_n <- games$cards_n(input$partida)
        if (input$from > cards_n) {
          showError(
            "'Desde' tiene que ser menor a la cantidad de cartones en la partida."
          )
        }
        if (input$to > cards_n) {
          showError(
            "'Hasta' tiene que ser menor a la cantidad de cartones en la partida."
          )
        }
        games$add_sales(
          game = input$partida,
          institucion = input$institucion,
          desde = input$from,
          hasta = input$to
        )
      })
      req(input$partida, input$institucion, input$from, input$to)
      updateNumericInput(session, "from", value = numeric(0))
      updateNumericInput(session, "to", value = numeric(0))
      shinyjs::js$refocus(NS(id, "from"))
    })

    observeEvent(input$btn_remove, {
      appCatch({
        if (!isTruthy(input$partida)) {
          showError(
            "No se puede eliminar ventas porque no hay ninguna partida seleccionada."
          )
        }
        sales_count <- games$sales_count(input$partida)
        if (sales_count == 0) req(FALSE)
        inputs <- c(input$from, input$to)
        if (any(!unlist(lapply(inputs, isTruthy)))) {
          showError("No se puede eliminar ventas si falta uno de los limites.")
        }
        if (input$from <= 0 || input$to <= 0) {
          showError("'Desde' y 'Hasta' tienen que ser mayor a 1.")
        }
        if (input$from > input$to) {
          showError("'Desde' tiene que ser menor que 'Hasta'.")
        }
        games$remove_sales(
          game = input$partida,
          desde = input$from,
          hasta = input$to
        )
      })
      req(input$partida, input$institucion, input$from, input$to)
      updateNumericInput(session, "from", value = numeric(0))
      updateNumericInput(session, "to", value = numeric(0))
      shinyjs::js$refocus(NS(id, "from"))
    })
  })
}



salesUI <- function(id) {
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "",
        tabPanel(
          title = "Cargar ventas",
          fluidRow(
            colInput(
              f = selectInput,
              colwidth = 6,
              inputId = NS(id, "partida"),
              label = "Partida",
              choices = ""
            ),
            colInput(
              f = selectInput,
              colwidth = 6,
              inputId = NS(id, "institucion"),
              label = "Institucion",
              choices = ""
            )
          ),
          fluidRow(
            colInput(
              f = numericInput,
              colwidth = 6,
              inputId = NS(id, "from"),
              label = "Desde",
              value = 1,
              min = 1
            ),
            colInput(
              f = numericInput,
              colwidth = 6,
              inputId = NS(id, "to"),
              label = "Hasta",
              value = 10,
              min = 1
            )
          ),
          fluidRow(
            colInput(
              f = actionButton,
              colwidth = 6,
              inputId = NS(id, "btn_load"),
              label = "Cargar",
              width = "100%"
            ),
            colInput(
              f = actionButton,
              colwidth = 6,
              inputId = NS(id, "btn_remove"),
              label = "Borrar",
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
        title = "Ventas registradas",
        tabPanel(
          title = "Resumen",
          verbatimTextOutput(NS(id, "text_summary"))
        ),
        tabPanel(
          title = "Tabla",
          DT::dataTableOutput(NS(id, "table"))
        )
      )
    )
  )
}