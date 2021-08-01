vendorsServer <- function(id, store, games, vendors) {
  moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      appCatch({
        data <- vendors$get(drop_id = TRUE)
        data$` ` <- buttonColumn(nrow(data), NS(id, "remove_row"))
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
              list(width = "10%", class = "dt-right", targets = 4)
            )
          )
        )
      })
    })

    observeEvent(input$remove_row, {
      appCatch({
        row <- shiny::isolate(input$table_rows_selected)
        data <- vendors$get()
        req(nrow(data) > 0)
        id <- data[[row, "id"]]
        vendors$remove(id)
        games$remove_sales_by_vendor(vendor_id = id)
      })
    })

    observe({
      appCatch({
        choices <- vendors$get_var("id")
        names(choices) <- vendors$get_var("institucion")
        updateSelectInput(
          session = session,
          inputId = "id",
          choices = choices
        )
      })
    })

    observeEvent(input$btn_add, {
      appCatch({
        inputs <- c(
          input$institucion, input$localidad, input$direccion, input$contacto
        )
        if (any(!unlist(lapply(inputs, isTruthy)))) {
          showError("No se puede agregar vendedores si hay campos vacios")
        }

        if (input$institucion %in% vendors$get_var("institucion")) {
          showError("Ya existe una Institucion con ese nombre")
        }
        vendors$add(
          institucion = input$institucion,
          localidad = input$localidad,
          direccion = input$direccion,
          contacto = input$contacto
        )
      })
    })

    observeEvent(input$btn_remove, {
      appCatch({
        req(input$id)
        vendor_sales <- games$sales_by_vendor(vendor_id = input$id)
        if (is.null(vendor_sales) || nrow(vendor_sales) == 0) {
          vendors$remove(input$id)
        } else {
          msg <- paste(
            "Este vendedor tiene ventas realizadas que aun no fueron jugadas.",
            "Al borrar al vendedor, tambien eliminas sus ventas.",
            sep = "\n"
          )
          shinypop::nx_confirm(
            inputId = "confirm",
            title = "Confirmar operacion?",
            message = msg,
            button_ok = "Confirmar",
            button_cancel = "Cancelar"
          )
          observeEvent(input$confirm,
            {
              if (input$confirm) {
                vendors$remove(input$id)
                games$remove_sales_by_vendor(vendor_id = input$id)
              }
            },
            once = TRUE,
            ignoreInit = TRUE
          )
        }
      })
    })
  })
}


vendorsUI <- function(id) {
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "",
        tabPanel(
          title = "Agregar vendedores",
          fluidRow(
            colInput(
              f = textInput,
              colwidth = 6,
              inputId = NS(id, "institucion"),
              label = "Institucion",
              placeholder = "Escriba el nombre de la institucion."
            ),
            colInput(
              f = textInput,
              colwidth = 6,
              inputId = NS(id, "localidad"),
              label = "Localidad",
              placeholder = "Indique la localidad de la institucion."
            )
          ),
          fluidRow(
            colInput(
              f = textInput,
              colwidth = 6,
              inputId = NS(id, "direccion"),
              label = "Direccion",
              placeholder = "Indique la direccion de la institucion."
            ),
            colInput(
              f = textInput,
              colwidth = 6,
              inputId = NS(id, "contacto"),
              label = "Contacto",
              placeholder = "Indique un contacto de referencia."
            )
          ),
          fluidRow(
            colInput(
              f = actionButton,
              colwidth = 6,
              inputId = NS(id, "btn_add"),
              label = "Agregar",
              disabled = FALSE,
              width = "100%"
            )
          )
        ),
        tabPanel(
          title = "Borrar vendedores",
          fluidRow(
            colInput(
              f = selectInput,
              colwidth = 6,
              inputId = NS(id, "id"),
              label = "Institucion",
              disabled = FALSE,
              choices = ""
            )
          ),
          fluidRow(
            colInput(
              f = actionButton,
              colwidth = 6,
              inputId = NS(id, "btn_remove"),
              label = "Borrar",
              disabled = FALSE,
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
          title = "Vendedores registrados",
          DT::dataTableOutput(NS(id, "table"))
        )
      )
    )
  )
}