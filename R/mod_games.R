gamesServer <- function(id, store, games) {
  moduleServer(id, function(input, output, session) {
    isolate({
      appCatch({
        mod_store <- reactiveValues()
      })
    })

    observeEvent(input$btn_add, {
      appCatch({
        req(input$nombre, input$fecha, input$tiras_n)
        if (input$tiras_n <= 0) {
          showError("La cantidad de tiras no puede ser menor a 1.")
        }
        if (input$tiras_n > 10000) {
          showError("La cantidad de tiras no puede ser mayor a 10000.")
        }
        games$new_game(input$nombre, input$fecha, round(input$tiras_n))
        shinypop::nx_notify_success(
          paste0("La partida '", input$nombre, "' fue creada exitosamente.")
        )
      })
    })

    observe({
      appCatch({
        updateSelectInput(
          session = session,
          inputId = "partida_mod",
          choices = games$names("unplayed")
        )
      })
    })

    observeEvent(input$partida_mod, {
      appCatch({
        if (input$partida_mod == "") {
          carton <- NA
          fecha <- NA
        } else {
          carton <- games$cards_n(input$partida_mod)
          fecha <- games$date(input$partida_mod)
        }
        updateNumericInput(
          session = session,
          inputId = "tiras_n_mod",
          value = carton / 6
        )
        updateDateInput(
          session = session,
          inputId = "fecha_mod",
          value = fecha
        )
      })
    })

    observeEvent(input$btn_modify, {
      appCatch({
        req(input$partida_mod, input$tiras_n_mod, input$fecha_mod)
        cards_n_current <- games$cards_n(input$partida_mod)
        date_current <- games$date(input$partida_mod)
        if (
          cards_n_current == (input$tiras_n_mod * 6) &
            date_current == input$fecha_mod
        ) {
          req(FALSE)
        }

        if (input$tiras_n_mod * 6 < cards_n_current) {
          showError(
            "No se puede disminuir la cantidad de cartones de una partida"
          )
        }
        games$modify_game(input$partida_mod, input$tiras_n_mod, input$fecha_mod)
        shinypop::nx_notify_info(
          paste0(
            "La partida '", input$partida_mod, "' fue modificada exitosamente."
          )
        )
      })
    })

    observe({
      appCatch({
        updateSelectInput(
          session = session,
          inputId = "partida_rem",
          choices = games$names("unplayed")
        )
      })
    })

    observeEvent(input$btn_remove, {
      appCatch({
        req(input$partida_rem)
        sales_count <- games$sales_count(input$partida_rem)
        if (sales_count != 0) {
          shinypop::nx_confirm(
            inputId = "confirm",
            title = "Confirmar operacion?",
            message = "Hay ventas registradas para la partida que intenta eliminar.",
            button_ok = "Confirmar",
            button_cancel = "Cancelar"
          )
          observeEvent(input$confirm,
            {
              if (input$confirm) games$remove_game(input$partida_rem)
            },
            once = TRUE,
            ignoreInit = TRUE
          )
        } else {
          games$remove_game(input$partida_rem)
        }
      })
    })

    mod_store$delete_info_txt <- eventReactive(input$partida_rem, {
      appCatch({
        if (isTruthy(input$partida_rem)) {
          msg <- c(
            "Fecha estipulada de juego: ",
            games$date(input$partida_rem), "\n",
            "Cantidad de cartones vendidos: ",
            games$sales_count(input$partida_rem), "\n",
            "Cantidad de cartones: ",
            games$cards_n(input$partida_rem)
          )
          paste0(msg)
        }
      })
    })

    output$delete_info_txt <- renderText({
      appCatch(mod_store$delete_info_txt())
    })

    # Seccion de reporte
    observe({
      appCatch({
        updateSelectInput(
          session = session,
          inputId = "partida_report",
          choices = games$names("played")
        )
      })
    })

    observe({
      appCatch({
        if (isTruthy(input$partida_report)) {
          shinyjs::enable("download")
        } else {
          shinyjs::disable("download")
        }
      })
    })

    output$download <- downloadHandler(
      filename = function() {
        appCatch(paste0("reporte_", input$partida_report, ".pdf"))
      },
      content = function(file) {
        appCatch({
          path <- file.path(games$path(input$partida_report), "report.pdf")
          file.copy(path, file)
        })
      }
    )

    observe({
      if (isTruthy(input$partida_report)) {
        results <- games$results(input$partida_report)
        cards <- results$cards_n
        serie <- results$serie
        balls <- results$balls_n
        date_start <- format(results$date_start, "%Y/%m/%d %H:%M:%S")
        date_end <- format(results$date_end, "%Y/%m/%d %H:%M:%S")
      } else {
        cards <- date <- serie <- balls <- date_start <- date_end <- ""
      }
      shinyjs::html("cards_summary", cards)
      shinyjs::html("serie_summary", serie)
      shinyjs::html("balls_summary", balls)
      shinyjs::html("date_start", date_start)
      shinyjs::html("date_end", date_end)
    })

    output$report_summary <- renderUI({
      req(input$partida_report)
      results <- games$results(input$partida_report)
      tagList(
        mapply(
          summaryRow,
          paste("Ganadores de", results$winners_names),
          results$winners_count,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      )
    })
  })
}

gamesUI <- function(id) {
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "Partidas a jugar",
        tabPanelCreate(id),
        tabPanelModify(id),
        tabPanelDelete(id)
      )
    ),
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "Partidas jugadas",
        tabPanelReports(id)
      )
    )
  )
}

tabPanelCreate <- function(id) {
  tabPanel(
    title = "Crear",
    fluidRow(
      colInput(
        f = textInput,
        colwidth = 6,
        inputId = NS(id, "nombre"),
        label = "Nombre de la partida",
        placeholder = "Escriba un nombre para la partida..."
      ),
      colInput(
        f = dateInput,
        colwidth = 6,
        inputId = NS(id, "fecha"),
        label = "Fecha tentativa",
        language = "es",
        format = "dd-mm-yyyy",
        min = Sys.Date()
      )
    ),
    fluidRow(
      colInput(
        f = numericInput,
        colwidth = 6,
        inputId = NS(id, "tiras_n"),
        label = "Cantidad de tiras",
        width = "100%",
        value = 400,
        min = 1,
        step = 1
      )
    ),
    fluidRow(
      colInput(
        f = actionButton,
        colwidth = 6,
        inputId = NS(id, "btn_add"),
        label = "Crear partida",
        width = "100%"
      )
    )
  )
}

tabPanelModify <- function(id) {
  tabPanel(
    title = "Modificar",
    fluidRow(
      colInput(
        f = selectInput,
        colwidth = 6,
        inputId = NS(id, "partida_mod"),
        label = "Partida",
        choices = ""
      ),
      colInput(
        f = dateInput,
        colwidth = 6,
        inputId = NS(id, "fecha_mod"),
        label = "Fecha tentativa",
        language = "es",
        format = "dd-mm-yyyy",
        min = Sys.Date()
      )
    ),
    fluidRow(
      colInput(
        f = numericInput,
        colwidth = 6,
        inputId = NS(id, "tiras_n_mod"),
        label = "Cantidad de tiras",
        width = "100%",
        value = NULL,
        min = 1,
        step = 1
      )
    ),
    fluidRow(
      colInput(
        f = actionButton,
        colwidth = 6,
        inputId = NS(id, "btn_modify"),
        label = "Modificar partida",
        width = "100%"
      )
    )
  )
}

tabPanelDelete <- function(id) {
  tabPanel(
    title = "Eliminar",
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = NS(id, "partida_rem"),
          label = "Partida",
          choices = ""
        ),
        actionButton(
          inputId = NS(id, "btn_remove"),
          label = "Eliminar partida",
          width = "100%"
        )
      ),
      column(
        width = 6,
        tags$div(
          style = "margin-top: 24.5px;",
          verbatimTextOutput(
            outputId = NS(id, "delete_info_txt"),
            placeholder = TRUE
          )
        )
      )
    )
  )
}

tabPanelReports <- function(id) {
  tabPanel(
    title = "Reportes",
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = NS(id, "partida_report"),
          label = "Partida",
          choices = ""
        ),
        downloadButton(
          outputId = NS(id, "download"),
          label = "Descargar reporte",
          style = "width: 100%;"
        )
      )
    ),
    tags$div(
      fluidRow(
        column(
          width = 6,
          summaryRow(
            name = "Cartones jugados",
            id = NS(id, "cards_summary")
          ),
          summaryRow(
            name = "Serie",
            id = NS(id, "serie_summary")
          ),
          summaryRow(
            name = "Bolillas jugadas",
            id = NS(id, "balls_summary")
          ),
          summaryRow(
            name = "Inicio",
            id = NS(id, "date_start")
          ),
          summaryRow(
            name = "Finalizacion",
            id = NS(id, "date_end")
          )
        ),
        column(
          width = 6,
          uiOutput(NS(id, "report_summary"))
        )
      ),
      style = paste(
        "padding: 7.5px",
        "border: 1px solid #aaa",
        "border-radius: 5px",
        "margin-top: 10px",
        sep = ";"
      )
    )
  )
}
