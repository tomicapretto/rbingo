gamesServer <- function(id, games) {
  moduleServer(id, function(input, output, session) {
    gamesCreateServer("crear", games)
    gamesModifyServer("modificar", games)
    gamesDeleteServer("eliminar", games)
    gamesReportsServer("reportes", games)
  })
}

gamesCreateServer <- function(id, games) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$btn_crear, {
      appCatch({
        req(input$nombre, input$fecha, input$tiras_n)
        if (input$tiras_n <= 0) {
          showError("La cantidad de tiras no puede ser menor a 1.")
        }
        if (input$tiras_n > 10000) {
          showError("La cantidad de tiras no puede ser mayor a 10000.")
        }
        games$new_game(input$nombre, input$fecha, round(input$tiras_n))
        showSuccess(
          paste0("La partida '", input$nombre, "' fue creada exitosamente.")
        )
      })
    })
  })
}

gamesModifyServer <- function(id, games) {
  moduleServer(id, function(input, output, session) {
    observe({
      appCatch({
        updateSelectInput(session, "partida", choices = games$names("unplayed"))
      })
    })

    observeEvent(input$partida, {
      req(input$partida)
      appCatch({
        cartones <- games$cards_n(input$partida)
        fecha <- games$date(input$partida)
        updateNumericInput(session, "tiras_n", value = cartones / 6)
        updateDateInput(session, "fecha", value = fecha)
      })
    })

    observeEvent(input$btn_modificar, {
      appCatch({
        req(input$partida, input$tiras_n, input$fecha)
        cards_n_current <- games$cards_n(input$partida)
        date_current <- games$date(input$partida)
        if (
          cards_n_current == (input$tiras_n * 6) &
          date_current == input$fecha) {
          req(FALSE)
        }
        if (input$tiras_n * 6 < cards_n_current) {
          showError("No se puede disminuir la cantidad de cartones de una partida")
        }
        games$modify_game(input$partida, input$tiras_n, input$fecha)
        showInfo(
          paste0("La partida '", input$partida, "' fue modificada exitosamente.")
        )
      })
    })
  })
}

gamesDeleteServer <- function(id, games) {
  moduleServer(id, function(input, output, session) {
    observe({
      appCatch({
        updateSelectInput(session, "partida", choices = games$names("unplayed"))
      })
    })

    observeEvent(input$btn_eliminar, {
      appCatch({
        req(input$partida)
        sales_count <- games$sales_count(input$partida)
        if (sales_count != 0) {
          shinypop::nx_confirm(
            inputId = "confirm",
            title = "Confirmar operacion?",
            message = "Hay ventas registradas para la partida que intenta eliminar.",
            button_ok = "Confirmar",
            button_cancel = "Cancelar"
          )
          observeEvent(input$confirm, {
            if (input$confirm) games$remove_game(input$partida)
            },
            once = TRUE,
            ignoreInit = TRUE
          )
        } else {
          games$remove_game(input$partida)
        }
      })
    })


    info <- eventReactive(input$partida, {
      appCatch({
        if (isTruthy(input$partida)) {
          msg <- c(
            " Fecha estipulada de juego: ",
            format(games$date(input$partida), "%d-%m-%Y"), "\n",
            "Cantidad de cartones vendidos: ",
            games$sales_count(input$partida), "\n",
            "Cantidad de cartones: ",
            games$cards_n(input$partida)
          )
          paste0(msg)
        }
      })
    })

    output$info <- renderText({
      info()
    })

  })
}

gamesReportsServer <- function(id, games) {
  moduleServer(id, function(input, output, session) {
    observe({
      appCatch({
        updateSelectInput(session, "partida", choices = games$names("played"))
      })
    })

    observe({
      appCatch({
        if (isTruthy(input$partida)) {
          shinyjs::enable("download")
        } else {
          shinyjs::disable("download")
        }
      })
    })

    output$descargar <- downloadHandler(
      filename = function() {
        appCatch(paste0("reporte_", input$partida, ".pdf"))
      },
      content = function(file) {
        appCatch({
          path <- file.path(games$path(input$partida), "report.pdf")
          file.copy(path, file)
        })
      }
    )

    observe({
      if (isTruthy(input$partida)) {
        results <- games$results(input$partida)
        cards <- results$cards_n
        serie <- results$serie
        balls <- results$balls_n
        date_start <- format(results$date_start, "%m/%d/%Y %H:%M:%S")
        date_end <- format(results$date_end, "%m/%d/%Y %H:%M:%S")
      } else {
        cards <- date <- serie <- balls <- date_start <- date_end <- ""
      }
      shinyjs::html("cartones_jugados", cards)
      shinyjs::html("serie", serie)
      shinyjs::html("bolillas_jugadas", balls)
      shinyjs::html("fecha_inicio", date_start)
      shinyjs::html("fecha_finalizacion", date_end)
    })

    output$resumen_reporte <- renderUI({
      req(input$partida)
      results <- games$results(input$partida)
      tagList(
        mapply(
          summaryRow,
          paste("Ganadores de", results$prize_names),
          results$winners_n,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      )
    })
  })
}

# UI
gamesUI <- function(id) {
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "Partidas a jugar",
        gamesCreateUI(NS(id, "crear")),
        gamesModifyUI(NS(id, "modificar")),
        gamesDeleteUI(NS(id, "eliminar"))
      )
    ),
    fluidRow(
      shinydashboard::tabBox(
        height = "auto",
        width = 12,
        title = "Partidas jugadas",
        gamesReportsUI(NS(id, "reportes"))
      )
    )
  )
}

gamesCreateUI <- function(id) {
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
        inputId = NS(id, "btn_crear"),
        label = "Crear partida",
        width = "100%"
      )
    )
  )
}

gamesModifyUI <- function(id) {
  tabPanel(
    title = "Modificar",
    fluidRow(
      colInput(
        f = selectInput,
        colwidth = 6,
        inputId = NS(id, "partida"),
        label = "Partida",
        choices = ""
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
        value = NULL,
        min = 1,
        step = 1
      )
    ),
    fluidRow(
      colInput(
        f = actionButton,
        colwidth = 6,
        inputId = NS(id, "btn_modificar"),
        label = "Modificar partida",
        width = "100%"
      )
    )
  )
}

gamesDeleteUI <- function(id) {
  tabPanel(
    title = "Eliminar",
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = NS(id, "partida"),
          label = "Partida",
          choices = ""
        ),
        actionButton(
          inputId = NS(id, "btn_eliminar"),
          label = "Eliminar partida",
          width = "100%"
        )
      ),
      column(
        width = 6,
        tags$div(
          style = "margin-top: 24.5px;",
          verbatimTextOutput(
            outputId = NS(id, "info"),
            placeholder = TRUE
          )
        )
      )
    )
  )
}

gamesReportsUI <- function(id) {
  tabPanel(
    title = "Reportes",
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = NS(id, "partida"),
          label = "Partida",
          choices = ""
        ),
        downloadButton(
          outputId = NS(id, "descargar"),
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
            id = NS(id, "cartones_jugados")
          ),
          summaryRow(
            name = "Serie",
            id = NS(id, "serie")
          ),
          summaryRow(
            name = "Bolillas jugadas",
            id = NS(id, "bolillas_jugadas")
          ),
          summaryRow(
            name = "Inicio",
            id = NS(id, "fecha_inicio")
          ),
          summaryRow(
            name = "Finalizacion",
            id = NS(id, "fecha_finalizacion")
          )
        ),
        column(
          width = 6,
          uiOutput(NS(id, "resumen_reporte"))
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
