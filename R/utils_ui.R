colInput <- function(f, colwidth, inputId, label, disabled = FALSE, ...) {
  if (disabled) {
    column(
      width = colwidth,
      shinyjs::disabled(
        f(inputId = inputId, label = label, ...)
      )
    )
  } else {
    column(
      width = colwidth,
      f(inputId = inputId, label = label, ...)
    )
  }
}

tabItemCustom <- function(tabName, moduleUI, moduleID) {
  shinydashboard::tabItem(
    tabName = tabName,
    wellPanel(
      style = "background: transparent; border: transparent; box-shadow: none;",
      moduleUI(moduleID)
    )
  )
}

bingoBall <- function(id) {
  tags$div(
    class = "ball-display",
    tags$div(
      class = "content",
      tags$div(
        class = "ball-content",
        tags$div(
          class = "ball-number", id = id
        )
      )
    )
  )
}


sidebarMenuHeader <- function(text, icon_class) {
  tags$li(
    class = "header",
    style = "font-size: 14px",
    tags$i(class = icon_class),
    text
  )
}

summaryRow <- function(name, value = NULL, id = NULL) {
  fluidRow(
    column(
      width = 6,
      tags$p(
        name,
        style = "text-align:left"
      )
    ),
    column(
      width = 6,
      tags$strong(
        tags$p(
          value,
          id = id,
          style = "text-align:right"
        )
      )
    )
  )
}

horizontal_line <- function() {
  tags$hr(class = "board-hr")
}
