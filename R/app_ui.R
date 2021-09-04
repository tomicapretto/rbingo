app_ui <- function() {
  shinydashboard::dashboardPage(
    title = "Bingo",
    header = shinydashboard::dashboardHeader(title = "Bingo App"),
    body = ui_body(),
    sidebar = ui_sidebar()
  )
}

ui_body <- function() {
  shinydashboard::dashboardBody(
    class = "rbingo-body",
    shinyjs::useShinyjs(),
    tags$head(
      shiny::includeCSS(app_file("www", "board.css")),
      shiny::includeCSS(app_file("www", "bingo-ball.css")),
      shiny::includeCSS(app_file("www", "remove_link.css")),
      tags$style("rbingo-body {min-height: 100vh !important}"),
    ),

    # JS para ocultar el header. 'none' para ocultar y '' to mostrar
    shinyjs::extendShinyjs(
      text = "shinyjs.hideHeader = function(p) {$('header').css('display', p)}",
      functions = "hideHeader"
    ),
    # JS para hace focus en elementos
    shinyjs::extendShinyjs(
      text = "shinyjs.refocus = function(e_id) {
        document.getElementById(e_id).focus();
      }",
      functions = "refocus"
    ),

    # Activar notificaciones
    shinypop::use_notiflix_notify(
      position = "right-bottom",
      timeout = 8000
    ),
    shinypop::use_notiflix_confirm(
      okButtonBackground = "#367fa9",
      titleFontSize = "20px",
      titleColor = "#367fa9",
      messageFontSize = "16px",
      buttonsFontSize = "16px",
      borderRadius = "10px",
      width = "400px"
    ),
    shinypop::use_notiflix_report(
      borderRadius = "15px",
      titleFontSize = "24px",
      titleMaxLength = 50,
      messageFontSize = "20px",
      messageMaxLength = 1200,
      buttonFontSize = "24px",
      width = "360px",
      backOverlayColor = "rgba(0,0,0,0.05)"
    ),
    shinydashboard::tabItems(
      tabItemCustom(
        tabName = "games",
        moduleUI = gamesUI,
        moduleID = "games"
      ),
      tabItemCustom(
        tabName = "vendors",
        moduleUI = vendorsUI,
        moduleID = "vendors"
      ),
      tabItemCustom(
        tabName = "sales",
        moduleUI = salesUI,
        moduleID = "sales"
      ),
      tabItemCustom(
        tabName = "cards",
        moduleUI = cardsUI,
        moduleID = "cards"
      ),
      tabItemCustom(
        tabName = "play",
        moduleUI = playUI,
        moduleID = "play"
      ),
      tabItemCustom(
        tabName = "board",
        moduleUI = boardUI,
        moduleID = "board"
      )
    )
  )
}

ui_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      sidebarMenuHeader("Configuracion de partidas", "fa fa-cog fa-fw"),
      shinydashboard::menuItem(
        text = "Partidas",
        tabName = "games"
      ),
      shinydashboard::menuItem(
        text = "Generar cartones",
        tabName = "cards"
      ),
      sidebarMenuHeader("Base de datos", "fa fa-database"),
      shinydashboard::menuItem(
        text = "Vendedores",
        tabName = "vendors"
      ),
      shinydashboard::menuItem(
        text = "Cargar ventas",
        tabName = "sales"
      ),
      sidebarMenuHeader("Bingo", "fa fa-play"),
      shinydashboard::menuItem(
        text = "Jugar",
        tabName = "play"
      ),
      shinyjs::hidden(
        shinydashboard::menuItem(
          text = "Pizarra",
          tabName = "board"
        )
      )
    )
  )
}