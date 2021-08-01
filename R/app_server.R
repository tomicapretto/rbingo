app_server <- function(input, output, session) {
  store <- reactiveValues(playing = FALSE)

  isolate({
    vendors <- Vendors$new()
    games <- Games$new(vendors)
    cards <- Cards$new()
  })

  gamesServer("games", store, games)
  vendorsServer("vendors", store, games, vendors)
  salesServer("sales", store, games, vendors)
  cardsServer("cards", store, games, cards)
  playServer("play", store, games, cards, session)
  boardServer("board", store, games, cards, session)
}

# Notas:
# * Tal vez los objetos R6 no deberian ir dentro de "store".
#   ya que se pasan por referencia (no se copian) y evitaria tener que
#   escribir store por todos lados.