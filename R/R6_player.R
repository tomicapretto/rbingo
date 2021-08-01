Player = R6::R6Class(
  classname = "Player",
  cloneable = FALSE,
  class = FALSE,

  public = list(
    sales = NULL,
    sales_count = NULL,
    cards_sold = NULL,
    strips_sold = NULL,
    strips_sold_n = NULL,
    card_n = NULL,
    strip_n = NULL,
    row_pos = NULL,
    card_pos = NULL,
    row_count = NULL,
    card_count = NULL,

    initialize = function(game, cards) {

      self$sales = game$sales()
      self$sales_count = game$sales_count()

      self$cards_sold = unlist(Map(`:`, self$sales$desde, self$sales$hasta))
      self$strips_sold = unique(((self$cards_sold - 1) %/% 6) + 1)
      self$strips_sold_n = length(self$strips_sold)

      self$card_n = game$cards_n()
      self$strip_n = round(self$card_n / 6)

      self$row_pos = lapply(cards$row_pos, function(x) x[self$strips_sold])
      self$card_pos = lapply(cards$card_pos, function(x) x[self$strips_sold])

      self$row_count = replicate(self$strips_sold_n, vector("numeric", 18), FALSE)
      self$card_count = replicate(self$strips_sold_n, vector("numeric", 6), FALSE)

      names(self$row_count) = as.character(self$strips_sold)
      names(self$card_count) = as.character(self$strips_sold)
    },

    add_ball = function(ball) {
      row_pos = self$row_pos[[ball]]
      row_val = mapply(`[[`, self$row_count, pos = row_pos, USE.NAMES = FALSE) + 1
      self$row_count = Map(`[<-`, self$row_count, pos = row_pos, value = row_val)
      card_pos = self$card_pos[[ball]]
      card_val = mapply(`[[`, self$card_count, pos = card_pos, USE.NAMES = FALSE) + 1
      self$card_count = Map(`[<-`, self$card_count, pos = card_pos, value = card_val)
    },

    remove_ball = function(ball) {
      row_pos = self$row_pos[[ball]]
      row_val = mapply(`[[`, self$row_count, pos = row_pos, USE.NAMES = FALSE) - 1
      self$row_count = Map(`[<-`, self$row_count, pos = row_pos, value = row_val)

      card_pos = self$card_pos[[ball]]
      card_val = mapply(`[[`, self$card_count, pos = card_pos, USE.NAMES = FALSE) - 1
      self$card_count = Map(`[<-`, self$card_count, pos = card_pos, value = card_val)
    },

    row_matches = function(matches = 5, type = c("eq", "ge")) {
      type = match.arg(type)
      fun = switch(type, "eq" = `==`, "ge" = `>=`)

      matching_rows = lapply(self$row_count, function(x) {
        y = which(fun(x, matches))
        if (length(y) > 0) y else NULL
      })

      # Tiras que tienen al menos una fila que satisfacen la condicion
      strip_not_null = which(unlist(lapply(matching_rows, function(x) !is.null(x))))
      # Cartones, dentro de las tiras, que satisfacen la condicion
      matching_cards = lapply(matching_rows[strip_not_null], function(x) ((x - 1) %/% 3) + 1)
      # Tambien nos llevamos la fila que satisface la condicion, porque dos lineas
      # consecutivas pueden suceder en un mismo carton (por mas que sea muy raro)
      matching_rows = matching_rows[strip_not_null]
      # Numero de carton que satisface la condicion, en la escala original
      # (tiene en cuenta solo a los cartones vendidos)
      matches = unlist(Map(get_card_number, as.numeric(names(strip_not_null)), matching_cards))

      cards = matches[matches %in% self$cards_sold] # habia un unique antes
      # rows = unlist(matching_rows[which(matches %in% self$cards_sold)])
      # rows = ((rows - 1) %% 3) + 1
      # return(list("cards" = cards, "rows" = rows))
      return(unique(cards))
    },

    card_matches = function(matches = 15, type = c("eq", "ge")) {
      type = match.arg(type)
      fun = switch(type, "eq" = `==`, "ge" = `>=`)
      matching_cards = lapply(self$card_count, function(x) {
        y = which(fun(x, matches))
        if (length(y) > 0) y else NULL
      })
      strip_not_null = which(unlist(lapply(matching_cards, function(x) !is.null(x))))
      matching_cards = matching_cards[strip_not_null]

      matches = unlist(Map(get_card_number, as.numeric(names(strip_not_null)), matching_cards))
      unique(matches[matches %in% self$cards_sold])
    }
  )
)

get_card_number = function(ticket_n, card_n) {
  (ticket_n - 1) * 6 + card_n
}
