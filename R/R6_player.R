Player = R6::R6Class(
  classname = "Player",
  cloneable = FALSE,

  public = list(
    rvs = "<reactiveValues>",
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
    cards = NULL,
    prizes = NULL,
    balls = numeric(0),

    initialize = function(game, cards, prizes) {

      self$rvs <- reactiveValues()
      self$rvs$next_prize <- ""

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

      # Mantengo el listado de cartones para meterlo en el winner
      self$cards = cards

      # Una lista con objetos de clase Prize
      self$prizes = prizes
      self$rvs$prizes_played = list()
    },

    get_advances = function() {
      prize = self$get_next_prize()
      if (!is.null(prize)) {
        prize$get_advances(self)
      }
    },

    get_header = function() {
      prize = self$get_next_prize()
      if (is.null(prize)) {
        "Juego finalizado"
      } else {
        paste0("Siguiente premio<br>", prize$name)
      }
    },

    get_previous_prize = function() {
      if (length(self$rvs$prizes_played)) {
        self$rvs$prizes_played[[length(self$rvs$prizes_played)]]
      }
    },

    get_next_prize = function() {
      if (length(self$rvs$prizes_played) < length(self$prizes)) {
        self$prizes[[length(self$rvs$prizes_played) + 1]]
      }
    },

    check_prize_backward = function() {
      prize <- self$get_previous_prize()
      if (!is.null(prize)) {
        if (is(prize, "SmallestMatchPrize")) {
          prize$actual_matches <- NULL
          prize$done <- FALSE
        } else {
          prize$play(self, numeric(0))
        }
        if (!prize$done) {
          self$rvs$prizes_played <- head(self$rvs$prizes_played, -1)
          return(prize)
        }
      }
    },

    check_prize_forward = function() {
      # Devuelve un premio si es que se lo gana
      prize <- self$get_next_prize()
      if (!is.null(prize)) {
        exclude <- self$get_winners_id_of_equivalent_prize(prize)
        prize$play(self, exclude)
        if (prize$done) {
          self$rvs$prizes_played <- append(self$rvs$prizes_played, prize)
          return(prize)
        }
      }
    },

    get_winners_id_of_equivalent_prize = function(prize) {
      winners <- c()
      for (other_prize in self$rvs$prizes_played) {
        same_type_of_prize <- class(other_prize)[1] == class(prize)[1]
        same_matches <- other_prize$matches == prize$matches
        if (same_type_of_prize && same_matches) {
          winners_id <- vapply(
            other_prize$winners,
            function(x) x$id,
            FUN.VALUE = numeric(1)
          )
          winners <- unique(c(winners, winners_id))
        }
      }
      return(winners)
    },

    is_full_card_won = function() {
      for (prize in self$rvs$prizes_played) {
        if (prize$name == "Carton lleno") return(TRUE)
      }
      return(FALSE)
    },

    add_ball = function(ball) {
      row_pos = self$row_pos[[ball]]
      row_val = mapply(`[[`, self$row_count, pos = row_pos, USE.NAMES = FALSE) + 1
      self$row_count = Map(`[<-`, self$row_count, pos = row_pos, value = row_val)

      card_pos = self$card_pos[[ball]]
      card_val = mapply(`[[`, self$card_count, pos = card_pos, USE.NAMES = FALSE) + 1
      self$card_count = Map(`[<-`, self$card_count, pos = card_pos, value = card_val)
      self$balls <- c(self$balls, ball)
      return(self$check_prize_forward())
    },

    remove_ball = function(ball) {
      row_pos = self$row_pos[[ball]]
      row_val = mapply(`[[`, self$row_count, pos = row_pos, USE.NAMES = FALSE) - 1
      self$row_count = Map(`[<-`, self$row_count, pos = row_pos, value = row_val)

      card_pos = self$card_pos[[ball]]
      card_val = mapply(`[[`, self$card_count, pos = card_pos, USE.NAMES = FALSE) - 1
      self$card_count = Map(`[<-`, self$card_count, pos = card_pos, value = card_val)
      self$balls <- self$balls[self$balls != ball]
      return(self$check_prize_backward())
    },

    row_matches = function(matches = 5, exclude = c(), prize = FALSE) {
      matching_rows = lapply(self$row_count, function(x) {
        y = which(x == matches)
        if (length(y) > 0) y else NULL
      })

      # Tiras que tienen al menos una fila que satisfacen la condicion
      strip_not_null = which(!unlist(lapply(matching_rows, is.null)))

      # Cartones, dentro de las tiras, que satisfacen la condicion
      matching_cards = lapply(
        matching_rows[strip_not_null],
        function(x) ((x - 1) %/% 3) + 1
      )

      # Tambien nos llevamos la fila que satisface la condicion, porque dos lineas
      # consecutivas pueden suceder en un mismo carton (por mas que sea muy raro)
      matching_rows = matching_rows[strip_not_null]

      # Numero de carton que satisface la condicion, en la escala original
      # (tiene en cuenta solo a los cartones vendidos)
      matches = unlist(
        Map(get_card_number, as.numeric(names(strip_not_null)), matching_cards)
      )
      cards <- unique(matches[matches %in% self$cards_sold])
      if (!is.null(cards)) {
        if (prize) {
          return(self$make_winners(cards))
        } else {
          return(cards)
        }
      }
    },

    card_matches = function(matches = 15, exclude = c(), prize = FALSE) {
      matching_cards = lapply(self$card_count, function(x) {
        y = which(x == matches)
        if (length(y) > 0) y else NULL
      })
      strip_not_null = which(!unlist(lapply(matching_cards, is.null)))
      matching_cards = matching_cards[strip_not_null]
      matches = unlist(
        Map(get_card_number, as.numeric(names(strip_not_null)), matching_cards)
      )
      cards <- unique(matches[matches %in% self$cards_sold])
      cards <- setdiff(cards, exclude)
      if (!is.null(cards)) {
        if (prize) {
          return(self$make_winners(cards))
        } else {
          return(cards)
        }
      }
    },

    make_winners = function(cards) {
      winners <- vector("list", length(cards))
      for (i in seq_along(cards)) {
        card_id <- cards[i]
        idx <- between_which(card_id, self$sales$desde, self$sales$hasta)
        seller <- self$sales[idx, "institucion"]
        card <- self$cards$get_card_by_id(card_id)
        winners[[i]] <- Winner$new(card_id, seller, card)
      }
      return(winners)
    }

  )
)

get_card_number <- function(ticket_n, card_n) {
  (ticket_n - 1) * 6 + card_n
}

between_which <- function(x, low, high) {
  which(low <= x & x <= high )
}
