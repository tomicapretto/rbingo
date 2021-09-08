new_prize <- function(name, matches, space = c("row", "card")) {
  if (space == "row") {
    return(RowPrize$new(name, matches))
  } else {
    return(CardPrize$new(name, matches))
  }
}

Prize = R6::R6Class(
  classname = "Prize",
  public = list(
    name = NULL,
    matches = NULL,
    done = FALSE,
    draws = NULL,
    winners = NULL,
    initialize = function(name, matches) {
      self$name = name
      self$matches = matches
    },

    play = function(player, exclude) {
      matches <- self$get_matches(player)
      matches <- setdiff(matches, exclude)
      if (length(matches) > 0) {
        self$winners <- matches
        self$done <- TRUE
      } else {
        self$winners <- NULL
        self$done <- FALSE
      }
    },

    get_matches = function() {
      stop("Not implemented")
    }

  )
)

RowPrize = R6::R6Class(
  classname = "RowPrize",
  inherit = Prize,
  public = list(
    get_advances = function(player, step = 1) {
      label <- paste("Cartones con", self$matches - step, "aciertos en linea")
      value <- length(player$row_matches(self$matches - step))
      return(list(label = label, value = value))
    },
    get_matches = function(player) {
      player$row_matches(self$matches)
    }
  )
)

CardPrize = R6::R6Class(
  classname = "CardPrize",
  inherit = Prize,
  public = list(
    get_advances = function(player, step = 1) {
      label <- paste("Cartones con", self$matches - step, "aciertos")
      value <- length(player$card_matches(self$matches - step))
      return(list(label = label, value = value))
    },
    get_matches = function(player) {
      player$card_matches(self$matches)
    }
  )
)

