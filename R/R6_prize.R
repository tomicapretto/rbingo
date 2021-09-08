new_prize <- function(name, matches, space = c("row", "card")) {
  if (space == "row") {
    return(RowPrize$new(name, matches))
  } else {
    if (matches == 0) {
      return(SmallestMatchPrize$new(name, matches))
    }
    return(CardPrize$new(name, matches))
  }
}

Prize = R6::R6Class(
  classname = "Prize",
  cloneable = FALSE,
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
      winners <- self$get_matches(player, exclude)
      if (length(winners) > 0) {
        self$winners <- winners
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
    get_matches = function(player, exclude) {
      player$row_matches(self$matches, exclude = exclude, prize = TRUE)
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
    get_matches = function(player, exclude) {
      player$card_matches(self$matches, exclude = exclude, prize = TRUE)
    }
  )
)

SmallestMatchPrize =  R6::R6Class(
  classname = "SmallestMatchPrize",
  inherit = Prize,
  public = list(
    actual_matches = NULL,
    get_advances = function(player, step = 1) {
      return(NULL)
    },
    get_matches = function(player, exclude) {
      found <- FALSE
      matches <- 0
      while (!found) {
        winners <- player$card_matches(matches, prize = TRUE)
        if (length(winners)) {
          found <- TRUE
        } else {
          matches <- matches + 1
        }
      }
      self$actual_matches <- matches
      return(winners)
    }
  )
)
