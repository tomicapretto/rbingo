new_prize <- function(name, matches, space = c("row", "card"), cumulated = NULL) {
  if (space == "row") {
    return(RowPrize$new(name, matches))
  } else {
    if (matches == 0) {
      return(SmallestMatchPrize$new(name, matches))
    }
    return(CardPrize$new(name, matches, cumulated = cumulated))
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
    cumulated_n = NULL,
    cumulated = FALSE,
    initialize = function(name, matches, cumulated = NULL) {
      self$name = name
      self$matches = matches
      self$cumulated_n = cumulated
    },

    play = function(player, exclude) {
      winners <- self$get_matches(player, exclude)
      if (length(winners) > 0) {
        self$winners <- winners
        self$draws <- player$balls
        if (is.numeric(self$cumulated_n)) {
          if (length(self$draws) <= self$cumulated_n) {
            self$cumulated <- TRUE
          }
        }
        self$done <- TRUE
      } else {
        self$winners <- NULL
        self$draws <- NULL
        self$cumulated <- FALSE
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
