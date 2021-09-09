Winner <- R6::R6Class(
  classname = "Winner",
  cloneable = FALSE,
  public = list(
    id = NULL,
    seller = NULL,
    card = NULL,
    initialize = function(id, seller, card, draws) {
      self$id = id
      self$seller = seller
      self$card = card
    }
  )
)
