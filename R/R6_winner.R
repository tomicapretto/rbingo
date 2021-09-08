Winner <- R6::R6Class(
  classname = "Winner",
  cloneable = FALSE,
  public = list(
    id = NULL,
    seller = NULL,
    initialize = function(id, seller) {
      self$id = id
      self$seller = seller
    }
  )
)
