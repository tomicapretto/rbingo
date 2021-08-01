Vendors <- R6::R6Class(
  "Vendors",
  cloneable = FALSE,
  class = FALSE,
  public = list(
    rvs = "<reactiveValues>",
    vcount = 0, # vendors count

    initialize = function() {
      self$rvs <- reactiveValues()
      path <- file.path(app_file("data", "vendors"), "df.rds")
      if (file.exists(path)) {
        self$rvs$df <- readRDS(path)
      } else {
        self$rvs$df <- data.frame(matrix(nrow = 0, ncol = 5), stringsAsFactors = FALSE)
        col_nms <- c("id", "institucion", "localidad", "direccion", "contacto")
        colnames(self$rvs$df) <- col_nms
      }

      path <- file.path(app_file("data", "vendors"), "vcount.rds")
      if (file.exists(path)) {
        self$vcount <- readRDS(path)
      }
    },
    sort = function() {
      self$rvs$df <- self$rvs$df[order(self$rvs$df$id), ]
    },
    save = function() {
      self$sort()
      saveRDS(self$rvs$df, file.path(app_file("data", "vendors"), "df.rds"))
      saveRDS(self$vcount, file.path(app_file("data", "vendors"), "vcount.rds"))
    },
    get = function(drop_id = FALSE) {
      if (drop_id) {
        data <- self$rvs$df
        return(data[-1])
      } else {
        return(self$rvs$df)
      }
    },
    get_var = function(var) {
      self$rvs$df[[var]]
    },
    get_institutions = function() {
      x <- unique(self$rvs$df$id)
      names(x) <- unique(self$rvs$df$institucion)
      return(x)
    },
    add = function(institucion, localidad, direccion, contacto) {
      self$vcount <- self$vcount + 1
      id <- self$vcount
      self$rvs$df[nrow(self$rvs$df) + 1, ] <- list(
        id, institucion, localidad,
        direccion, contacto
      )
      self$save()
    },
    remove = function(id) {
      self$rvs$df <- self$rvs$df[self$rvs$df$id != id, ]
      self$save()
    }
  )
)