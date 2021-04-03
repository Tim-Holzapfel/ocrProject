#' @title Clear Workspace and Console
#'
clear <- function() {
  remove(list = ls(pos = 1), pos = 1)
  gc()
  cat("\f")
}
