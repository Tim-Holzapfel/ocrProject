#' Clear Workspace and Console
#'
#' @export
#'
clear <- function() {
  remove(list = ls(pos = 1), pos = 1)
  gc()
  cat("\f")
}
