#' File Path
#'
#' @param file_append Name of file to append to path
#'
#' @export
#'
file_path <- function(file_append = "") {
  file_root <- dirname(rstudioapi::getSourceEditorContext()$path)

  return(file.path(file_root, file_append))
}
