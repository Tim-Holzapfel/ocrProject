#' @title Find Excel Paths
#'
#' @param path Root directory
#'
find_excel_paths <- function(path) {
  excel_paths <-
    list.files(
      path = path,
      pattern = "\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = TRUE,
      all.files = TRUE
    )

  if (sjmisc::is_empty(excel_paths)) stop("I could not find any Excel-sheets.")

  return(excel_paths)
}
