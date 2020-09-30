#' Find Files in Project
#'
#' @param data_file Name of file to search for
#'
#' @export
#'
find_files <- function(data_file) {
  found_file <- list.files(
    pattern = paste0("^", data_file, "$"),
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )

  if (length(found_file) > 1) stop("Found more than one file.")

  if (length(found_file) == 0) stop("Didn't find any file. Maybe wrong file ending?")

  return(found_file)
}
