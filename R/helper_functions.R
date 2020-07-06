#' Open File in Stata
#'
#' @description The Stata data viewer is clearly superior to the R data viewer
#'   at the moment. Thus it makes sense to utilize the capabilities of both
#'   programs to achieve maximum results.
#'
#' @param file R file that is to be opened in Stata
#'
#' @return Stata file
#' @export
#'
open_stata <- function(file) {
  temp_stata <- tempfile(fileext = ".dta")

  haven::write_dta(file, temp_stata)

  shell(temp_stata)
}
