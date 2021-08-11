#' Roman To Alpha
#'
#' @param x Roman number
#'
#' @return Abbreviated name of the month
#'
#' @description This function takes a roman numeral as argument, replaces it
#'   with an Arabic number and finally returns the month the roman numeral
#'   represented based on the Arabic number
#'
roman_to_alpha <- function(x) {
  month.abb[roman2numeric(x)]
}
