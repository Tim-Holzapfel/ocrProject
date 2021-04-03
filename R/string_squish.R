#' String Squish
#'
#' @description Removing leading, trailing and duplicate spaces from all string
#'   characters of a dataframe
#'
#' @param dataframe_tb dataframe that should be squished
#'
#' @importFrom rlang .data
#' @importFrom dplyr where
#'
#'
#' @note One thing one has to be careful about is that "str_squish" will return
#'   variables it has been applied upon as type character. This function
#'   therefore only selects columns of type character to keep non-string columns
#'   in the original type.
#'
string_squish <- function(dataframe_tb) {
  tb_return <-
    dataframe_tb %>%
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.character),
        .fns = stringr::str_squish
      )
    )
  return(tb_return)
}
