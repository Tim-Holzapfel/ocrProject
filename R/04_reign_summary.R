#' @title Reign Summary
#'
#' @description This function generates a summary about the reign of the regents
#'   including the length of the reign, the start and end year (if given) and
#'   the century and decade the ruler ruled in.
#'
#' @author Tim Holzapfel
#'
#' @importFrom rlang .data
#'
#' @export
#'
gen_reign_summary <- function() {
  base_data_set <- gen_deathyear()

  # TODO split the reign_start, reign_length etc variables into a lower and an
  # upper bound

  # The problem with determining the start and end year of a regents reign (the
  # time frame in which he ruled) is that, if only the century is known with
  # certainty but not the exact year then Truhart replaced the last two digits
  # with dots like 14.. which means that the ruler ruled in the 14th century.
  # Chances are that a regular expression will extract the same year for the
  # start and the end of rule. To make sure that that doesn't happen the end
  # year is only extract if it's preceded by a hyphen and the start year is set
  # to missing if it's equal to the end year. The end year cannot be mistaken
  # for the start year because of the preceding hyphen so the only candidate for
  # mistakes is the start year which is why it is set to missing if it equals
  # the end year.

  reign_data <-
    base_data_set %>%
    dplyr::mutate(
      reign_start = stringr::str_extract(.data$period, "\\d{3,4}") %>%
        as.numeric(),
      reign_end = stringi::stri_extract_last_regex(.data$period, "(?<=-)\\d{3,4}") %>%
        as.numeric(),
      reign_start = dplyr::na_if(.data$reign_start, .data$reign_end),
      reign_length = .data$reign_end - .data$reign_start,
      decade = dplyr::case_when(
        !is.na(reign_start) ~ round(.data$reign_start, -1),
        !is.na(reign_end) ~ round(.data$reign_end, -1)
      ),
      century = dplyr::case_when(
        !is.na(.data$reign_start) ~ round(.data$reign_start, -2),
        !is.na(.data$reign_end) ~ round(.data$reign_end, -2)
      ),
      half_cen = round(as.integer(.data$reign_start) / 50) * 50,
      .after = .data$period
    )

  return(reign_data)
}
