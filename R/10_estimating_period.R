#' Estimate Period
#'
#' @description Function to estimate the period and related variables like
#'  when the reign of the regent started, when it ended etc.
#'
#' @export
#'
gen_estimation <- function() {

  # TODO theoretically, this file is a modification of the "reign_summary.R" file
  # and, eventually, the both should be combined.


  remove_pattern <- c(
    "regent", "dep", "minister", "pretender", "region",
    "de facto", "civil", "chief", "in [A-z]{3,}"
  ) %>%
    paste0(collapse = "|")



  base_data <-
    gen_country() %>%
    dplyr::relocate(id_group, id, period, ruler) %>%
    dplyr::arrange(id_group, id) %>%
    # It is important to replace NA_character_ with the string "NA" because
    # otherwise the function to filter "pretender" periods is going also to delete
    # rows with missing period values.
    tidyr::replace_na(list(period = "")) %>%
    dplyr::filter(
      stringr::str_detect(
        period,
        stringr::regex(remove_pattern, ignore_case = TRUE),
        negate = TRUE
      )
    ) %>%
    dplyr::filter(
      stringr::str_detect(period, "\\d+")
    ) %>%
    dplyr::mutate(
      period = dplyr::if_else(
        period == "",
        NA_character_,
        as.character(period)
      )
    )

  # period = stringr::str_replace_all(
  #   period,
  #   "[XVI]+(?=\\s?\\.)",
  #   roman2numeric
  # )


  period_data <-
    base_data %>%
    dplyr::select(period) %>%
    dplyr::mutate(
      period = stringr::str_replace_all(period, "\\d+", "")
    ) %>%
    string_squish() %>%
    dplyr::distinct()

  # dplyr::select(id_group, id, ruler, period) %>%

  z_base_data_mod <-
    base_data %>%
    string_squish() %>%
    # Replacing "dato" with the date the current edition was published, 2002.
    dplyr::mutate(
      period = stringr::str_replace(period, "dato", "2002")
    ) %>%
    dplyr::mutate(
      # Number of years given per regent
      period_count = stringr::str_count(period, "\\d{3,4}"),
      # Binary to check whether the period was uncertain
      period_contains_dots = stringr::str_detect(period, "\\.\\."),
      period_dots_replaced = stringr::str_replace(period, "\\.\\.", "00"),
      dots_replaced_true = dplyr::if_else(
        (period_count == 0) & (period_contains_dots == TRUE), 1, 0
      ),
      # Only replace the dots with zeros IF there is only one year specified
      period = dplyr::if_else(
        dots_replaced_true == 1, period_dots_replaced, period
      )
    ) %>%
    dplyr::mutate(
      reign_start = stringr::str_extract(period, "\\d{3,4}(?=\\s?\\-\\s?\\d{3,4})"),
      reign_end = stringr::str_extract(period, "(?<=\\d{3,4}\\s?\\-\\s?)\\d{3,4}"),
      .after = period
    ) %>%
    tidyr::fill(reign_start, reign_end, .direction = "down") %>%
    dplyr::mutate(
      reign_start = as.integer(reign_start),
      reign_end = as.integer(reign_end),
      decade = dplyr::if_else(
        !is.na(reign_start),
        ceiling(reign_start / 10) * 10,
        NA_real_
      ),
      century = dplyr::if_else(
        !is.na(reign_start),
        ceiling(reign_start / 100) * 100,
        NA_real_
      ),
      half_cen = dplyr::if_else(
        !is.na(reign_start),
        ceiling(reign_start / 50) * 50,
        NA_real_
      )
    )

  return(z_base_data_mod)
}












#
#     ,
#     period_replaced = period,
#     .after = period
#   ) %>%
#   tidyr::fill(period_replaced, .direction = "down") %>%
#   dplyr::mutate(
#     period_estimated = dplyr::if_else(
#       is.na(period), 1, 0
#     )
#   ) %>%
#   dplyr::relocate(id_group, id, period, period_replaced, period_estimated, ruler)
#
#
#
#
#
