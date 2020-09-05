#' Correcting Region Data
#'
#' @description Removing invalid IDs, meaning regent IDs that have been entered
#'   wrong and include typos.
#'
#' @return header_data_corrected
#'
#' @export
#'
correcting_region_ids <- function() {
  # Integration point: after gen_header() because then the header rows are gone
  # which makes correcting the regent ids a lot easier.

  complete_data <- gen_header()

  # While checking the table of contents of all three Truhart books, it was
  # obvious that none of the level IDs is smaller than 2 (e.g. 66.53).
  #
  # The smallest level ID is 29-30. All IDs smaller than that must be wrong.

  valid_patterns <-
    stringr::regex("
      (^\\d{2,3}\\.$)
        |
      (^\\d{2}-\\d{2}$)
        |
      (^\\d{3}-\\d{3}$)
        |
      (^\\d{2,3}\\.\\d{1,3}$)
        |
      (^\\d{2,3}\\.\\d{1}-\\d{1}$)
        |
      (^\\d{2,3}\\.\\d{2}-\\d{2}$)
        |
      (^\\d{2,3}\\.\\d{3}-\\d{3}$)
      ", comments = TRUE)

  all_ids <-
    complete_data %>%
    dplyr::select(id) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      unique_id = 1:dplyr::n()
    ) %>%
    dplyr::filter(
      stringr::str_detect(id, "^\\d{3}$", negate = TRUE)
    )

  valid_ids <-
    all_ids %>%
    dplyr::mutate(
      slash_column = stringr::str_replace(id, "[^\\/]*$", ""),
      id_column = stringr::str_extract(id, "[^\\/]*$"),
      id_column_prefix = stringr::str_extract(
        id_column,
        "(^\\d{2,3}(?=\\.))|(^\\d{2,3}(?=-\\d{2,3}$))"
      ),
      id_column_prefix = as.integer(id_column_prefix),
      id_count = stringr::str_detect(
        id_column,
        valid_patterns
      )
    ) %>%
    dplyr::filter(id_count == TRUE) %>%
    dplyr::filter(id_column_prefix >= 29)



  invalid_ids <- all_ids[which(all_ids$unique_id %notin% valid_ids$unique_id), ]


  # TODO Find another solution instead of simply dropping the missing IDs

  header_data_corrected <-
    complete_data %>%
    dplyr::slice(-invalid_ids$unique_id) %>%
    dplyr::mutate(
      region_id = dplyr::if_else(
        stringr::str_detect(id, "\\/"),
        1,
        0
      ),
      id = stringr::str_extract(id, "[^\\/]*$"),
      .after = 1
    ) %>%
    tidyr::drop_na(id)

  return(header_data_corrected)

}
