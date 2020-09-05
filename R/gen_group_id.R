group_id_envir <- new.env(parent = emptyenv())

#' Generate Group Identifier
#'
#' @description The group identifier is the most important part when assigning
#'   ruler to administrative regions as it links the individuals to the
#'   administrative regions
#'
#' @return group_id_dataset
#'
#' @export
#'
gen_group_id <- function() {

  # Only run function if the dataset inside the environment "group_id_envir"
  # does not exists. Else skip the computation part to save time.

  if (exists("group_id_data", where = group_id_envir) == FALSE) {

    # Pattern to assign the correct id to the ruler. The id group is assigned
    # either based on the ids starting with only two or three digits and are
    # followed by a point (like 63.11-12)

    base_dataset <- correcting_region_ids()

    group_id_envir$group_id_data <-
      base_dataset %>%
      dplyr::mutate(
        id_group = stringr::str_extract(id, "(^\\d{2,3}(?=\\.))"),
        id_group = zoo::na.locf(id_group, na.rm = F),
        id_group = as.integer(id_group),
        .after = id
      )
  }

  group_id_data <- group_id_envir$group_id_data
}
