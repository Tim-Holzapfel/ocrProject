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

    base_dataset <- gen_header()

    id_pattern <-
      stringr::regex("
         ^\\d{2,4}(?=\\.)
      ", comments = TRUE)

    # It's still not clear which significance and meaning (if any) the ids
    # with the pattern one digit, followed by three digits and a forward slash
    # (like 7.115/64.461-469) have. Therefore, for the time being, the prefix
    # that often starts with 7 and ends with a forward slash is removed.
    # Because the 7./ prefix is not always only composed of one forward slash
    # but sometimes two or three, the most robust approach is to identify the
    # substring that comes or starts after the last forward slash and to
    # extract that part. Because a missing value is returned whenever no 7./
    # prefix was found, a auxiliary variable needs to be created that first
    # extract the not 7./ part of the string (if it exists or if a 7./ was
    # present) and then replace the value of the newly created variable with
    # the id variable whenever the newly created variable had non-missing
    # entries.

    group_id_envir$group_id_data <-
      base_dataset %>%
      data.table::as.data.table() %>%
      dplyr::mutate(
        id2 = stringr::str_extract(id, "(?<=\\/)[^\\/]+$"),
        id = dplyr::if_else(
          is.na(id2), # if id2 is missing (id didn't contain a 7./ string)
          id, # id2 is missing: replace id with id (status unchanged)
          id2 # id2 is not missing: replace id with id2
        ),
        id2 = NULL # drop id2 at the end again
      ) %>%
      dplyr::mutate(
        id_group = stringr::str_extract(id, id_pattern),
        id_group = zoo::na.locf(id_group, na.rm = F),
        id_group = as.integer(id_group),
        .after = id
      )
  }

  group_id_data <- group_id_envir$group_id_data
}
