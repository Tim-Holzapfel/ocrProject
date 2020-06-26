#' @title Level 5 Index
#'
#' @description Creating index for all observations at the highest level
#'
gen_level5_admin_id <- function() {

  # TODO There are some ids that belong to level 5, start with three digits but
  # end with two like 63.897-91 This must be fixed!!

  group_id_dataset <- gen_group_id()

  level5_pattern <-
    stringr::regex("
      (^\\d{2,4}\\.\\d{3}$)         # Either Only three digits
        |                           # OR
      (^\\d{2,4}\\.\\d{3}-\\d{3}$)  # Three digits followed by three digits
      ", comments = TRUE)

  # id_end cannot be empty, if it is, it's being replaced by id_start

  level5 <-
    group_id_dataset %>%
    dplyr::filter(
      stringr::str_detect(id, level5_pattern) == TRUE
    ) %>%
    dplyr::mutate(
      id = gsub(" ", "", id), # id can't contain empty spaces except headers!
      id_start = stringr::str_extract(id, "(?<=^\\d{2,4}\\.)\\d{3}") %>%
        as.numeric(),
      id_end = stringr::str_extract(id, "(?<=^\\d{2,4}\\.\\d{3}-)\\d{3}$") %>%
        as.numeric(),
      id_end = dplyr::if_else(is.na(id_end), id_start, id_end),
      range = id_end - id_start
    ) %>%
    dplyr::arrange(id_group, id_start, -range) %>%
    dplyr::relocate(id, id_group, id_start, id_end, range)

  }


#' @title Level 4 Index
#'
#' @description Creating index for all level 4 observations
#'
gen_level4_admin_id <- function() {
  group_id_dataset <- gen_group_id()

  level4_pattern <-
    stringr::regex("
      (^\\d{2,4}\\.\\d{2}$)         # Either Only two digits
        |                           # OR
      (^\\d{2,4}\\.\\d{2}-\\d{2}$)  # two digits followed by two digits
      ", comments = TRUE)

  level4 <-
    group_id_dataset %>%
    dplyr::filter(
      stringr::str_detect(id, level4_pattern) == TRUE
    ) %>%
    dplyr::mutate(
      id = gsub(" ", "", id), # id can't contain empty spaces except headers!
      id_start = stringr::str_extract(id, "(?<=^\\d{2,4}\\.)\\d{2}") %>%
        as.integer(),
      id_end = stringr::str_extract(id, "(?<=^\\d{2,4}\\.\\d{2}-)\\d{2}$") %>%
        as.integer(),
      id_end = dplyr::if_else(is.na(id_end), id_start, id_end),
      range = id_end - id_start
    ) %>%
    dplyr::arrange(id_group, id_start, -range) %>%
    dplyr::relocate(id, id_group, id_start, id_end, range)
}


#' @title Level 3 Index
#'
#' @description Creating index for all level 3 observations
#'
gen_level3_admin_id <- function() {
  group_id_dataset <- gen_group_id()

  level3_pattern <-
    stringr::regex("
      (^\\d{2,4}\\.\\d{1}$)         # Either Only two digit
        |                           # OR
      (^\\d{2,4}\\.\\d{1}-\\d{1}$)  # one digit followed by one digit
      ", comments = TRUE)

  level3 <-
    group_id_dataset %>%
    dplyr::filter(
      stringr::str_detect(id, level3_pattern) == TRUE
    ) %>%
    dplyr::mutate(
      id = gsub(" ", "", id), # id can't contain empty spaces except headers!
      id_group = stringr::str_extract(id, "\\d{2,4}(?=\\.\\d{1})") %>%
        as.integer(),
      id_start = stringr::str_extract(id, "(?<=^\\d{2,4}\\.)\\d{1}") %>%
        as.integer(),
      id_end = stringr::str_extract(id, "(?<=^\\d{2,4}\\.\\d{1}-)\\d{1}$") %>%
        as.integer(),
      id_end = dplyr::if_else(is.na(id_end), id_start, id_end),
      range = id_end - id_start
    ) %>%
    dplyr::arrange(id_group, id_start, -range) %>%
    dplyr::relocate(id, id_group, id_start, id_end, range)

  return(level3)
}

#' @title Index Level 2 Function
#'
#' @description Creates Index level 2
#'
gen_level2_admin_id <- function() {

  # group_id_dataset is obtained from the "gen_group_id()" function

  group_id_dataset <- gen_group_id()

  level2_pattern <-
    stringr::regex("
    (
      ^\\d{1}-\\d{1}$      # One Digit before and after the minus
    )
      |
    (
      ^\\d{2}-\\d{2}$      # Two Digit before and after the minus
    )
      |
    (
      ^\\d{3}-\\d{3}$      # Three Digits before and after the minus
    )", comments = TRUE)

  level2_init <-
    group_id_dataset %>%
    dplyr::filter(
      stringr::str_detect(id, level2_pattern) == TRUE
    )


  id_range <-
    stringr::str_split_fixed(level2_init$id, "-", n = 2) %>%
    data.table::as.data.table() %>%
    dplyr::rename(id_start = V1, id_end = V2) %>%
    dplyr::mutate(
      id_start = as.integer(id_start),
      id_end = as.integer(id_end),
      range = id_end - id_start
    )

  # Important to reiterate: level 2 doesn't have an id group as the id itself
  # is the id group! The id group should therefore be dropped.

  level2 <-
    cbind(level2_init, id_range) %>%
    dplyr::select(-"id_group") %>%
    dplyr::arrange(id_start, -range)
}


#' @title Index level 1 Function
#'
#' @description Creates Index Level 1
#'
gen_level1_admin_id <- function() {
  group_id_dataset <- gen_group_id()

  # As is the case for level 2, level 1 also does not have an id group because
  # the ids of level 1 are already the id groups

  level1 <- gen_group_id() %>%
    dplyr::filter(
      stringr::str_detect(id, "^\\d{2,4}\\.$") == TRUE
    )
}
