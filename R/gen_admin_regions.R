#' Admin Assign
#'
#' @description The most computational expansive, this function assigns the
#'   individual territories/districts to the rulers.
#'
#' @details # General idea
#'
#'   The general idea is to compare two distinct data sets, in this case subsets
#'   of the original data set where the first subset consists only of the actual
#'   rulers and the second data set consists only of the administrative areas
#'   (so the second dataset is basically everything but the rulers)
#'
gen_admin_regions <- function() {
  base_dataset <- gen_group_id()

  ruler_data <-
    ruler_subset(base_dataset) %>%
    dplyr::mutate(
      unique_index = 1:dplyr::n()
    )

  # Assign names of index ranges (like "level51" and "level52") to the variable
  # admin names

  admin_names <- gen_unique_index_range()

  # Loop trough all unique index ranges and assign the admin regions to the
  # rulers

  for (j in admin_names) {

    # admin_id_end must always be at least as big as admin_id_start! Otherwise
    # foverlaps() will throw an error

    admin_data <-
      rlang::env_get(admin_environ, nm = j) %>%
      dplyr::filter(
        admin_id_end >= admin_id_start
      )

    # It is paramount that one distinguishes between the different id levels as
    # the assignment process is different for the different id levels.

    admin_level <-
      stringr::str_extract(j, "(?<=^level)\\d{1}") %>%
      as.integer()

    # Next the ids of the ruler needs to be adjusted. Remember that for level
    # 5, level 4 and level 3 the combination of id group and individual ids is
    # important! So within the id groups a administrative area is assigned to
    # a ruler only if the id of the ruler is either bigger or equal to
    # admin_id_start (the starting id of the admin level) and smaller or equal
    # to admin_id_end (the ending id of the admin level)

    if (admin_level %in% c(3, 4, 5)) {
      subset_level <-
        dplyr::case_when(
          admin_level == 5 ~ 3,
          admin_level == 4 ~ 2,
          admin_level == 3 ~ 1
        )

      ruler_id_subset <-
        ruler_data %>%
        dplyr::mutate(
          ruler_id = substr(id, 1, subset_level),
          ruler_id = as.integer(ruler_id),
          ruler_id_start = ruler_id,
          ruler_id_end = ruler_id,
          .after = id
        )

      data.table::setkey(admin_data, id_group, admin_id_start, admin_id_end)

      data.table::setkey(
        ruler_id_subset, id_group, ruler_id_start,
        ruler_id_end
      )

      # level1 and level2 themselves represent id_groups and thus have to be
      # treated differently than level5, level4 or level3. Furthermore, the keys
      # have to be set differently for level1 and level2 as the variable
      # "id_group" does not exist for them.
    } else if (admin_level %in% c(1, 2)) {
      ruler_id_subset <-
        ruler_data %>%
        dplyr::mutate(
          ruler_id = id,
          ruler_id_start = id_group,
          ruler_id_end = id_group,
          .after = id
        )

      data.table::setkey(admin_data, admin_id_start, admin_id_end)

      data.table::setkey(ruler_id_subset, ruler_id_start, ruler_id_end)
    }

    admin_overlap <-
      data.table::foverlaps(ruler_id_subset, admin_data) %>%
      dplyr::distinct(unique_index, .keep_all = TRUE) %>%
      dplyr::select(admin_region, unique_index)

    # The name of the created admin regions needs to be unique and thus gets
    # assigned the name of unique index range.

    names(admin_overlap)[1] <- j

    ruler_data <-
      dplyr::left_join(ruler_data, admin_overlap, by = "unique_index")
  }

  return(ruler_data)
}
