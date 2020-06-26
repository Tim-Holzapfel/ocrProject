

admin_environ <- new.env(parent = emptyenv())

#' Unique Index range
#'
#' @description Function to determine and return the unique index level of
#'   all administrative ares
#'
gen_unique_index_range <- function() {
  for (j in 1:5) {

    # For level 5, level 4 and level 3

    if (j %in% c(3, 4, 5)) {

      # Call gen_level_admin_id function and assign the dataset returned by that
      # function to the variable "level".

      level <- do.call(paste("gen_level", j, "_admin_id", sep = ""), list())


      # Find overlapping IDs and

      i <- 1
      repeat {
        index <-
          which(
            (level$id_start >= dplyr::lag(level$id_start)) &
              (level$id_end <= dplyr::lag(level$id_end)) &
              (level$id_group == dplyr::lag(level$id_group))
          )

        # Stop iteration if there are no overlapping values left

        if (rlang::is_empty(index)) break

        # Binding the subset of the data to the environment with the name
        # "admin_environ"

        admin_environ[[paste0("level", j, i)]] <- level[-index, ]

        level <- level[index, ]

        i <- i + 1
      }
    } else if (j == 2) {

      # For level 2

      level <- gen_level2_admin_id()

      i <- 1
      repeat {
        index <-
          which(
            (level$id_start >= dplyr::lag(level$id_start)) &
              (level$id_end <= dplyr::lag(level$id_end))
          )

        if (rlang::is_empty(index)) break

        admin_environ[[paste0("level", j, i)]] <- level[-index, ]

        level <- level[index, ]

        i <- i + 1
      }
    } else if (j == 1) {

      # For level 1

      level <- gen_level1_admin_id()

      admin_environ[["level1"]] <- level
    }

    index_names <-
      rlang::env_names(admin_environ) %>%
      sort()
  }

  return(index_names)
}


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
assign_admin_to_ruler <- function() {
  group_id_dataset <- gen_group_id()

  ruler_data <- ruler_subset(group_id_dataset)

  admin_names <- gen_unique_index_range()

  for (j in admin_names) {

    # admin_environ = name of the environment where the data sets are stored and
    # j is the name of the individual data sets

    admin_data <- rlang::env_get(admin_environ, nm = j) # nm = name of dataset

    # Determining whether the current string represents level 5, level 4, level
    # 3, level 2 or level 1

    admin_level <-
      stringr::str_extract(j, "(?<=^level)\\d{1}") %>%
      as.integer()

    # Again, as was the case before, level 2 and level 1 have to be treated
    # differently than level 5, level 4 and level 3 in terms of mathematical
    # operations

    if (admin_level %in% c(3, 4, 5)) {

      # Replacing the retrieved level index with the scalar that is required in
      # order to manipulate the data table

      # Next the ids of the ruler needs to be adjusted. Remember that for level
      # 5, level 4 and level 3 the combination of id group and individual ids is
      # important! So within the id groups a administrative area is assigned to
      # a ruler only if the id of the ruler is either bigger or equal to
      # admin_id_start (the starting id of the admin level) and smaller or equal
      # to admin_id_end (the ending id of the admin level)

      ruler_id <-
        dplyr::case_when(
          admin_level == 5 ~ 3,
          admin_level == 4 ~ 2,
          admin_level == 3 ~ 1
        ) %>%
        substr(ruler_data$id, 1, .) %>%
        as.integer()

      # Variable to make sure that the id group of the ruler is the same as that
      # of the administrative area

      ruler_id_group <- ruler_data$id_group

      # Same as above but only for the id group

      admin_id_group <- admin_data$id_group

      # Lower bound for the ruler id. Thus, the ruler id needs to be at least
      # as big or bigger than this value in order for the administrative area
      # to be assigned

      admin_id_start <- admin_data$id_start

      # Upper bound for the ruler id. Thus, the ruler id needs to smaller or
      # equal to this value.

      admin_id_end <- admin_data$id_end

      for (i in 1:dim(admin_data)[1]) {
        index <-
          dplyr::between(
            ruler_id,
            left = admin_id_start[i],
            right = admin_id_end[i]
          ) & (
            ruler_id_group == admin_id_group[i]
          )

        ruler_data[index, j] <- admin_data[i, "ruler"]
      }

      # Continuing with administrative level 2
    } else if (admin_level == 2) {

      # Same as before with the difference that for administrative level 2 the
      # only thing important is the id group of the ruler and the id of the
      # administrative level. This is because for level 2 id group and id are
      # the same.

      ruler_id_group <- ruler_data$id_group

      # This here actually is not the individual id as before but rather the id
      # group that is being created here.

      admin_id_start <- admin_data$id_start

      # Again this here is actually the id group end that is being created here

      admin_id_end <- admin_data$id_end

      # Because id and id group are the same for administrative level 2 it is
      # not necessary to explicitly control for the id group as it was the case
      # before.

      for (i in 1:dim(admin_data)[1]) {
        index <-
          dplyr::between(
            ruler_id_group,
            left = admin_id_start[i],
            right = admin_id_end[i]
          )

        ruler_data[index, j] <- admin_data[i, "ruler"]
      }

      # Continuing with administrative level 1
    } else if (admin_level == 2) {

      # Same as before with the difference that for administrative level 1 the
      # only thing important is the id group of the ruler and the id of the
      # administrative level. This is because for level 2 id group and id are
      # the same.

      ruler_id_group <- ruler_data$id_group

      # Because the id of administrative level 1 has no range (all ids are
      # individual, distinct values) the admin_id_start and admin_id_end are the
      # same value.

      admin_id_start <- admin_data$id

      admin_id_end <- admin_data$id

      for (i in 1:dim(admin_data)[1]) {
        index <-
          dplyr::between(
            ruler_id_group,
            left = admin_id_start[i],
            right = admin_id_end[i]
          )

        ruler_data[index, j] <- admin_data[i, "ruler"]
      }
    }
  }

  return(ruler_data)
}
