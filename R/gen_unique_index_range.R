

admin_environ <- new.env(parent = emptyenv())

#' Unique Index range
#'
#' @description Function to determine and return the unique index level of
#'   all administrative ares
#'
#' @export
#'
gen_unique_index_range <- function() {
  for (j in 1:5) {

    # For level 5, level 4 and level 3

    if (j %in% c(3, 4, 5)) {

      # Call gen_level_admin_id function and assign the dataset returned by that
      # function to the variable "level".

      level <- do.call(paste0("gen_level", j, "_admin_id"), list())


      # Find overlapping IDs and

      i <- 1
      repeat {
        index <-
          which(
            (level$admin_id_start >= dplyr::lag(level$admin_id_start)) &
              (level$admin_id_end <= dplyr::lag(level$admin_id_end)) &
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
            (level$admin_id_start >= dplyr::lag(level$admin_id_start)) &
              (level$admin_id_end <= dplyr::lag(level$admin_id_end))
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
