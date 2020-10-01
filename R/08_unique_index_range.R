
admin_environ <- new.env(parent = emptyenv())

#' Unique Index range
#'
#' @description Function to determine and return the unique index level of
#'   all administrative ares
#'
#' @export
#'
gen_unique_index_range <- function() {
  requireNamespace("data.table")

  for (j in 1:5) {

    # For level 5, level 4 and level 3

    if (j %in% c(3, 4, 5)) {

      # Call gen_level_admin_id function and assign the dataset returned by that
      # function to the variable "level".

      level <- do.call(paste0("gen_level", j, "_admin_id"), list()) %>%
        dplyr::arrange(admin_id_start, admin_id_end) %>%
        dplyr::filter(admin_id_start <= admin_id_end)

      data.table::setDT(level)

      # Find overlapping IDs and

      # To create as few "sublevels" as possible, it is important to remove
      # linear combinations of xid and yid. Linear combinations in this case
      # means
      #
      # xid | yid
      # ---------
      # 5   | 1
      # 1   | 5
      #
      # To identify linear combinations per group it is very important not only
      # to create a variable that holds the linear combinations but also one
      # that hold the inverse linear combination, because two id groups can have
      # the same integer value for overlapping ranges. To uniquely identify the
      # right linear combination per id group, it is therefore necessary to
      # create the linear combination and the inverse linear combination.

      i <- 1
      repeat {
        data.table::setkey(level, id_group, admin_id_start, admin_id_end)
        level_overlap <- data.table::foverlaps(level, level, which = TRUE) %>%
          dplyr::filter(xid != yid) %>%
          dplyr::mutate(
            lin_com = xid + yid,
            lin_com_inv = abs(xid - yid)
          ) %>%
          dplyr::distinct(lin_com, lin_com_inv, .keep_all = TRUE)

        overlap_index <- level_overlap$xid %>% unique()

        # Stop iteration if there are no overlapping values left

        if (rlang::is_empty(overlap_index)) break

        # Binding the subset of the data to the environment with the name
        # "admin_environ"

        admin_environ[[paste0("level", j, i)]] <- level[-overlap_index, ]

        level <- level[overlap_index, ]

        i <- i + 1
      }
    } else if (j == 2) {

      # For level 2

      level <- gen_level2_admin_id() %>%
        dplyr::arrange(admin_id_start, admin_id_end) %>%
        dplyr::filter(admin_id_start <= admin_id_end)

      data.table::setDT(level)

      i <- 1
      repeat {
        data.table::setkey(level, admin_id_start, admin_id_end)
        level_overlap <- data.table::foverlaps(level, level, which = TRUE) %>%
          dplyr::filter(xid != yid) %>%
          dplyr::mutate(
            lin_com = xid + yid,
            lin_com_inv = abs(xid - yid)
          ) %>%
          dplyr::distinct(lin_com, lin_com_inv, .keep_all = TRUE)

        overlap_index <- level_overlap$xid %>% unique()

        if (rlang::is_empty(overlap_index)) break

        admin_environ[[paste0("level", j, i)]] <- level[-overlap_index, ]

        level <- level[overlap_index, ]

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






# clear()
#
# level <- gen_level4_admin_id() %>% dplyr::select(
#   admin_region, admin_region, admin_id_start, admin_id_end, range,
#   id, id_group
# ) %>%
#   dplyr::arrange(admin_id_start, admin_id_end) %>%
#   dplyr::filter(admin_id_start <= admin_id_end)
#
# data.table::setDT(level)
#
# data.table::setkey(level, id_group, admin_id_start, admin_id_end)
#
# # level_id_groups <- level %>% dplyr::select(id_group) %>% dplyr::group_by(id_group) %>%
# #   dplyr::mutate(id = dplyr::cur_group_id())
#
# level_overlap <- data.table::foverlaps(level, level, which = TRUE) %>%
#   dplyr::filter(xid != yid) %>%
#   dplyr::mutate(
#     lin_com = xid + yid,
#     lin_com_inv = abs(xid - yid)
#   ) %>%
#   dplyr::distinct(lin_com, lin_com_inv, .keep_all = TRUE)
#
#
# overlap_index <- level_overlap$xid %>% unique()
#
# level_clear <- level %>% dplyr::slice(-overlap_index)
#
# level_not_clear <- level %>% dplyr::slice(overlap_index)
#
