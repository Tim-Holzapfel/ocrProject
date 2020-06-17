
#' @title Base Frame
#'
#' @description Creating the base data frame
#'
#' @keywords internal
create_base_frame <-
  function() {
    df <-
      finalize_sheet() %>%
      dplyr::mutate(
        Death = dplyr::if_else(
          stringr::str_detect(Ruler, "†"), 1, 0, missing = 0
        ),
        Murdered = dplyr::if_else(
          stringr::str_detect(Ruler, "‡"), 1, 0, missing = 0
        )
      )

    # subset of data frame of IDs with three digits (ruler)

    index <-
      stringr::str_which(df$ID, "^[0-9][0-9][0-9]$")

    df_sub <-
      df %>%
      dplyr::slice(index) %>%
      dplyr::select("ID", "Period", "Ruler", "N", "Death", "Murdered") %>%
      dplyr::mutate(
        ID = as.integer(ID)
      ) %>%
      tidyr::drop_na(Ruler)

    return(df_sub)
  }



#' @title Death year
#'
#' @description Generating the death year
#'
#' @importFrom stats setNames
#'
#' @export
calculate_death_year <-
  function() {
    df_sub <-
      create_base_frame()

    # regular expression to test for the existence of at least a
    # three or four digit number surrounded by opening and closing brackets.
    # The first expression specifies cases in which the cross appeared before
    # the year and the second expression after the year.

    cross_before <-
      "(\\([^\\(\\)]*[†‡][^†‡\\(\\)]*\\d{3,4}[^\\)\\(]*\\))"

    cross_after <-
      "(\\([^\\(\\)]*\\d{3,4}[^†‡\\(\\)]*[†‡][^\\)\\(]*\\))"

    cross_both <- paste(cross_before, cross_after, sep = "|")


    # substep makes it possible to make sure that all relevant
    # cases containing a cross were detected


    df_sub <- df_sub %>%
      dplyr::mutate(
        cross = stringi::stri_extract_first_regex(Ruler, cross_both)
      )

    df_double <- tidyr::drop_na(df_sub, cross)

    # The month march, which written in Truhart style using roman
    # numerals as III, was often recognized as 111.

    df_double$cross <-
      df_double$cross %>%
      stringr::str_replace("([0-9])\\.(111)\\.([0-9])", "\\1\\.III\\.\\3")

    # Splitting the variable deathyear by single and double cross.
    # If a time period was given, meaning if Truhart specified
    # a birth year and a death year then those are usually separated by a cross.
    # If the left side of the string contains no
    # year then only the death year was given.

    df_split <-
      stringr::str_split(
        string = df_double$cross,
        pattern = "(†)|(‡)",
        simplify = TRUE,
        n = 2
      ) %>%
      tibble::as_tibble(.name_repair = "universal") %>%
      dplyr::select(
        Birthyear = ...1,
        Deathyear = ...2
      )

    # TODO Currently the functions do not account for for estimates of the form:
    # (1780/ or 1810). This should be fixed in the future.

    deathyear <-
      df_split["Deathyear"] %>%
      dplyr::mutate(
        base_year = stringi::stri_extract_last_regex(Deathyear, "\\d{3,4}"),
        est_year = stringr::str_extract(Deathyear, "(?<=\\d{3,4}\\/)\\d+"),
        est_count = stringi::stri_count_regex(Deathyear, "\\d{3,4}"),
        length_est = stringr::str_count(est_year),
        deathyear_u = stringi::stri_sub_replace(
          base_year,
          from = -length_est,
          replacement = est_year
        ) %>% as.integer(),
        deathyear_l = base_year %>% as.integer(),
        deathyear_u =
          ifelse(
            is.na(deathyear_u),
            deathyear_l,
            deathyear_u
          )
      ) %>%
      dplyr::select(deathyear_l, deathyear_u)


    birthyear <-
      df_split["Birthyear"] %>%
      dplyr::mutate(
        base_year = stringi::stri_extract_last_regex(Birthyear, "\\d{3,4}"),
        est_year = stringr::str_extract(Birthyear, "(?<=\\d{3,4}\\/)\\d+"),
        est_count = stringi::stri_count_regex(Birthyear, "\\d{3,4}"),
        length_est = stringr::str_count(est_year),
        birthyear_u = stringi::stri_sub_replace(
          base_year,
          from = -length_est,
          replacement = est_year
        ) %>% as.integer(),
        birthyear_l = base_year %>% as.integer(),
        birthyear_u =
          ifelse(
            is.na(birthyear_u),
            birthyear_l,
            birthyear_u
          )
      ) %>%
      dplyr::select(birthyear_l, birthyear_u)



    df_final <- cbind(df_double, birthyear, deathyear)

    return(df_final)
  }






