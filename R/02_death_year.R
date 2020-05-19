#' @import stringr
#' @import stringi
#' @import tidyr
#' @import dplyr
#' @importFrom magrittr %>%
#' @import tibble

rm(list = ls())


index <- new.env(parent = emptyenv())

#' Indicating death year
#'
#' Assigning a boolean variable to rulers who were either murdered or assinated
#'
#' @return
#' @export
#'
#' @examples
indicate_deathyear <-
  function() {

    load("R/sysdata.rda")

    df <-
      df %>%
      dplyr::mutate(
        N = 1:dim(df)[1],
        Death = 0,
        Murdered = 0
      )

    df[stringr::str_which(df$Ruler, "†"), "Death"] <- 1

    df[stringr::str_which(df$Ruler, "‡"), "Murdered"] <- 1

    return(df)
  }



#' subsetting to only include IDs with three digits
#'
#' @return
#' @export
#'
#' @examples
create_subset_df <-
  function() {

    df <-
      indicate_deathyear()

    # subset of dataframe of IDs with three digits (ruler)

    index <-
      stringr::str_which(df$ID, "^[0-9][0-9][0-9]$")

    df_sub <-
      df %>%
      dplyr::slice(index) %>%
      dplyr::select("ID", "Period", "Ruler", "N", "Death", "Murdered")

    df_sub$ID <-
      df_sub$ID %>%
      as.integer()

    df_sub <-
      tidyr::drop_na(df_sub, Ruler)

    return(df_sub)
  }



#' Calculating the death year
#'
#' @return
#' @export
#'
#' @examples
create_base_frame <-
  function() {
    df_sub <-
      create_subset_df()

    # regular expression to test for the existence of at least a three or four digit
    # number surrounded by opening and closing brackets. The first expression specifies cases
    # in which the cross appeared before the year and the second expression after the year.

    cross_before <-
      "(\\([^\\(\\)]*[†‡][^†‡\\(\\)]*\\d{3,4}[^\\)\\(]*\\))"

    cross_after <-
      "(\\([^\\(\\)]*\\d{3,4}[^†‡\\(\\)]*[†‡][^\\)\\(]*\\))"

    # substep makes it possible to make sure that all relevant cases containing a cross
    # where detected

    df_sub$cross <-
      df_sub$Ruler %>%
      stringi::stri_extract_first_regex(paste(cross_before, cross_after, sep = "|"))



    df_double <- df_sub %>% dplyr::slice(which(!is.na(df_sub$cross)))


    # The month march, which written in Truhart style using roman numerals as III, was often recognized as 111.

    df_double$cross <-
      df_double$cross %>%
      stringr::str_replace("([0-9])\\.(111)\\.([0-9])", "\\1\\.III\\.\\3")


    # Splitting the variable deathyear by single and double cross. If a time period was given, meaning if Truhart specified
    # a birthyear and a deathyer then those are usually seperated by a cross. If the left side of the string contains no
    # year then only the deathyear was given.

    df_split <-
      df_double$cross %>%
      stringi::stri_split_regex(
        "(†)|(‡)",
        simplify = TRUE,
        n = 2, # more than 2 splits should not occur
        tokens_only = TRUE
      ) %>%
      tibble::as_tibble() %>%
      dplyr::select(Birthyear = V1, Deathyear = V2)

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






































