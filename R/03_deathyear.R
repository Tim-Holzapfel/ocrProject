
#' death year
#'
#' @description Generating the death year
#'
#' @importFrom rlang .data
#'
#' @export
#'
gen_deathyear <- function() {

  # Regular Expression pattern to locate the death year. It looks for the
  # presence of three or four digits number (year) that comes after an opening
  # bracket. The opening bracket can be followed by any number as long as it is
  # not year. Important: the regular expression should not involve a closing
  # bracket at the end because that would exclude birth years where the death
  # year was unknown as those cases usually follow a pattern similar to this:
  # "(name of the city 1460-".

  base_dataset <- gen_base_dataset()

  year_loc <-
    stringr::regex("
                     \\(           # Opening Brackets
                     [^\\(\\)]*    # Unspecified number of non opening
                                   # or closing brackets
                     \\d{3,4}      # three or four digit number
                     [^\\(\\)]*    # Non-opening or closing brackets
                     ", comments = TRUE)

  # Using Unicode to identify rows that contain either a normal cross
  # (indicating that the death year of the ruler is at least approximately
  # known) or a double-cross (also known as the cross of Lorraine, indicating
  # that the ruler was assassinated). \U2020 is the Unicode string for the
  # normal cross and \U2021 is the Unicode string for the double cross.

  deathyear_full <-
    base_dataset %>%
    dplyr::mutate(
      death = dplyr::if_else(
        stringr::str_detect(ruler, "\U2020"), 1, 0,
        missing = 0
      ),
      murdered = dplyr::if_else(
        stringr::str_detect(ruler, "\U2021"), 1, 0,
        missing = 0
      ),
      .after = references
    )

  # Constructing the "building frame" by creating the "base year". Base year
  # in this sense means the substring of the variable ruler which contains the
  # the birth year and/or the death year of the ruler. This variable, which is
  # named year_base, will then be used to create the upper and lower bound of
  # the birth year and death year variables. "year_l" (for year lower) is the
  # year_base variable in which the years that contained an approximation in
  # the form of a forward slash "/" have been cut out.

  death_year_sub <-
    deathyear_full %>%
    ruler_subset() %>%
    # subset of data frame containing ruler
    dplyr::filter(
      stringr::str_detect(ruler, year_loc) == TRUE
    ) %>%
    dplyr::mutate(
      year_base = stringi::stri_extract_last_regex(
        ruler,
        year_loc,
        comments = TRUE
      ), # initial string
      year_l = stringr::str_replace_all(
        year_base, # lower estimate bound
        "(\\d{3,4})(\\/\\d{1,4})",
        "\\1"
      ),
      .after = references
    )

  # Est contains the estimated year that was cut away from the variable
  # "year_l". That means if the year was approximated in the form of 1576/77
  # then "year_l" only contains the "root", meaning the base year without the
  # approximation, in this case 1576 and the variable "est" then only contains
  # the approximation, in this case the string 77.

  est <-
    stringr::str_extract(death_year_sub$year_base, "(?<=\\d{3,4}\\/)\\d+")

  # This variable, est_pos, is an auxiliary variable that is used to locate
  # the position or to determine the location of the approximated year. More
  # specifically, this variable only locates the position of the forward slash
  # "/" with the base year (the "root") being inside a lookaround.

  est_pos <-
    stringr::str_locate(
      death_year_sub$year_base,
      "(?<=\\d{3,4})\\/\\d{1,4}"
    )

  # Because the variable "est_pos" only locates the position of the forward
  # slash "/" of the approximated year, it (the variable "est_pos") needs to
  # be corrected so that it correctly points to or reports the position of the
  # approximated year that actually needs to be substituted. But because the
  # "approximation range", meaning whether the approximation consists of one,
  # two, three or four digits, is also a variable, the amount by which
  # "est_pos" needs to be corrected varies.

  est_pos_cor <- est_pos - stringr::str_length(est)

  # This step creates the variable "year_u" (year upper). One thing that is
  # very important to mention at this point is that, even though the function
  # "stri_sub_replace" from the "stringi" package can be very useful, the
  # function description is very misleading. "from" in "stri_sub_replace" is
  # actually expected to contain BOTH the starting position AND the ending
  # position of the replacement.

  death_year_sub$year_u <-
    stringi::stri_sub_replace(
      death_year_sub$year_l,
      from = est_pos_cor,
      replacement = est
    )

  # Using the variables created before to calculate the age and to a binary
  # variable indicating whether or not the birth year was known

  death_year_sub_age <-
    death_year_sub %>%
    dplyr::mutate(
      year_u = dplyr::if_else(is.na(year_u), year_l, year_u),
      birthyear_l =
        stringr::str_extract(year_l, "\\d{3,4}(?=\\s?-)") %>%
          as.integer(),
      birthyear_u =
        stringr::str_extract(year_u, "\\d{3,4}(?=\\s?-)") %>%
          as.integer(),
      year_l =
        stringr::str_remove(year_l, "\\d{3,4}(?=\\s?-)"),
      year_u =
        stringr::str_remove(year_u, "\\d{3,4}(?=\\s?-)"),
      deathyear_l = stringi::stri_extract_last_regex(year_l, "\\d{3,4}") %>%
        as.integer(),
      deathyear_u = stringi::stri_extract_last_regex(year_u, "\\d{3,4}") %>%
        as.integer(),
      age_l = deathyear_l - birthyear_l,
      age_u = deathyear_u - birthyear_u,
      .after = references
    )


  # Generating control data set to see which ages are negative

  control_data_negative_age <-
    death_year_sub_age %>%
    dplyr::filter(
      age_l < 0
    )

  death_year_final <-
    death_year_sub_age %>%
    dplyr::filter(age_l > 0 | is.na(age_l)) %>% # a negative age is a computation error
    dplyr::select(-c("year_base", "year_u", "year_l")) # removing aux variables

  # Subset of the "death_year_final" dataset that only contains observations
  # that specified either a birth -or a death year for the ruler.

  df_final_full <-
    death_year_final %>%
    dplyr::select(
      birthyear_l,
      birthyear_u,
      deathyear_l,
      deathyear_u,
      N,
      age_l,
      age_u
    ) %>%
    dplyr::full_join(deathyear_full, ., by = "N") %>%
    dplyr::mutate(
      birthyear_known =
        dplyr::case_when(
          stringi::stri_detect_regex(id, "^\\d{3}$", negate = TRUE) ~ as.integer(NA),
          !is.na(birthyear_l) ~ as.integer(1),
          TRUE ~ as.integer(0)
        )
    )

  return(df_final_full)
}


# options(enhancedView.standard_view = FALSE)



# death_place <-
#   stringr::str_split(
#     death_year_sub$year_base,
#     "(?<=\\d{2,4})-",
#     simplify = TRUE,
#     n = 2
#     ) %>%
#   tibble::as_tibble(.name_repair = "universal") %>%
#   dplyr::select(birth_year = ...1, death_year = ...2) %>%
#   dplyr::mutate(
#     birth_year_test = gsub("n\\.", "", birth_year),
#     birth_year_test = gsub("c\\.", "", birth_year_test),
#     birth_year_test = gsub("p\\.", "", birth_year_test),
#     birth_place = stringr::str_extract(birth_year, "[A-z\\-\\/ ]+")
#   )




# This last step now creates the final dataframe.
#
# death_year_final <-
#   death_year_sub %>%
#   dplyr::mutate(
#     year_u = dplyr::if_else(is.na(year_u), year_l, year_u),
#     birthyear_l =
#       stringr::str_extract(year_l, "\\d{3,4}(?=\\s?-)") %>%
#       as.integer(),
#     birthyear_u =
#       stringr::str_extract(year_u, "\\d{3,4}(?=\\s?-)") %>%
#       as.integer(),
#     year_l =
#       stringr::str_remove(year_l, "\\d{3,4}(?=\\s?-)"),
#     year_u =
#       stringr::str_remove(year_u, "\\d{3,4}(?=\\s?-)"),
#     deathyear_l = stringi::stri_extract_last_regex(year_l, "\\d{3,4}") %>%
#       as.integer(),
#     deathyear_u = stringi::stri_extract_last_regex(year_u, "\\d{3,4}") %>%
#       as.integer(),
#     age_l = deathyear_l - birthyear_l,
#     age_u = deathyear_u - birthyear_u,
#     birthyear_known =
#       .after = references
#   ) %>%
#   dplyr::filter(age_l > 0 | is.na(age_l)) %>%
#   dplyr::select(-c("year_base", "year_u", "year_l")) # removing aux variables
#
