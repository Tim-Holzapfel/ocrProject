
#' @title Death year
#'
#' @description Generating the death year
#'
#' @importFrom stats setNames
#' @import stringr
#' @import stringi
#' @import dplyr
#' @import tidyr
#'
#' @export
calculate_death_year <-
  function() {
    df <-
      finalize_sheet() %>%
      mutate(
        Death = if_else(
          str_detect(Ruler, "\U2020"), 1, 0, missing = 0
        ),
        Murdered = if_else(
          str_detect(Ruler, "\U2021"), 1, 0,
          missing = 0
        )
      )

    # subset of data frame of IDs with three digits (ruler)

    index_ruler <-
      str_which(df$ID, "^[0-9][0-9][0-9]$")



    df_sub <-
      df %>%
      slice(index_ruler) %>%
      select("ID", "Period", "Ruler", "N") %>%
      mutate(
        ID = as.integer(ID)
      ) %>%
      drop_na(Ruler)

    # ABBYY, the text recognition program sometimes makes the mistake that it
    # takes the number 1 for the small letter L. This next step corrects for
    # that mistake

    df_sub <-
      df_sub %>%
      mutate(
        Ruler = gsub("I11", "III", Ruler),
        Ruler = str_replace_all(Ruler, "(I\\s?)(\\d{3,4})", "1\\2"),
        Ruler = str_replace_all(Ruler, "(l\\s?)(\\d{3,4})", "1\\2")
      )

    # Regular Expression pattern to locate the death year. It looks for the
    # presence of three or four digits number (year) that comes after an opening
    # bracket. The opening bracket can be followed by any number as long as it
    # is not year.

    year_loc <-
      regex("
                     \\(           # Opening Brackets
                     [^\\(\\)]*    # Unspecified number of non opening
                                   # or closing brackets
                     \\d{3,4}      # three or four digit number
                     [^\\(\\)]*    # Non-opening or closing brackets
                     ", comments = TRUE)


    # Index of the observation that contain the pattern described by year_loc

    year_index <- stringr::str_which(df_sub$Ruler, year_loc)

    # Constructing the "building frame" by creating the "base year". Base year
    # in this sense means the substring of the variable Ruler which contains the
    # the birth year and/or the death year of the Ruler. This variable, which is
    # named year_base, will then be used to create the upper and lower bound of
    # the birth year and death year variables. "year_l" (for year lower) is the
    # year_base variable in which the years that contained an approximation in
    # the form of a forward slash "/" have been cut out.

    df_year <-
      df_sub %>%
      slice(year_index) %>%
      mutate(
        year_base = str_extract(Ruler, year_loc), # initial string
        year_l = str_replace_all(
          year_base, # lower estimate bound
          "(\\d{3,4})(\\/\\d{1,4})",
          "\\1"
        )
      )

    # Est contains the estimated year that was cut away from the variable
    # "year_l". That means if the year was approximated in the form of 1576/77
    # then "year_l" only contains the "root", meaning the base year without the
    # approximation, in this case 1576 and the variable "est" then only contains
    # the approximation, in this case the string 77.

    est <- str_extract(df_year$year_base, "(?<=\\d{3,4}\\/)\\d+")

    # This variable, est_pos, is an auxiliary variable that is used to locate
    # the position or to determine the location of the approximated year. More
    # specifically, this variable only locates the position of the forward slash
    # "/" with the base year (the "root") being inside a lookaround.

    est_pos <- str_locate(
      df_year$year_base,
      "(?<=\\d{3,4})\\/\\d{1,4}"
    )

    # Because the variable "est_pos" only locates the position of the forward
    # slash "/" of the approximated year, it (the variable "est_pos") needs to
    # be corrected so that it correctly points to or reports the position of the
    # approximated year that actually needs to be substituted. But because the
    # "approximation range", meaning whether the approximation consists of one,
    # two, three or four digits, is also a variable, the amount by which
    # "est_pos" needs to be corrected varies.

    est_pos_cor <- est_pos - str_length(est)

    # This step creates the variable "year_u" (year upper). One thing that is
    # very important to mention at this point is that, even though the function
    # "stri_sub_replace" from the "stringi" package can be very useful, the
    # function description is very misleading. "from" in "stri_sub_replace" is
    # actually expected to contain BOTH the starting position AND the ending
    # position of the replacement.

    df_year$year_u <-
      stri_sub_replace(
        df_year$year_l,
        from = est_pos_cor,
        replacement = est
      )


    # This last step now creates the final dataframe.

    df_final <-
      df_year %>%
      mutate(
        year_u = if_else(is.na(year_u), year_l, year_u),
        birthyear_l = str_extract(year_l, "\\d{3,4}(?=\\s?-)") %>%
          as.integer(),
        birthyear_u = str_extract(year_u, "\\d{3,4}(?=\\s?-)") %>%
          as.integer(),
        year_l = str_remove(year_l, "\\d{3,4}(?=\\s?-)"),
        year_u = str_remove(year_u, "\\d{3,4}(?=\\s?-)"),
        deathyear_l = stri_extract_last_regex(year_l, "\\d{3,4}") %>%
          as.integer(),
        deathyear_u = stri_extract_last_regex(year_u, "\\d{3,4}") %>%
          as.integer()
      ) %>%
      select(-c("year_base", "year_u", "year_l"))



    # Subset of the "df_final" dataset that only contains observation that
    # specified either a birth -or a death year for the Ruler.

    df_final_full <-
      df_final %>%
      select(birthyear_l, birthyear_u, deathyear_l, deathyear_u, N) %>%
      full_join(df, ., by = "N")

    return(df_final_full)
  }
