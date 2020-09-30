base_dataset_environ <- new.env(parent = emptyenv())

#' Creating Base dataframe
#'
#' @description Dataframe that will be used as the "building foundation" for all
#'   the following data cleaning functions.
#'
#' @return input_data
#'
gen_base_dataset <- function() {

  # Only run function if the dataset inside the environment
  # "base_dataset_environ" does not exists. Else skip the computation part to
  # save time.

  if (exists("base_dataset", where = base_dataset_environ) == FALSE) {


    # Creating a list of all the available excel files. Important! The rows of
    # the overview function "overview_data" need to be ordered

    overview_data <- gen_overview()

    input_list <- list(
      input_path = overview_data[["excel_file"]],
      input_continent = overview_data[["continent"]],
      input_continent_region = overview_data[["continent_region"]],
      input_startpage = overview_data[["startpage"]],
      input_endpage = overview_data[["endpage"]]
    )

    # Concatenating the individual sheets to one big R file. Furthermore,
    # performing very important! data cleaning operations. The first "mutate()"
    # step removes all leading, trailing and repeated white spaces from all
    # variables of type string.

    base_dataset_init <-
      furrr::future_pmap_dfr(input_list, read_excel_files) %>%
      string_squish() %>%
      dplyr::mutate(
        ruler = gsub("I11", "III", ruler),
        ruler = stringr::str_replace_all(ruler, "(I\\s?)(\\d{3,4})", "1\\2"),
        ruler = stringr::str_replace_all(ruler, "(l\\s?)(\\d{3,4})", "1\\2"),
        N = dplyr::row_number()
      )


    # This function takes a roman numeral as argument, replaces it with an
    # Arabic number and finally returns the month the roman numeral represented
    # based on the Arabic number

    roman_to_alpha <- function(x) month.abb[roman2numeric(x)]

    # Truhart had a rather specific style when it comes to writing dates as
    # Truhart used roman numerals to specify the month. This can be very
    # confusing for people not familiar with the style of Truhart and there is
    # also a lot of potential for misunderstandings along the way. To mitigate
    # this I propose to replace all roman numerals representing months (it is
    # important not to replace all roman numerals as roman numerals part of a
    # regents title e.g. "so-and-so the third" should be left as is).

    # To implement the proposed step from above it is important to first remove
    # the misplaced white space that was sometimes included between the month
    # and the year e.g. III. 1590 instead of the correct III.1590. Afterwards
    # the roman numeral representing a month is replaced with the abbreviated
    # month.

    base_dataset_final <-
      base_dataset_init %>%
      # Remove leading, trailing and consecutive white spaces.
      string_squish() %>%
      dplyr::mutate(
        period = stringr::str_replace_all(
          period,
          "(\\s?l\\s?)([0-9][0-9][0-9])",
          "1\\2"
        ),
        ruler = stringr::str_replace_all(
          ruler,
          "([XVI]+\\.)(\\s)(\\d{2,4})",
          "\\1\\3"
        ),
        ruler = stringr::str_replace_all(
          ruler,
          "([XVI]+(?=\\.\\s?\\d{2,4}))",
          roman_to_alpha
        ),
        ruler = stringi::stri_replace_first_regex(
          ruler, # removing white space in: 1 1771
          "(?:\\s)(\\d{1})(?:\\s)(\\d{3})",
          "$1$2"
        ),
        # replace all special, possibly language dependent character with their
        # ascii representation
        ruler = stringi::stri_trans_general(ruler, "latin-ascii")
      )

    rlang::env_bind(base_dataset_environ, base_dataset = base_dataset_final)
  }

  base_dataset <- base_dataset_environ$base_dataset

  return(base_dataset)
}
