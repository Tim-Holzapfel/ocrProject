
overview_environ <- new.env(parent = emptyenv())

#' Generating overview
#'
#' @description This function generates an overview of the available files
#'
#' @param input_path Path to the directory where the Truhart files are saved
#'
#' @author Tim Holzapfel
#'
#' @return overview_data
#'
#' @export
#'
gen_overview <- function(input_path = "D:/km/Truhart") {

  # Creating a list of all the available excel files. Important! The rows of the
  # overview function "excel_files" need to be ordered

  # To make the manual adjustment easier it makes sense to couple the
  # excel_files with their pdf counterpart so that the pdf files can be opened
  # faster. The excel and pdf files are joined using the fuzzy join method. To
  # make the strings as comparable as possible, another variable is created that
  # contains only the file name without the path and without the name of the
  # student who created it. The last part is because while most of the pdf files
  # have the name of their creator attached with an underscore this is not
  # always the case for excel-files. After the merge the two additional
  # variables can be dropped again.

  # Only run function if the dataset inside the environment
  # "overview_environ" does not exists. Else skip the computation part to
  # save time.

  if (exists("overview_data", where = overview_environ) == FALSE) {

    # List of available Excel files

    overview_excel <-
      list.files(
        path = input_path,
        pattern = ".xlsx$",
        recursive = TRUE,
        full.names = TRUE,
        include.dirs = TRUE,
        ignore.case = TRUE,
        all.files = FALSE
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        fuzzy_match = stringr::str_extract(
          value,
          "(?<=\\/)[^\\/]*(?=\\.xlsx$)"
        ),
        fuzzy_match = stringr::str_extract(
          fuzzy_match,
          "^[^\\_]*"
        )
      )

    # It's easier to allocate the individual continent_regions to the rulers
    # when the relevant information are stored inside the excel sheets as meta
    # data. The function "gen_meta_data" extracts the meta data from the excel
    # sheets and allocates them accordingly. The meta data of excel files are
    # stored in the description file of the excel file and can be extracted by
    # unzipping the excel file. Since the extracted files have to be stored
    # somewhere, it is convenient to store them in a temporary file that by its
    # nature gets deleted after the script has run.

    temp_dir <- tempdir()

    gen_meta_data <- function(input_path) {
      path <- input_path[1]

      doc <-
        XML::xmlInternalTreeParse(
          unzip(
            path,
            files = "docProps/core.xml",
            exdir = temp_dir
          )
        )

      meta_data <- XML::xmlValue(XML::getNodeSet(doc, "/*/dc:description"))
    }

    suppressMessages({
      excel_meta_data <-
        apply(overview_excel, 1, gen_meta_data) %>%
        stringr::str_split(";", simplify = TRUE) %>%
        tibble::as_tibble(.name_repair = "universal") %>%
        dplyr::rename(continent = ...1, continent_region = ...2) %>%
        dplyr::mutate(
          continent = stringr::str_extract(continent, "(?<=Continent:).*"),
          continent_region =
            stringr::str_extract(continent_region, "(?<=Region:).*")
        )
    })
    # List of available PDF files

    overview_pdf <-
      list.files(
        path = input_path,
        pattern = ".pdf$",
        recursive = TRUE,
        full.names = TRUE,
        include.dirs = TRUE,
        ignore.case = TRUE,
        all.files = FALSE
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        fuzzy_match = stringr::str_extract(value, "(?<=\\/)[^\\/]*(?=\\.pdf$)"),
        fuzzy_match = stringr::str_extract(fuzzy_match, "^[^\\_]*")
      ) %>%
      dplyr::filter(fuzzy_match != "Truhart")

    overview_environ$overview <-
      fuzzyjoin::stringdist_left_join(
        x = overview_excel,
        y = overview_pdf,
        by = "fuzzy_match",
        method = "osa",
        max_dist = 1
      ) %>%
      tibble::as_tibble() %>%
      dplyr::select(excel_file = value.x, pdf_file = value.y) %>%
      dplyr::mutate(
        continent = excel_meta_data$continent,
        continent_region = excel_meta_data$continent_region,
        startpage =
          as.integer(
            stringi::stri_extract_last_regex(
              excel_file,
              "[0-9]+(?=to|-)"
            )
          ),
        endpage =
          as.integer(
            stringi::stri_extract_last_regex(
              excel_file,
              "(?<=to|-)[0-9]+"
            )
          )
      ) %>%
      dplyr::arrange(continent, startpage)
  }

  overview_data <- overview_environ$overview

  return(overview_data)
}

#' Creating Dataframes from Excel Files
#'
#' @description Creating Excel-sheets from the available files
#'
#' @inheritParams gen_overview
#'
#' @param input_continent Name of the continent the Excel file refers to
#' @param input_continent_region Name of the continent region the Excel file refers to
#' @param input_startpage Starting page of the Excel file
#' @param input_endpage Ending page of the Excel file
#' @param input_pdf_file Pdf file corresponding to the excel file
#'
#' @return Excel sheet in data frame format
#'
#' @export
#'
read_excel_files <- function(input_path,
                             input_continent = NA,
                             input_continent_region = NA,
                             input_startpage = NA,
                             input_endpage = NA,
                             input_pdf_file = NA) {

  # Suppress those annoying "New names:" messages

  suppressMessages(
    concat <-
      readxl::read_xlsx(
        path = input_path,
        col_names = FALSE,
        col_types = "text",
        .name_repair = "universal"
      )
  )

  data.table::setDT(concat)

  # Not all of the excel files contain a "references" column,
  # since this column was only implemented later on.
  # In case the content of the fourth column got mistakenly inserted in the
  # fifth or sixth column

  if (ncol(concat) <= 3) {
    concat$references <- as.character(NA)
  } else if (ncol(concat) > 4) {
    concat <- concat %>%
      tidyr::unite(references, 4:ncol(concat), na.rm = TRUE)
  }

  # Renaming the columns/variables

  names(concat) <- c("id", "period", "ruler", "references")

  # Important: remove empty rows only after the variable excel_row has been
  # created, otherwise it loses its explanatory power. Also, it is convenient to
  # simply copy-paste the excel paste into the address bar of the explorer to
  # open the excel files. However, for that to be possible it is necessary that
  # the the forwardslash that R employs be replaced with two backslashes that
  # windows can use.

  sheet <-
    concat %>%
    dplyr::mutate(
      continent = input_continent,
      continent_region = input_continent_region,
      startpage = input_startpage,
      endpage = input_endpage,
      pdf_file = input_pdf_file,
      excel_sheet = gsub("\\/", "\\\\", input_path),
      excel_row = paste0("A", 1:dplyr::n()) # Makes it easier to open in Excel
    ) %>%
    dplyr::filter(
      !(is.na(id) & is.na(period) & is.na(ruler) & is.na(references))
    ) # Removing empty rows
}

base_dataset_environ <- new.env(parent = emptyenv())

#' Creating Base dataframe
#'
#' @description Dataframe that will be used as the "building foundation" for all
#'   the following data cleaning functions.
#'
#' @return input_data
#'
#' @export
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
      input_endpage = overview_data[["endpage"]],
      input_pdf_file = overview_data[["pdf_file"]]
    )

    # Concatenating the individual sheets to one big R file. Furthermore,
    # performing very important! data cleaning operations. The first "mutate()"
    # step removes all leading, trailing and repeated white spaces from all
    # variables of type string.

    base_dataset_init <-
      furrr::future_pmap_dfr(input_list, read_excel_files) %>%
      dplyr::mutate(
        dplyr::across(where(is.character), stringr::str_squish)
      ) %>%
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
      dplyr::mutate(
        ruler = stringr::str_replace_all(
          ruler,
          "([XVI]+\\.)(\\s)(\\d{2,4})",
          "\\1\\3"
        ),
        ruler = stringr::str_replace_all(
          ruler,
          "([XVI]+(?=\\.\\s?\\d{2,4}))",
          roman_to_alpha
        )
      )

    rlang::env_bind(base_dataset_environ, base_dataset = base_dataset_final)
  }

  base_dataset <- base_dataset_environ$base_dataset

  return(base_dataset)
}
