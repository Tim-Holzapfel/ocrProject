
#' @title Generating overview
#'
#' @description This function generates an overview of the available
#' files
#'
#' @param path_input Path to the directory where the Truhart files are saved
#'
gen_overview <- function(path_input = "D:/km/Truhart") {

  # Creating a list of all the available excel files. Important! The rows of the
  # overview function "excel_files" need to be ordered

  list.files(
    path = path_input,
    pattern = "xlsx",
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Startpage =
        as.integer(
          stringi::stri_extract_last_regex(
            value,
            "[0-9]+(?=to|-)"
          )
        ),
      Endpage =
        as.integer(
          stringi::stri_extract_last_regex(
            value,
            "(?<=to|-)[0-9]+"
          )
        ),
      Continent =
        stringi::stri_extract_last_regex(
          value,
          "(?<=\\/km\\/Truhart\\/)[A-z]+"
        )
    ) %>%
    dplyr::arrange(Continent, Startpage)
}


#' @title Concatenating Excel-files
#'
#' @description This function concatenates the individual Excel-files to one
#'   R-file
#'
#' @keywords internal
concat_sheets <- function() {

  # Creating a list of all the available excel files. Important! The rows of the
  # overview function "excel_files" need to be ordered

  excel_files <- gen_overview()

  input_list <- list(
    input_path = excel_files[["value"]],
    input_continent = excel_files[["Continent"]],
    input_startpage = excel_files[["Startpage"]],
    input_endpage = excel_files[["Endpage"]]
  )

  # Function to create the individual excel sheets. This function is given as
  # input to the map function of "purrr" to concatenate the excel sheets

  gen_sheet <-
    function(input_path, input_continent, input_startpage, input_endpage) {
      concat <-
        readxl::read_xlsx(
          path = input_path,
          col_names = FALSE,
          col_types = "text",
          .name_repair = "universal"
        )

      # Not all of the excel files contain a "References" column,
      # since this column was only implemented later on.
      # In case the content of the fourth column got mistakenly inserted in the
      # fifth or sixth column

      if (ncol(concat) <= 3) {
        concat$References <- as.character(NA)
      } else if (ncol(concat) > 4) {
        concat <- concat %>%
          tidyr::unite(References, 4:ncol(concat), na.rm = TRUE)
      }

      names(concat) <- c("ID", "Period", "Ruler", "References")

      sheet <-
        concat %>%
        dplyr::mutate(
          Continent = input_continent,
          Startpage = input_startpage,
          Endpage = input_endpage,
          Excel_sheet = input_path,
          Excel_Row = dplyr::row_number()
        )

      return(sheet)
    }

  # Concatenating the individual sheets to one big R file

  excel_sheets <-
    purrr::pmap_dfr(input_list, gen_sheet) %>%
    dplyr::mutate(
      N = dplyr::row_number()
    )

  return(excel_sheets)
}


#' @title Generate header
#'
#' @description The headers contain useful information i.e. the region and the
#'   country of the ruler. To extract these information, use is made of the fact
#'   that the left contains the region and the right header the country.
#'
gen_header <- function(sheet) {

  # sheet <- concat_sheets()

  # By construction "unite" removes the word or placeholder NA and leaves the
  # entry empty. This can lead to problems, therefore blank cells are replaced
  # with NA.

  # Create index for the relevant rows (those containing the header). To
  # identify the headers, only those rows are kept in which either the left and
  # middle columns and not the right column were empty or in which the right and
  # middle columns were empty and not the left column. However, a bit of
  # adjustment is required to make sure that some rows were not selected by
  # mistake.

  # Left header not empty -> Part of continent
  # Right header not empty -> Name of country or region
  # Selecting rows in which either the left column is not empty but the
  # middle and right columns are empty or in which the right column is not empty
  # but the middle column and the left column are.

  header <-
    sheet %>%
    tidyr::unite(
      Ruler_ref,
      c("Ruler", "References"),
      na.rm = TRUE,
      remove = FALSE,
      sep = " "
    ) %>%
    dplyr::mutate(
      Ruler_ref = dplyr::na_if(Ruler_ref, "")
    ) %>%
    dplyr::select(Region = ID, Period, Country = Ruler_ref, N) %>%
    dplyr::filter(
      (!is.na(Region) & is.na(Period) & is.na(Country)) |
        (is.na(Region) & is.na(Period) & !is.na(Country))
    ) %>%
    dplyr::filter(
      stringi::stri_detect_regex(Region, "^\\d{1,4} [A-z]+") |
        stringi::stri_detect_regex(Country, "[A-z]+\\)? \\d{1,4}$")
    ) %>%
    dplyr::mutate(
      Period = NULL,
      Page = stringr::str_extract(Region, "^\\d{1,4}"),
      Page = ifelse(
        is.na(Page),
        stringr::str_extract(Country, "\\d{1,4}$"),
        Page
      ) %>%
        as.integer(),
      Region = stringr::str_remove(Region, "^\\d{1,4}") %>%
        trimws(),
      Country = stringr::str_remove(Country, "\\d{1,4}$") %>%
        trimws()
    )

  return(header)
}



#' @title Final Sheet
#'
#' @description Finalizing the sheet
#'
#' @export
#'
finalize_sheet <- function() {
  sheet <- concat_sheets()

  header <- gen_header(sheet)

  file <-
    dplyr::left_join(sheet, header, by = "N") %>%
    dplyr::select(
      ID, Period, Ruler, References, Region, Country, Page,
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      Region = zoo::na.locf(Region, na.rm = FALSE),
      Country = zoo::na.locf(Country, na.rm = FALSE),
      Country = zoo::na.locf(Country, na.rm = FALSE, fromLast = TRUE),
      Page = zoo::na.locf(Page, na.rm = FALSE)
    ) %>%
    dplyr::arrange(Continent, Page, Excel_Row)


  return(file)
}
















































#'
#' #' creating the header for the files
#' #'
#' #' @name create_header
#' #'
#' #' @import magrittr
#' #'
#' #' @noRd
#' create_header <- function() {
#'   sheet <- create_sheet()
#'
#'   header <- sheet %>%
#'     tidyr::unite("Ruler_ref",
#'       c("Ruler", "References"),
#'       na.rm = TRUE,
#'       remove = FALSE,
#'       sep = " "
#'     )
#'
#'   # By construction "unite" removes the word or placeholder NA and leaves
#'   # the entry empty. This can lead to problems,
#'   # therefore blank cells are replaced with NA.
#'
#'   header[which(header$Ruler_ref == ""), "Ruler_ref"] <- NA
#'
#'   index_header <- header %>%
#'     with(
#'       which(
#'         (!is.na(ID) & is.na(Period) & is.na(Ruler_ref)) |
#'           (is.na(ID) & is.na(Period) & !is.na(Ruler_ref))
#'       )
#'     )
#'
#'   header <- header %>%
#'     dplyr::slice(index_header) %>%
#'     dplyr::select(
#'       Region = ID, Country = Ruler_ref, N,
#'       Excel_path, Continent, Startpage, Endpage
#'     )
#'
#'
#'   header %<>% dplyr::filter(
#'     (stringi::stri_detect_regex(Country, "[A-z ]+[0-9]+$") |
#'       stringi::stri_detect_regex(Region, "^[0-9]+[A-z ]+"))
#'   )
#'
#'   # Header on the left side: Number is explicitly extracted from the
#'   # right to prevent missmatches
#'
#'   header$header_right <-
#'     stringi::stri_extract_last(header$Country, regex = c("[0-9]+"))
#'
#'   # Header on the right side: Number is explicitly extracted from the
#'   # left to prevent missmatches
#'
#'   header$header_left <-
#'     stringi::stri_extract_first(header$Region, regex = c("[0-9]+"))
#'
#'   header <-
#'     tidyr::unite(
#'       header,
#'       "Page",
#'       c("header_right", "header_left"),
#'       na.rm = TRUE,
#'       remove = TRUE
#'     )
#'
#'   # Page should only be formatted to integer after the left and right
#'   # header have been united
#'
#'   header$Page %<>% as.integer()
#'
#'   # Test to check that the page header is valid
#'   page_header_test <-
#'     (header$Page >= header$Startpage) & (header$Page <= header$Endpage)
#'
#'   # TODO move the test "assertthat" to the test files folder
#'   # print(assertthat::validate_that(all(page_header_test),
#'   #   msg = "Warning: Not all of the header pages are in their right place!"
#'   # ))
#'
#'   wrong_page_index <-
#'     which(!page_header_test)
#'
#'   # Wrong pages
#'
#'   wrong_header <-
#'     header[wrong_page_index, ]
#'
#'
#'   usethis::use_data(wrong_header, overwrite = TRUE)
#'
#'   # The entries with the wrong page header are dropped
#'   # and need to be corrected
#'
#'   header <-
#'     header[-wrong_page_index, ]
#'
#'   # Test to check that there are no missing page headers.
#'   # To check the page increments, the pages need to be in the right order
#'
#'   header <-
#'     header[order(
#'       header$Continent,
#'       header$Startpage,
#'       header$Endpage,
#'       header$Page,
#'       decreasing = FALSE
#'     ), ]
#'
#'   # Generating "page increment", difference of the page number
#'   # between one page and the next. Should be 1 unless adjacent
#'   # pages are missing.
#'
#'   header$increment <- header$Page - dplyr::lag(header$Page)
#'
#'   header$increment[1] <- 1
#'
#'   # Entries from different continents are usually from different books,
#'   # these entries need therefore to be excluded
#'
#'   header$increment[which(!(header$Continent ==
#'     dplyr::lag(header$Continent)))] <- 1
#'
#'
#'   pages_missing <-
#'     header[which(header$increment != 1), ]
#'
#'
#'   usethis::use_data(pages_missing, overwrite = TRUE)
#'
#'
#'   # Once the number has been extracted from the left and right header
#'   # it can or should be removed
#'
#'   header$Region <-
#'     stringr::str_remove(header$Region, "[0-9]+") %>% trimws()
#'
#'   header$Country <-
#'     stringr::str_remove(header$Country, "[0-9]+") %>% trimws()
#'
#'   index$header <-
#'     header[["N"]]
#'
#'   return(header)
#' }
