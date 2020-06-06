#' @title Excel Overview
#'
#' @description This function can be used to generate a quick overview of
#' the available excel
desc_file <-
  function() {

    # Listing all available excel files in the given directory and the
    # sub-directories

    excel_count <-
      function(x) {
        nrow(
          readxl::read_xlsx(
            path = x[1],
            col_names = F,
            range = cellranger::cell_cols(1),
            .name_repair = "minimal"
          )
        )
      }

    excel_files <-
      list.files(
        path = "D:/km/Truhart",
        pattern = "xlsx",
        recursive = T,
        full.names = T
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        "Startpage" =
          stringi::stri_extract_last_regex("value", "[0-9]+(?=to|-)") %>%
            as.integer(),
        "Endpage" =
          stringi::stri_extract_last_regex("value", "(?<=to|-)[0-9]+") %>%
            as.integer(),
        "Continent" = stringi::stri_extract_last_regex("value", "(?<=\\/km\\/Truhart\\/)[A-z]+"),
        "Rows" = apply(excel_files, 1, excel_count),
        "Rows_sum" = cumsum("Rows")
      )

    return(excel_files)
  }

#' @title Concatenating Excel-files
#'
#' @description This function concatenates the individual Excel-files to one
#' R-file
#'
#' @param path_input The path to the Truhart Excel-files
#'
#' @export
generate_sheets <- function(path_input = "D:/km/Truhart") {
  excel_files <-
    list.files(
      path = path_input,
      pattern = "xlsx",
      recursive = T,
      full.names = T
    )


  concat_excel <- function(input) {
    concat <-
      readxl::read_xlsx(
        path = paste0(input),
        col_names = FALSE,
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
        tidyr::unite("References", 4:ncol(concat), na.rm = TRUE)
    }

    names(concat) <- c("ID", "Period", "Ruler", "References")

    sheet <-
      concat %>%
      dplyr::mutate(
        Continent =
          stringi::stri_extract_last_regex(input, "(?<=\\/km\\/Truhart\\/)[A-z]+"),
        Startpage =
          stringi::stri_extract_last_regex(input, "[0-9]+(?=to|-)") %>%
            as.integer(),
        Endpage =
          stringi::stri_extract_last_regex(input, "(?<=to|-)[0-9]+") %>%
            as.integer(),
        Excel_sheet = input,
        Excel_N = dplyr::row_number()
      )


    return(sheet)
  }

  excel_sheets <- purrr::map_dfr(excel_files, concat_excel)

  return(excel_sheets)
}
