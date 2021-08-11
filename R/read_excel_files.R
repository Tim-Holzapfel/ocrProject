#' Creating Dataframes from Excel Files
#'
#' @description Creating Excel-sheets from the available files
#'
#' @param input_path Path to the excel files
#' @param input_continent Name of the continent the Excel file refers to
#' @param input_region Name of the continent region the Excel file refers to
#' @param input_start_page Starting page of the Excel file
#' @param input_end_page Ending page of the Excel file
#'
#' @return Excel sheet in data frame format
#'
read_excel_files <- function(input_path,
                             input_continent,
                             input_region,
                             input_start_page,
                             input_end_page) {

  # input_path <- "D:/ocrProject/test_environment/excel_files/Truhart3new.p1254to1291.SouthEastAsia_edited_1.xlsx"

  concat <- openxlsx::read.xlsx(
    input_path,
    colNames = FALSE,
    skipEmptyRows = FALSE
  )

  # Not all of the excel files contain a "references" column, since this column
  # was only implemented later on. In case the content of the fourth column got
  # mistakenly inserted in the fifth or sixth column

  if (ncol(concat) <= 3) {
    concat$references <- as.character(NA)
  } else if (ncol(concat) > 4) {
    concat <- concat %>%
      tidyr::unite(references, 4:ncol(concat), na.rm = TRUE)
  }

  # Renaming the columns/variables

  names(concat) <- c("id", "period", "ruler", "references")

  # Important: remove empty rows only after the variable excel_row has been
  # created, otherwise it loses its explanatory power.

  sheet <-
    concat %>%
    dplyr::mutate(
      continent = input_continent,
      region = input_region,
      start_page = input_start_page,
      end_page = input_end_page,
      excel_sheet = input_path,
      excel_row = paste0("A", 1:dplyr::n()) # Makes it easier to open in Excel
    ) %>%
    dplyr::filter(
      !(is.na(id) & is.na(period) & is.na(ruler) & is.na(references))
    ) # Removing empty rows
}
