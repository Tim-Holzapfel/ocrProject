#' Creating Dataframes from Excel Files
#'
#' @description Creating Excel-sheets from the available files
#'
#' @param input_path Path to the excel files
#' @param input_continent Name of the continent the Excel file refers to
#' @param input_continent_region Name of the continent region the Excel file refers to
#' @param input_startpage Starting page of the Excel file
#' @param input_endpage Ending page of the Excel file
#'
#' @return Excel sheet in data frame format
#'
#' @export
#'
read_excel_files <- function(input_path,
                             input_continent = NA,
                             input_continent_region = NA,
                             input_startpage = NA,
                             input_endpage = NA) {

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
      excel_sheet = gsub("\\/", "\\\\", input_path),
      excel_row = paste0("A", 1:dplyr::n()) # Makes it easier to open in Excel
    ) %>%
    dplyr::filter(
      !(is.na(id) & is.na(period) & is.na(ruler) & is.na(references))
    ) # Removing empty rows
}
