#' Exporting data to the Stata format
#'
#' @return
#' @export
#'
#' @examples
data_export_to_stata <- function() {
  requireNamespace("tidyselect")

  names_drop <- c(
    "id_keep", "unique_index", "original_sort", "id_old", "region_id", "start_page", "end_page"
  )

  base_data_set <- gen_estimation()

  data_export <-
    base_data_set %>%
    dplyr::mutate(
      id_keep = dplyr::if_else(
        is.na(unique_index) & id_valid == TRUE,
        FALSE,
        TRUE
      ),
      excel_sheet = basename(excel_sheet)
    ) %>%
    dplyr::relocate(id_keep, unique_index, id_valid) %>%
    dplyr::relocate(iso3, .after = country) %>%
    dplyr::relocate(page, .after = excel_row) %>%
    dplyr::filter(
      id_keep == TRUE
    ) %>%
    dplyr::arrange(original_sort) %>%
    dplyr::select(-any_of(names_drop)) %>%
    dplyr::rename(regent = ruler, pdf_page = page)

  Hmisc::label(data_export$id_valid) <- "Observation with an invalid ID should be treated with the utmost caution as most of the allocations depend on a valid ID. Invalid ID in this context means that the ID could not be correctly interpreted."

  Hmisc::label(data_export$id_group) <- "ID groups define the spatiotemporal framework in which the regent can exert influence, e.g.: a kingdom."

  Hmisc::label(data_export$excel_row) <- "Row of the Excel sheet the observation originated from"

  Hmisc::label(data_export$excel_sheet) <- "Name of the Excel sheet the observation originated from"

  Hmisc::label(data_export$N) <- "Row number used internally"

  Hmisc::label(data_export$N) <- "Side note associated with the respective observation. Mostly references to other pages."

  Hmisc::label(data_export$country) <- "Name of the country the regent reigned over (modern)"

  Hmisc::label(data_export$iso3) <- "ISO 3166-1 alpha-3 Code of the country the regent reigned over"

  Hmisc::label(data_export$pdf_page) <- "Page number of the PDF on which the Optical Character Recognition was applied to generate the excel sheet. Name of the PDF is identical to the excel sheet containing the data"

  Hmisc::label(data_export$id) <- "Identifier for the respective regent. Note: Identifier are only unique within ID groups. "

  Hmisc::label(data_export$period) <- "Period in which the regent reigned"

  Hmisc::label(data_export$regent) <- "Name of the regent"

  Hmisc::label(data_export$death) <- "Binary; 1 if it's known that the regent died a natural death, 0 otherwise"

  Hmisc::label(data_export$murdered) <- "Binary; 1 if it's known that the regent died a violent death, 0 otherwise"


  Hmisc::label(data_export$birthyear_l) <- "Lower estimation bound if the birth year was not exactly clear"

  Hmisc::label(data_export$birthyear_u) <- "Upper estimation bound if the birth year was not exactly clear"

  Hmisc::label(data_export$deathyear_l) <- "Lower estimation bound if the death year was not exactly clear"

  Hmisc::label(data_export$deathyear_u) <- "Upper estimation bound if the death year was not exactly clear"

  Hmisc::label(data_export$age_l) <- "Lower estimation bound if the age was not exactly clear"

  Hmisc::label(data_export$age_u) <- "Upper estimation bound if the age was not exactly clear"

  Hmisc::label(data_export$birthyear_known) <- "Binary; 1 if the birth year of the ruler was known, 0 otherwise"

  Hmisc::label(data_export$reign_start) <- "Start of the reigning period of the regent"

  Hmisc::label(data_export$reign_end) <- "End of the reigning period of the regent"

  Hmisc::label(data_export$reign_length) <- "Duration of the reigning period of the ruler"

  Hmisc::label(data_export$decade) <- "Decade in which the regent reigned"

  Hmisc::label(data_export$century) <- "Century in which the regent reigned"

  # attr(data_export, "note") <- c("Testing the notes function", "1")

  Hmisc::label(data_export) <-
    "Regents of Nations, Volumes 1-5, Book Series by Peter Truhart"


  haven::write_dta(data_export, path = "geo_data/regents_of_nations_full.dta", version = 14)
}
