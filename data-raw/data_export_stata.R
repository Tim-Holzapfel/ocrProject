
# Creating stata datasets



data_export <- gen_header() %>%
  dplyr::select(-c(
    "pdf_page", "N", "excel_row",
    "excel_sheet", "excel_sheet", "startpage", "endpage",
    "pdf_file", "page"
  ))

Hmisc::label(data_export$birthyear_l) <-
  "Lower bound if the birth year was estimated"

Hmisc::label(data_export$birthyear_u) <-
  "Upper bound if the birth year was estimated"

Hmisc::label(data_export$deathyear_l) <-
  "Lower bound if the death year was estimated"

Hmisc::label(data_export$deathyear_u) <-
  "Upper bound if the death year was estimated"

Hmisc::label(data_export$murdered) <-
  "Binary, 1 if the Ruler was murdered, 0 else"

Hmisc::label(data_export$death) <-
  "Binary, 1 if the death year of the Ruler is known, 0 else"

Hmisc::label(data_export$period) <- "Period in which the Ruler reigned"


haven::write_dta(data_export, path = "data/Truhart_data.dta")


# Modifying dataset for Svenja since she only has Stata 12 and Stata 12 only
# allows for strings shorter than 200.

gen_substring <- function(data) {
  data_aub <- stringr::str_sub(data, 1, 180)
}

data_export_svenja <- apply(data_export, 2, gen_substring) %>%
  tibble::as_tibble()

haven::write_dta(data_export_svenja,
  path = "data/Truhart_data_svenja.dta", version = 12
)
