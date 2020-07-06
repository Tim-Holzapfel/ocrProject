
# Creating stata datasets

base_data_set <- gen_admin_regions()


data_export <-
  base_data_set %>%
  dplyr::select(
    -c(
      "pdf_page", "N", "excel_row",
      "excel_sheet", "excel_sheet", "startpage", "endpage",
      "pdf_file", "page", "unique_index"
    )
  )

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


haven::write_dta(data_export, path = "data/regents_of_nations.dta")
