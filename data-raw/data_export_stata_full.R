

#   ____________________________________________________________________________
#   Creating stata datasets                                                 ####

requireNamespace("tidyselect")

names_drop <- c(
  "pdf_page", "N", "excel_row",
  "excel_sheet", "excel_sheet", "startpage", "endpage",
  "pdf_file", "page", "region", "references", "unique_index", "original_sort"
)

base_data_set <- gen_admin_regions()

data_export <-
  base_data_set %>%
  tidyr::drop_na(unique_index) %>%
  dplyr::rename(regent = ruler) %>%
  dplyr::select(-any_of(names_drop))


Hmisc::label(data_export$id) <- "Identifier, IDs consisting of only three digits represent regents"

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


haven::write_dta(data_export, path = "data/regents_of_nations_full.dta", version = 14)
