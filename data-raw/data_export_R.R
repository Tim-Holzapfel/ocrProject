
export_data <- gen_admin_regions()

regents_of_nations <- export_data %>%
  dplyr::select(-c(
    "pdf_page", "N", "excel_row",
    "excel_sheet", "excel_sheet", "startpage", "endpage",
    "pdf_file", "page", "unique_index"
  ))

save(regents_of_nations, file = "data/regents_of_nations.rda")

save(regents_of_nations,
     file = "D:/Uni Tuebingen/Master/Kurse/MasterThesisTimHolzapfel/data/regents_of_nations.rda")
