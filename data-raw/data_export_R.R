
export_data <- gen_admin_regions()

regents_of_nations <- export_data %>%
  dplyr::select(-c(
    "pdf_page", "N", "excel_row",
    "excel_sheet", "excel_sheet", "startpage", "endpage",
    "pdf_file", "page", "unique_index"
  ))

saveRDS(regents_of_nations, "data/regents_of_nations.RDS")

saveRDS(regents_of_nations,
        "D:/uni_tuebingen/Master/Kurse/MasterThesisTimHolzapfel/data/main_data/regents_of_nations.RDS",
        compress = FALSE)

