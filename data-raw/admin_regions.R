
admin_regions <- gen_admin_regions()

admin_data <- admin_regions %>%
  dplyr::arrange(N) %>%
  dplyr::select(-c(
    "page", "startpage", "endpage", "pdf_file", "excel_sheet",
    "excel_row", "N", "pdf_page", "unique_index"
  ))




haven::write_dta(admin_data, "data/Truhart_admin_regions.dta")
