

regents_of_nations <-
  gen_country() %>%
  tidyr::drop_na(country) %>%
  dplyr::select(-starts_with("level")) %>%
  dplyr::select(-dplyr::any_of(c(
    "pdf_page", "N", "excel_row",
    "excel_sheet", "startpage", "endpage",
    "pdf_file", "page", "unique_index", "group_id", "original_sort",
    "country_id", "region_id", "id_old"
  )))







dplyr::select(-c(
  "pdf_page", "N", "excel_row",
  "excel_sheet", "excel_sheet", "startpage", "endpage",
  "pdf_file", "page", "unique_index"
))





names(regents_of_nations)


saveRDS(regents_of_nations, "data/regents_of_nations.RDS", compress = FALSE)

# saveRDS(regents_of_nations,
#         "D:/uni_tuebingen/Master/Kurse/MasterThesisTimHolzapfel/data/main_data/regents_of_nations.RDS",
#         compress = FALSE)



dplyr::select(-starts_with("level")) %>%
  dplyr::select(-c(country_id, unique_index, region_id, id_old))
