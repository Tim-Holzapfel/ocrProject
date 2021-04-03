

regents_of_nations <-
  gen_estimation() %>%
  dplyr::rename(regent = ruler) %>%
  tidyr::drop_na(country) %>%
  dplyr::select(-starts_with("level")) %>%
  dplyr::select(-dplyr::any_of(c(
    "pdf_page", "N", "excel_row",
    "excel_sheet", "startpage", "endpage",
    "pdf_file", "page", "unique_index", "group_id", "original_sort",
    "country_id", "region_id", "id_old", "period_count", "period_contains_dots",
    "period_dots_replaced", "dots_replaced_true"
  )))


names(regents_of_nations)

saveRDS(regents_of_nations, "data/regents_of_nations.RDS", compress = FALSE)

saveRDS(regents_of_nations,
  "D:/uni_tuebingen/Master/Kurse/MasterThesisTimHolzapfel/data/main_data/regents_of_nations.RDS",
  compress = FALSE
)
