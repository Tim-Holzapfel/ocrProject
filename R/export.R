
df <- ocrProject::create_final_sheet() %>%
  dplyr::select(-c(Excel_path, Startpage, Endpage, Page))

haven::write_dta(df, "data/Truhart.dta", version = 15)
