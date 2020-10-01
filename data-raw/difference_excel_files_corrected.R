

not_corrected_files <-
  list.files(
    path = "D:/lehrstuhl_wirtschaftsgeschichte/km/Truhart",
    pattern = "\\.xlsx$",
    recursive = TRUE,
    all.files = TRUE
  ) %>%
  basename() %>%
  unique() %>%
  tibble::as_tibble()


not_corrected_files$value_sub <-
  not_corrected_files$value %>%
  stringr::str_extract("^[^\\_]*") %>%
  stringr::str_replace("\\.xlsx", "")

not_corrected_tbl <- not_corrected_files["value_sub"]


corrected_files <-
  list.files(
    path = "D:/ocrProject/completed_files",
    pattern = "\\.xlsx$",
    recursive = TRUE,
    all.files = TRUE
  ) %>%
  basename() %>%
  unique() %>%
  tibble::as_tibble()

corrected_files$value_sub <-
  corrected_files$value %>%
  stringr::str_extract("^[^\\_]*") %>%
  stringr::str_replace("\\.xlsx", "")

corrected_tbl <- corrected_files["value_sub"]






t1 <- fuzzyjoin::stringdist_anti_join(not_corrected_tbl, corrected_tbl)
