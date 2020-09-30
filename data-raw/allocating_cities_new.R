# library(enhancedView)
# conflicted::conflict_prefer("View", "enhancedView")
clear()

library(dplyr)


admin_group_ids <-
  readRDS("test_environment/data/admin_regions.RDS") %>%
  dplyr::filter(
    stringr::str_detect(id, "^\\d{3}$", negate = TRUE)
  ) %>%
  dplyr::arrange(id_group, id) %>%
  dplyr::select(id_group, id, period, ruler, page, excel_sheet)

country_pattern <-
  countrycode::codelist %>%
  dplyr::select(country = country.name.en, iso3c) %>%
  mutate_all(list(~ na_if(., ""))) %>%
  tidyr::drop_na(iso3c) %>%
  dplyr::mutate(
    country = stringr::str_replace(country, "\\(.*\\)", ""),
    n_chars = nchar(country)
  ) %>%
  dplyr::arrange(dplyr::desc(n_chars)) %>%
  dplyr::mutate(
    country = dplyr::case_when(
      stringr::str_detect(country, "Congo") ~ "Congo",
      stringr::str_detect(country, "Sudan") ~ "Sudan",
      stringr::str_detect(country, "Hong Kong") ~ "Hong Kong",
      TRUE ~ as.character(country)
    )
  ) %>%
  dplyr::distinct(country, .keep_all = TRUE) %>%
  magrittr::extract2("country") %>%
  stringi::stri_trans_general("latin-ascii") %>%
  stringr::str_replace_all(
    c("\u0027" = "\u0027", "\u2019" = "\u0027", "\u02BC" = "\u0027", "\u00B4" = "\u0027"),
  ) %>%
  c("Erythrea", "Turkestan", "Arabia", "Burma", "Malayan Peninsula", "Singapur",
    "Kalimantan", "Kapunduk", "Hawaii", "Melanesia", "Polynesia", "Mali\\sStates",
    "Timuktu", "Sonrhai States", "Sossebaki States", "Upper Guinean Coast",
    "Cote D'Ivoire", "ANGOLA\\s\\&\\sCONGO", "ZAIRE", "Congo\\s(Zaire)", "Somaliland") %>%
  paste0("\\b", ., "\\b", collapse = "|") %>%
  paste0("(", ., ")")



admin_regions <-
  readRDS("test_environment/data/admin_regions.RDS") %>%
  dplyr::relocate(country) %>%
  dplyr::mutate(
    country_id = paste0(id_group, ".", id),
    id_unique = dplyr::row_number(),
    .after = 1
  ) %>%
  dplyr::arrange(id_group, id) %>%
  tidyr::drop_na(unique_index)


result_function <- function(x) {

  # Very long strings have an increased likelihood of containing information
  # that should not be considered as an individual country.
  string_length <- nchar(x)


  string_count <- stringr::str_count(
    x,
    stringr::regex(country_pattern, ignore_case = TRUE)
  )

  string_extract <- stringr::str_extract(
    x,
    stringr::regex(country_pattern, ignore_case = TRUE)
  )

  result_return <- dplyr::if_else(
    (string_count == 1) & (string_length <= 70),
    string_extract,
    NA_character_
  )

  return(result_return)

}


stop_pattern <-
  c("\\(", "ct\\.?:", "dep\\.", "t\\.?:", "States West of Nigeria") %>%
  paste0(collapse = "|") %>%
  paste0("(", ., ").*")


admin_levels <-
  admin_regions %>%
  dplyr::select(dplyr::starts_with(paste0("level", 1:4))) %>%
  tidyr::unite("level", dplyr::everything(), sep = "|", na.rm = TRUE) %>%
  magrittr::extract2("level") %>%
  stringr::str_split("\\|", simplify = TRUE) %>%
  as.data.frame() %>%
  mutate_all(list(~ na_if(., ""))) %>%
  dplyr::mutate(dplyr::across(.fns = function(x) {
    stringr::str_replace(x, stop_pattern, "")
  })) %>%
  dplyr::group_by(across(dplyr::starts_with("V"))) %>%
  dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
  cbind(id = admin_regions$country_id, .) %>%
  dplyr::distinct(across(dplyr::starts_with("V")), .keep_all = TRUE)


z_results <-
  admin_levels %>%
  plyr::colwise(.fun = result_function)(.) %>%
  tidyr::unite("capital", dplyr::everything(), sep = "|", na.rm = TRUE) %>%
  cbind(., admin_levels)



z_results_fill <-
  z_results %>%
  mutate_all(list(~ na_if(., ""))) %>%
  dplyr::mutate(
    id_second = stringr::str_extract(id, "(?<=\\.)\\d+") %>% as.integer(),
    id_first = stringr::str_extract(id, "\\d+(?=\\.)") %>% as.integer(),
    .after = id
  ) %>%
  dplyr::group_by(id_first) %>%
  dplyr::arrange(id_first, id_second) %>%
  tidyr::fill(capital, .direction = "downup")




















test_string <- "SUDAN SUDAN"

result_function(admin_levels$V1)



string_unique <-
  test_string %>%
  stringr::str_split("\\s") %>%
  unlist() %>%
  unique() %>%
  paste0(collapse = " ")








