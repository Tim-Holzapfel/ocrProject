# library(enhancedView)
# conflicted::conflict_prefer("View", "enhancedView")
clear()

admin_regions <-
  readRDS("test_environment/data/admin_regions.RDS") %>%
  dplyr::mutate(
    id_unique = dplyr::row_number(),
    .after = 1
  ) %>%
  dplyr::relocate(country)

library(dplyr)
































countries_mod <-
  countries %>%
  dplyr::mutate(
    country = dplyr::case_when(
      stringr::str_detect(country, "China") ~ "China",
      stringr::str_detect(country, "Namibia") ~ "Namibia, Boswana, South Africa, Lesotho",
      stringr::str_detect(country, "Ehtiopia") ~ "Ehtiopia, Erythrea, Djibouti",
      stringr::str_detect(country, "Indonesia") ~ "Indonesia",
      TRUE ~ as.character(country)
    )
  ) %>%
  dplyr::distinct()










admin_unite <-
  admin_regions %>%
  tidyr::drop_na(unique_index) %>%
  dplyr::select(dplyr::starts_with("level")) %>%
  tidyr::unite(
    "capitals",
    dplyr::everything(),
    sep = "|",
    na.rm = TRUE
  )


admin_split <-
  admin_unite %>%
  magrittr::extract2("capitals") %>%
  stringi::stri_trans_totitle() %>%
  stringr::str_split("\\|", simplify = TRUE) %>%
  as.data.frame()













admin_levels <-
  admin_regions %>%
  tidyr::drop_na(unique_index) %>%
  dplyr::select(dplyr::starts_with("level")) %>%
  tidyr::unite(
    "level2",
    dplyr::starts_with("level2"),
    sep = ",",
    na.rm = TRUE
  ) %>%
  tidyr::unite(
    "level3",
    dplyr::starts_with("level3"),
    sep = ",",
    na.rm = TRUE
  ) %>%
  tidyr::unite(
    "level4",
    dplyr::starts_with("level4"),
    sep = ",",
    na.rm = TRUE
  ) %>%
  tidyr::unite(
    "level5",
    dplyr::starts_with("level5"),
    sep = ",",
    na.rm = TRUE
  ) %>%
  dplyr::distinct()







country_pattern <-
  countrycode::codelist %>%
  dplyr::select(country.name.en, iso3c) %>%
  mutate_all(list(~ na_if(., ""))) %>%
  tidyr::drop_na(iso3c) %>%
  dplyr::mutate(
    country.name.en = stringr::str_replace(country.name.en, "\\(.*\\)", ""),
    n_chars = nchar(country.name.en)
  ) %>%
  dplyr::filter(
    stringr::str_detect(country.name.en, "Island", negate = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(n_chars)) %>%
  magrittr::extract2("country.name.en") %>%
  stringi::stri_trans_general("latin-ascii") %>%
  stringr::str_replace_all(
    c("\u0027" = "\u0027", "\u2019" = "\u0027", "\u02BC" = "\u0027", "\u00B4" = "\u0027"),
  ) %>%
  paste0(collapse = "|") %>%
  paste0("^(", ., ")$")







t1 <- country_pattern_regex %>% as.data.frame()




country_pattern_regex <-
  countrycode::codelist %>%
  magrittr::extract2("country.name.en.regex") %>%
  paste0(collapse = "|") %>%
  paste0("^(", ., ")$")





test_string <- "Sudan WEST AFRICA,SAHEL REGION,WESTERN SAHEL Mali, North Guinea"
test_string <- "Sudan"



stringr::str_extract_all(
  test_string,
  stringr::regex(country_pattern, ignore_case = TRUE)
)





level1 <- stringr::str_match(
  admin_levels$level1,
  stringr::regex(country_pattern_regex, ignore_case = TRUE)
)[, 1]



level2 <- stringr::str_match(
  admin_levels$level2,
  stringr::regex(country_pattern_regex, ignore_case = TRUE)
)[, 1]















# n_chars = nchar(country.name.en)
















t1 <- stringr::str_detect()
































































admin_regions <-
  regents_of_nations %>%
  tidyr::unite(
    "capitals",
    level1:level53,
    sep = "|",
    na.rm = TRUE
  ) %>%
  dplyr::select(capitals) %>%
  dplyr::mutate(
    capitals = stringr::str_replace(capitals, "dep\\.[^\\|]*", "")
  ) %>%
  dplyr::mutate(
    capitals = stringr::str_replace_all(
      capitals,
      c("\u0027" = "\u0027", "\u2019" = "\u0027", "\u02BC" = "\u0027", "\u00B4" = "\u0027"),
    )
  )

codelist <- countrycode::codelist

# Replacing other possible apostrophe variants with a uniform one to avoid
# finding no match because of unicode differences

country_list <-
  countrycode::codelist %>%
  magrittr::extract2("country.name.en") %>%
  stringi::stri_trans_general("latin-ascii") %>%
  stringr::str_replace_all(
    c("\u0027" = "\u0027", "\u2019" = "\u0027", "\u02BC" = "\u0027", "\u00B4" = "\u0027"),
  )

# It is important that Nigeria comes before Niger in the country list because
# otherwise will Nigeria be matched with Niger due to partial matching.

n_country <- length(country_list)
n_nigeria <- which(country_list == "Nigeria")
n_niger <- which(country_list == "Niger")
country_list_mod <-
  country_list[c(1:(n_niger - 1), n_nigeria, n_niger, (n_nigeria + 1):n_country)]

# Regular expression that is going to be used for the matching

country_pattern <- paste0(country_list_mod, collapse = "|")

n_sample <- dim(admin_levels)[1]

admin_sample <-
  admin_regions %>%
  dplyr::slice(1:n_sample)

admin_sample_level <-
  admin_levels %>%
  dplyr::slice(1:n_sample)

country_pattern_df <- stringr::str_match_all(
  string = admin_sample$capitals,
  pattern = stringr::regex(country_pattern, ignore_case = FALSE)
) %>%
  lapply(function(x) {
    if (length(x) == 0) {
      x <- matrix(data = "", ncol = 20)
    }
    y <- x[, 1]
    n_row <- max(1, length(y))
    x_matrix <- tibble::tibble(.rows = 1)
    x_matrix[1, paste0("V", 1:20)] <- character(1)
    x_matrix[1, 1:n_row] <- as.list(y)
    return(x_matrix)
  }) %>%
  rlist::list.stack() %>%
  tibble::as_tibble() %>%
  dplyr::mutate_all(dplyr::na_if, "") %>%
  janitor::remove_empty(which = "cols")

names(country_pattern_df)

country_pattern_df_unite <-
  country_pattern_df %>%
  tidyr::unite("c_unite", names(country_pattern_df), na.rm = TRUE, sep = ",") %>%
  dplyr::mutate(
    c_unite = stringi::stri_trans_totitle(c_unite)
  )

country_freq_tibble <- tibble::tibble(.rows = n_sample)
country_freq_tibble[, country_list_mod] <- numeric(n_sample)

for (i in country_list_mod) {
  country_freq_tibble[, i] <-
    stringr::str_count(
      country_pattern_df_unite$c_unite,
      stringr::regex(i, ignore_case = TRUE)
    )
}

country_freq_tibble <- as.matrix(country_freq_tibble)

# TODO in case of a tie vote (meaning two countries appear with the same
# frequency), make sure to select the country that appears first.

country_pattern_df_unite$country <-
  country_list_mod[Rfast::rowMaxs(country_freq_tibble)]

country_bind <-
  country_pattern_df_unite %>%
  cbind(admin_sample_level, admin_sample) %>%
  cbind(regents_of_nations[1:n_sample, c("region", "continent", "continent_region")])


regents_of_nations_co <-
  regents_of_nations %>%
  dplyr::select(-country) %>%
  cbind(country_bind[, c("c_unite", "country")]) %>%
  dplyr::filter(c_unite != "") %>%
  dplyr::select(-c(c_unite, id_group, region_id)) %>%
  dplyr::select(-tidyselect::starts_with("level")) %>%
  dplyr::relocate(id, country, period, ruler, birthyear_known, birthyear_u, deathyear_u)

saveRDS(regents_of_nations_co, "output/regents_of_nations_co.RDS", compress = FALSE)




#   ____________________________________________________________________________
#   Missing Countries                                                       ####

# geonames_cities <- readRDS("output/geonames_cities.RDS")
#
# missing_countries <-
#   country_bind %>%
#   dplyr::filter(c_unite == "") %>%
#   dplyr::mutate(
#     level4 = stringr::str_replace(level4, " \\& ", "|")
#   )
#
# pat1 <- "Katsina"
#
# results <- stringr::str_which(
#   geonames_cities$name,
#   stringr::regex(
#     pat1,
#     ignore_case = TRUE
#   )
# ) %>%
#   geonames_cities[., ] %>%
#   dplyr::distinct(country, region, region23)
#
# results_df <- geonames_cities[results, ]
#
# results <- geonames_cities[stringr::str_which(geonames_cities$name, stringr::fixed("Kano", ignore_case = TRUE)), ]
