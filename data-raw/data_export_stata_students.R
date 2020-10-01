
# Creating stata datasets

names_drop <- c(
  "pdf_page", "N", "excel_row",
  "excel_sheet", "excel_sheet", "startpage", "endpage",
  "pdf_file", "page", "region", "references"
)

requireNamespace("tidyselect")

data_export <- removing_invalid_ids() %>%
  dplyr::select(-any_of(names_drop)) %>%
  dplyr::rename(regent = ruler, region = continent_region) %>%
  tidyr::drop_na(id) %>%
  dplyr::relocate(
    id, period, regent, continent, region, country,
    decade, century, half_cen, birthyear_l, birthyear_u,
    deathyear_l, deathyear_u, age_l, age_u, birthyear_known,
    reign_start, reign_end, reign_length
  )






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


haven::write_dta(data_export, path = "data/regents_of_nations.dta", version = 14)














#   ____________________________________________________________________________
#   Experimental code                                                       ####


data_export <-
  data_export %>%
  dplyr::mutate(
    country_san = country,
    country_san = stringi::stri_replace_all_regex(
      country_san,
      "([A-z])(?:\\s)([a-z])",
      "$1$2"
    ),
    country_san = stringr::str_to_title(country_san),
    country_san =
      dplyr::case_when(
        country_san == "African Kingdom" ~ "African Kingdoms",
        country_san == "Amenia" ~ "Armenia",
        stringr::str_detect(country_san, "Indonesia") ~ "Indonesia",
        stringr::str_detect(country_san, "China") ~ "China",
        stringr::str_detect(country_san, "Ehtiopia") ~ "Ehtiopia, Erythrea & Djibouti",
        stringr::str_detect(country_san, "Namibia") ~ "Namibia & Boswana, South Africa & Lesotho",
        stringr::str_detect(country_san, "British India") ~ "British India",
        stringr::str_detect(country_san, "Cameroon") ~ "Cameroon, Gabon & Equatorial Guinea",
        stringr::str_detect(country_san, "Central & Western") ~ "Central & Western Regions",
        stringr::str_detect(country_san, "Central Sahel") ~ "Central Sahel Region",
        stringr::str_detect(country_san, "Christian") ~ "Christian Kingdoms",
        stringr::str_detect(country_san, "Colonial") ~ "Colonial Order & Independence",
        stringr::str_detect(country_san, "Ge\\^W") ~ "Germano-Roman Empire",
        stringr::str_detect(country_san, "Grmiano") ~ "Germano-Roman Empire",
        stringr::str_detect(country_san, "Indomesia") ~ "Indonesia",
        stringr::str_detect(country_san, "Madhya") ~ "Madhya Pradesh",
        stringr::str_detect(country_san, "Northernitalia") ~ "Northern Italia",
        stringr::str_detect(country_san, "Portuguese India 6") ~ "Portuguese India",
        stringr::str_detect(country_san, "Afhanistan") ~ "Afghanistan",
        stringr::str_detect(country_san, "Arabic") ~ "Arabia",
        stringr::str_detect(country_san, "Autralia") ~ "Australia",
        stringr::str_detect(country_san, "Azerbaijan \\(South\\)") ~ "Azerbaijan",
        stringr::str_detect(country_san, "Azerbaican") ~ "Azerbaijan",
        stringr::str_detect(country_san, "Azerbajan") ~ "Azerbaijan",
        stringr::str_detect(country_san, "Bihav") ~ "Bihar",
        stringr::str_detect(country_san, "Muslim") ~ "Muslim Spain",
        stringr::str_detect(country_san, "Indonesia") ~ "Indonesia",
        stringr::str_detect(country_san, "Indonesia") ~ "Indonesia",
        TRUE ~ as.character(country_san)
      ),
    .after = regent
  ) %>%
  dplyr::select(-country) %>%
  dplyr::rename(country = country_san) %>%
  dplyr::relocate(id, period, regent, country, region, continent) %>%
  dplyr::filter(
    stringr::str_detect(id, "^\\d{3}$")
  )

Hmisc::label(data_export$id) <- "Identifier of regents"

Hmisc::label(data_export$period) <- "Reigning period of regent"

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

attr(data_export, "note") <- c("Testing the notes function", "1")

Hmisc::label(data_export) <-
  "Regents of Nations, Volumes 1-5, Book Series by Peter Truhart"


haven::write_dta(data_export, path = "data/regents_of_nations.dta", version = 15)





































# Modifying dataset for Svenja since she only has Stata 12 and Stata 12 only
# allows for strings shorter than 200.

gen_substring <- function(data) {
  data_aub <- stringr::str_sub(data, 1, 180)
}

data_export_svenja <- apply(data_export, 2, gen_substring) %>%
  tibble::as_tibble()

haven::write_dta(data_export_svenja,
  path = "data/Truhart_data_svenja.dta", version = 12
)
