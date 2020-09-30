# library(enhancedView)
# conflicted::conflict_prefer("View", "enhancedView")

clear()

admin_regions <-
  readRDS("test_environment/data/admin_regions.RDS") %>%
  dplyr::mutate(
    id_unique = dplyr::row_number(),
    .after = 1
  ) %>%
  dplyr::filter(continent == "Africa") %>%
  dplyr::relocate(id, id_old, id_unique)



#   ____________________________________________________________________________
#   Allocating Principalities                                               ####

# Pattern to remove unnecessary parts from the principalities
princ_replace_pattern <-
  c(
    "(?<!^)\\(", "[ct]+\\.:", "t:", "[dD]yn\\.", "dep\\.", "Muslim Dynast\\,",
    "Dynast\\.", "[A-z]\\.\\d+", "(?<!^)\\d+", ","
  ) %>%
  paste0(., collapse = "|") %>%
  paste0("(", ., ").*")

admin_princ <-
  admin_regions %>%
  dplyr::mutate(
    princ_ind = stringr::str_detect(id_old, "\\/"),
    princ = dplyr::if_else(
      princ_ind,
      ruler,
      NA_character_
    ),
    .after = id_old
  ) %>%
  dplyr::mutate(
    princ = stringr::str_replace(princ, princ_replace_pattern, "")
  ) %>%
  tidyr::fill(princ, .direction = "down") %>%
  tidyr::drop_na(unique_index)




#   ____________________________________________________________________________
#   Combining levels of admin regions                                       ####


admin_unite <-
  admin_princ %>%
  dplyr::select(dplyr::starts_with("level")) %>%
  tidyr::unite(
    "capitals",
    dplyr::everything(),
    sep = "|",
    na.rm = TRUE
  )

admin_unite$id_unique <- admin_princ$id_unique

admin_unite$country_admin <- admin_princ$country


admin_unite$capitals_sub <-
  admin_unite$capitals %>%
  stringr::str_replace_all("\\s\\([^\\|]*", "")

admin_split <-
  admin_unite %>%
  magrittr::extract2("capitals_sub") %>%
  stringi::stri_trans_totitle() %>%
  stringr::str_split("\\|", simplify = TRUE) %>%
  as.data.frame()


#   ____________________________________________________________________________
#   Allocating Countries                                                    ####


admin_string <-
  admin_unite %>%
  magrittr::extract2("capitals") %>%
  stringi::stri_trans_totitle()

country_pattern <-
  countrycode::codelist %>%
  dplyr::select(country.name.en, continent) %>%
  tidyr::drop_na(continent) %>%
  dplyr::mutate(
    n_chars = nchar(country.name.en)
  ) %>%
  dplyr::arrange(dplyr::desc(n_chars)) %>%
  magrittr::extract2("country.name.en") %>%
  stringi::stri_trans_general("latin-ascii") %>%
  stringr::str_replace_all(
    c("\u0027" = "\u0027", "\u2019" = "\u0027", "\u02BC" = "\u0027", "\u00B4" = "\u0027"),
  ) %>%
  paste0(collapse = "|") %>%
  paste0("^", ., "$")

country_pattern_df <- stringr::str_extract(
  string = admin_string,
  pattern = stringr::regex(country_pattern, ignore_case = TRUE)
) %>%
  as.data.frame() %>%
  dplyr::rename(country = ".") %>%
  cbind(admin_split) %>%
  cbind(country_admin = admin_princ$country)



country_pattern_df[which(country_pattern_df$country == "Somaliland"), "country"] <-
  "Somalia"



#   ____________________________________________________________________________
#   Missing Countries / Countries that could not be allocated               ####


geonames_africa <- readRDS(find_files("geonames_africa.RDS")) %>%
  tibble::as_tibble()

country_missing <-
  country_pattern_df %>%
  cbind(princ = admin_princ$princ) %>%
  dplyr::relocate(princ) %>%
  dplyr::filter(is.na(country)) %>%
  dplyr::group_by(princ) %>%
  dplyr::mutate(
    id_missing = dplyr::cur_group_id()
  ) %>%
  dplyr::ungroup()

country_missing_sub <-
  country_missing %>%
  dplyr::distinct(id_missing, .keep_all = TRUE)

country_missing_sub$princ_str_dist <- NA_real_
country_missing_sub$princ_match <- NA_character_

for (i in seq_len(nrow(country_missing_sub))) {
  print(i)

  princ_str_dist <- stringdist::stringdist(
    country_missing_sub$princ[i],
    geonames_africa$countryname,
    method = "jw",
    p = 0.1
  ) %>%
    as.data.frame() %>%
    dplyr::rename(str_dist = ".") %>%
    cbind(geonames_africa) %>%
    dplyr::arrange(str_dist)

  country_missing_sub$princ_str_dist[i] <- princ_str_dist$str_dist[1]
  country_missing_sub$princ_match[i] <- princ_str_dist$countryname[1]
  country_missing_sub$country[i] <- princ_str_dist$country[1]
}

country_missing_mod <-
  country_missing_sub %>%
  dplyr::select(id_missing, country_sub = country) %>%
  dplyr::left_join(country_missing, ., by = "id_missing") %>%
  dplyr::select(-c(country, princ, id_missing)) %>%
  dplyr::rename(country = country_sub) %>%
  dplyr::relocate(country)



# country_pattern_df without missing countries
country_pattern_df_mod <-
  country_pattern_df %>%
  tidyr::drop_na(country) %>%
  rbind(country_missing_mod)

#   ____________________________________________________________________________
#   Extracting String containing capitals (ct.:)                            ####

capital_pattern <-
  stringr::regex("
  (?<=ct(\\.:|:))[^|]*
                 ", comments = TRUE)
split_pattern <-
  stringr::regex("
    ,         # Match a comma
    (?!       # only if it's not followed by...
    [^\\(]*     # any number of characters except opening parens
    \\)       # followed by a closing parens
    ).*         # End of lookahead
              ", comments = TRUE)

stop_words_pattern <-
  c(
    "t\\.[:]?", "[dD]yn\\.", "dep\\.", "nomin\\.", "Dynasty", "British",
    "Dominion", "Republic", "French"
  ) %>%
  paste0(collapse = ".*|")

admin_levels_mod <-
  admin_unite %>%
  dplyr::mutate(
    cities = stringr::str_extract(capitals, capital_pattern),
    cities_mod = stringr::str_remove(cities, stop_words_pattern),
    cities_com = stringr::str_remove(cities_mod, split_pattern),
    cities_paren = stringr::str_remove(cities_com, "(?<=\\)).*")
  ) %>%
  cbind(country = country_pattern_df_mod$country)

capital_df <-
  admin_levels_mod %>%
  dplyr::select(id_unique, country, cities_paren) %>%
  tidyr::drop_na(cities_paren) %>%
  string_squish()

# dplyr::distinct(cities_paren, .keep_all = TRUE) %>%


capital_df_mod <-
  capital_df %>%
  dplyr::mutate(
    cities_paren = stringr::str_replace(cities_paren, "\\.\\.", "00"),
    # First: removing dates like 1.Jan.1580. This has to be done first so that
    # no "orphaned" date parts remain like '.Jan'.
    capital1 = stringr::str_replace(cities_paren, "\\d+\\.[A-z]+\\.\\d+", ""),
    # Next, removing approximate dates like 'c.1870'
    capital2 = stringr::str_replace(capital1, "[A-z]\\.\\d+", ""),
    capital3 = stringr::str_replace(capital2, "\\d{3,4}-\\d{2,4}", ""),
    capital4 = stringr::str_replace(capital3, "\\d{3,4}\\/\\d{2,4}", ""),
    capital5 = stringr::str_replace_all(capital4, "\\d+", "")
  )

library(dplyr)

capital_df_final <-
  capital_df_mod %>%
  dplyr::select(id_unique, country, capital = capital5) %>%
  string_squish() %>%
  dplyr::mutate(
    capital_paren = stringr::str_extract(capital, "(?<=\\().*(?=\\))"),
    capital = stringr::str_replace(capital, "\\(.*", ""),
    capital_slash = stringr::str_extract(capital, "(?<=\\/).*"),
    capital = stringr::str_replace(capital, "\\/.*", ""),
    capital_slash = dplyr::if_else(
      is.na(capital_slash),
      stringr::str_extract(capital_paren, "(?<=\\/)[A-z-'´`\\/ ]+"),
      capital_slash
    ),
    capital_paren = stringr::str_replace(capital_paren, "\\/.*", "")
  ) %>%
  tidyr::drop_na(country) %>%
  dplyr::group_by(country, capital, capital_paren, capital_slash) %>%
  dplyr::mutate(
    id_capital = dplyr::cur_group_id()
  ) %>%
  dplyr::ungroup() %>%
  string_squish() %>%
  dplyr::mutate(
    dplyr::across(
      .cols = all_of(c("capital", "capital_paren", "capital_slash")),
      .fns = function(x) {
        stringr::str_replace(x, "\\-\\s.*", "") %>%
          stringr::str_replace("^[A-z]$", "") %>%
          stringr::str_replace("^A\\.A\\.$", "Addis Ababa") %>%
          stringr::str_replace("n\\.", "")
      }
    )
  ) %>%
  mutate_all(list(~ na_if(., "")))

capital_df_sub <-
  capital_df_final %>%
  dplyr::distinct(id_capital, .keep_all = TRUE)

capital_df_sub$cities_match_main <- NA_character_
capital_df_sub$main_str_dist <- NA_real_
capital_df_sub$main_match_lat <- NA_real_
capital_df_sub$main_match_lon <- NA_real_

capital_df_sub$cities_match_paren <- NA_character_
capital_df_sub$paren_str_dist <- NA_real_
capital_df_sub$paren_match_lat <- NA_real_
capital_df_sub$paren_match_lon <- NA_real_

capital_df_sub$cities_match_slash <- NA_character_
capital_df_sub$slash_str_dist <- NA_real_
capital_df_sub$slash_match_lat <- NA_real_
capital_df_sub$slash_match_lon <- NA_real_




# t1 <- capital_df_sub %>%
#   dplyr::mutate(
#     dplyr::across(
#       .cols = all_of(c("capital", "capital_paren", "capital_slash")),
#       .fns = function(x) {
#         stringr::str_replace(x, "\\-\\s.*", "") %>%
#           stringr::str_replace("^[A-z]$", "") %>%
#           stringr::str_replace("^A\\.A\\.$", "Addis Ababa") %>%
#           stringr::str_replace(country_pattern, "")
#       }
#     )
#   )

# %>%
#   dplyr::select(-c(id_unique, id_capital)) %>%
#   # needs to be removed after testing
#   dplyr::distinct()




#   ____________________________________________________________________________
#   Allocating Capitals                                                     ####

geonames_africa <- readRDS(find_files("geonames_africa.RDS")) %>%
  tibble::as_tibble()

for (i in 1:nrow(capital_df_sub)) {
  print(i)

  geonames_loop <-
    geonames_africa %>%
    dplyr::filter(country == capital_df_sub$country[i])

  cities_str_dist <- stringdist::stringdist(
    capital_df_sub$capital[i],
    geonames_loop$countryname,
    method = "jw",
    p = 0.1
  ) %>%
    as.data.frame() %>%
    dplyr::rename(str_dist = ".") %>%
    cbind(geonames_loop) %>%
    dplyr::arrange(str_dist)

  capital_df_sub$main_str_dist[i] <- cities_str_dist$str_dist[1]
  capital_df_sub$cities_match_main[i] <- cities_str_dist$countryname[1]
  capital_df_sub$main_match_lat[i] <- cities_str_dist$latitude[1]
  capital_df_sub$main_match_lon[i] <- cities_str_dist$longitude[1]

  if (!is.na(capital_df_sub$capital_paren[i])) {
    cities_str_dist <- stringdist::stringdist(
      capital_df_sub$capital_paren[i],
      geonames_loop$countryname,
      method = "jw",
      p = 0.1
    ) %>%
      as.data.frame() %>%
      dplyr::rename(str_dist = ".") %>%
      cbind(geonames_loop) %>%
      dplyr::arrange(str_dist)

    capital_df_sub$paren_str_dist[i] <- cities_str_dist$str_dist[1]
    capital_df_sub$cities_match_paren[i] <- cities_str_dist$countryname[1]
    capital_df_sub$paren_match_lat[i] <- cities_str_dist$latitude[1]
    capital_df_sub$paren_match_lon[i] <- cities_str_dist$longitude[1]
  }

  if (!is.na(capital_df_sub$capital_slash[i])) {
    cities_str_dist <- stringdist::stringdist(
      capital_df_sub$capital_slash[i],
      geonames_loop$countryname,
      method = "jw",
      p = 0.1
    ) %>%
      as.data.frame() %>%
      dplyr::rename(str_dist = ".") %>%
      cbind(geonames_loop) %>%
      dplyr::arrange(str_dist)

    capital_df_sub$slash_str_dist[i] <- cities_str_dist$str_dist[1]
    capital_df_sub$cities_match_slash[i] <- cities_str_dist$countryname[1]
    capital_df_sub$slash_match_lat[i] <- cities_str_dist$latitude[1]
    capital_df_sub$slash_match_lon[i] <- cities_str_dist$longitude[1]
  }
}


#   ____________________________________________________________________________
#   Selecting capital with smallest string dist                             ####



capital_df_sub_mod <-
  capital_df_sub %>%
  dplyr::mutate(
    paren_str_dist = dplyr::if_else(
      is.na(paren_str_dist),
      1,
      paren_str_dist
    ),
    slash_str_dist = dplyr::if_else(
      is.na(slash_str_dist),
      1,
      slash_str_dist
    ),
    capital_select = dplyr::case_when(
      (main_str_dist <= paren_str_dist) &
        (main_str_dist <= slash_str_dist) ~ cities_match_main,
      (paren_str_dist < main_str_dist) &
        (paren_str_dist <= slash_str_dist) ~ cities_match_paren,
      (slash_str_dist < paren_str_dist) &
        (slash_str_dist < main_str_dist) ~ cities_match_slash,
      TRUE ~ NA_character_
    ),
    capital_latitude = dplyr::case_when(
      (main_str_dist <= paren_str_dist) &
        (main_str_dist <= slash_str_dist) ~ main_match_lat,
      (paren_str_dist < main_str_dist) &
        (paren_str_dist <= slash_str_dist) ~ paren_match_lat,
      (slash_str_dist < paren_str_dist) &
        (slash_str_dist < main_str_dist) ~ slash_match_lat,
      TRUE ~ NA_real_
    ),
    capital_longitude = dplyr::case_when(
      (main_str_dist <= paren_str_dist) &
        (main_str_dist <= slash_str_dist) ~ main_match_lon,
      (paren_str_dist < main_str_dist) &
        (paren_str_dist <= slash_str_dist) ~ paren_match_lon,
      (slash_str_dist < paren_str_dist) &
        (slash_str_dist < main_str_dist) ~ slash_match_lon,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::relocate(capital, capital_select, capital_latitude, capital_longitude)




z_admin_regions_final <-
  capital_df_sub_mod %>%
  dplyr::select(id_capital, capital_select, capital_latitude, capital_longitude) %>%
  dplyr::left_join(capital_df_final, ., by = "id_capital") %>%
  dplyr::distinct(id_unique, .keep_all = TRUE) %>%
  dplyr::select(id_unique,
    capital_main = capital, capital_paren, capital_slash,
    capital_select, capital_latitude,
    capital_longitude,
    country_admin = country
  ) %>%
  dplyr::left_join(admin_princ, ., by = "id_unique") %>%
  dplyr::select(-starts_with("level")) %>%
  dplyr::select(-c(
    unique_index, id_unique, id_old, princ_ind, id_group, region_id, startpage,
    endpage, excel_sheet, excel_row, N, page, original_sort
  )) %>%
  dplyr::rename(country = country_admin, country_tru = country) %>%
  dplyr::relocate(
    id, period, ruler, references, capital_select, capital_main,
    capital_paren, capital_slash, country
  ) %>%
  string_squish() %>%
  dplyr::relocate(country_tru)








#   ____________________________________________________________________________
#   Data Export                                                             ####


Hmisc::label(z_admin_regions_final$id) <- "Identifier, IDs consisting of only three digits represent regents"

Hmisc::label(z_admin_regions_final$period) <- "Period in which the regent reigned"

Hmisc::label(z_admin_regions_final$ruler) <- "Name of the regent"

Hmisc::label(z_admin_regions_final$death) <- "Binary; 1 if it's known that the regent died a natural death, 0 otherwise"

Hmisc::label(z_admin_regions_final$murdered) <- "Binary; 1 if it's known that the regent died a violent death, 0 otherwise"


Hmisc::label(z_admin_regions_final$birthyear_l) <- "Lower estimation bound if the birth year was not exactly clear"

Hmisc::label(z_admin_regions_final$birthyear_u) <- "Upper estimation bound if the birth year was not exactly clear"

Hmisc::label(z_admin_regions_final$deathyear_l) <- "Lower estimation bound if the death year was not exactly clear"

Hmisc::label(z_admin_regions_final$deathyear_u) <- "Upper estimation bound if the death year was not exactly clear"

Hmisc::label(z_admin_regions_final$age_l) <- "Lower estimation bound if the age was not exactly clear"

Hmisc::label(z_admin_regions_final$age_u) <- "Upper estimation bound if the age was not exactly clear"

Hmisc::label(z_admin_regions_final$birthyear_known) <- "Binary; 1 if the birth year of the ruler was known, 0 otherwise"

Hmisc::label(z_admin_regions_final$reign_start) <- "Start of the reigning period of the regent"

Hmisc::label(z_admin_regions_final$reign_end) <- "End of the reigning period of the regent"

Hmisc::label(z_admin_regions_final$reign_length) <- "Duration of the reigning period of the ruler"

Hmisc::label(z_admin_regions_final$decade) <- "Decade in which the regent reigned"

Hmisc::label(z_admin_regions_final$century) <- "Century in which the regent reigned"

Hmisc::label(z_admin_regions_final$capital_select) <- "Capital that was selected"

Hmisc::label(z_admin_regions_final$capital_main) <- "Capital main part"

Hmisc::label(z_admin_regions_final$capital_paren) <- "Capital part inside parenthesis"

Hmisc::label(z_admin_regions_final$capital_slash) <- "Capital part after forward slash"

Hmisc::label(z_admin_regions_final$princ) <- "Principality"

Hmisc::label(z_admin_regions_final) <-
  "Regents of Nations, Volumes 1-5, Book Series by Peter Truhart"


haven::write_dta(z_admin_regions_final, path = "data/regents_of_nations.dta", version = 14)


##  ............................................................................
##  Alternative version with missing values filled                          ####


z_admin_regions_final_fill <-
  z_admin_regions_final %>%
  tidyr::fill(capital_select, capital_main, capital_paren, capital_slash,
    country, capital_latitude, capital_longitude,
    .direction = "down"
  )

haven::write_dta(z_admin_regions_final_fill, path = "data/regents_of_nations_fill.dta", version = 14)
