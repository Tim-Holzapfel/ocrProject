
country_data_environ <- new.env(parent = emptyenv())

#' Generate Country
#'
#' @description Function for allocating countries to regents.
#'
#' @export
#'
gen_country <- function() {
  if (exists("allocated_countries", where = country_data_environ) == FALSE) {

    #   ____________________________________________________________________________
    #   Subsetting String Pattern                                               ####

    stop_pattern <-
      c("\\(", "ct\\.?:", "dep\\.", "t\\.?:", "States West of Nigeria") %>%
      paste0(collapse = "|") %>%
      paste0("(", ., ").*")

    #   ____________________________________________________________________________
    #   Country Matching Pattern                                                ####

    # Additional countries that are mentioned/used by Truhart but that are not part
    # of the countrycode package.
    additional_countries <-
      c(
        "Erythrea", "Turkestan", "Burma", "Malayan Peninsula",
        "Singapur", "Kalimantan", "Kapunduk", "Hawaii",
        "Mali\\sStates", "Timuktu", "Cote D'Ivoire",
        "ZAIRE", "Congo\\s(Zaire)", "Somaliland"
      )

    country_list <-
      countrycode::codelist %>%
      dplyr::select(country = country.name.en, iso3c) %>%
      replace_empty() %>%
      tidyr::drop_na(iso3c) %>%
      dplyr::mutate(
        country = stringr::str_replace(country, "\\(.*\\)", ""),
        n_chars = nchar(country)
      ) %>%
      string_squish() %>%
      dplyr::arrange(dplyr::desc(n_chars)) %>%
      dplyr::select(-n_chars) %>%
      dplyr::mutate(
        country = dplyr::case_when(
          stringr::str_detect(country, "Congo") ~ "Congo",
          stringr::str_detect(country, "Sudan") ~ "Sudan",
          stringr::str_detect(country, "Hong Kong") ~ "Hong Kong",
          TRUE ~ as.character(country)
        )
      ) %>%
      dplyr::distinct(country, .keep_all = TRUE)

    country_pattern <-
      country_list %>%
      magrittr::extract2("country") %>%
      stringi::stri_trans_general("latin-ascii") %>%
      c(additional_countries) %>%
      paste0("\\b", ., "\\b", collapse = "|") %>%
      paste0("(", ., ")")

    # stringr::str_replace_all(
    #   c("\u0027" = "\u0027", "\u2019" = "\u0027", "\u02BC" = "\u0027", "\u00B4" = "\u0027"),
    # )

    #   ____________________________________________________________________________
    #   Main Workhorse: Extracts the name of the Capital                        ####

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
      ) %>%
        stringi::stri_trans_totitle()

      result_return <- dplyr::if_else(
        (string_count == 1) & (string_length <= 70),
        string_extract,
        NA_character_
      )

      return(result_return)
    }


    #   ____________________________________________________________________________
    #   Implementation of the previously defined functions                      ####


    admin_base_data <-
      gen_capital() %>%
      dplyr::relocate(id, id_group, id_old, unique_index, original_sort)

    # Main purpose is to check whether a group id level exists.
    admin_group_ids <-
      admin_base_data %>%
      dplyr::filter(
        stringr::str_detect(id, "^\\d{3}$", negate = TRUE)
      ) %>%
      dplyr::arrange(id_group, id) %>%
      dplyr::select(id_group, id, period, ruler, page, excel_sheet)

    index_unique <- which(is.na(admin_base_data$unique_index))

    admin_regions <-
      admin_base_data %>%
      dplyr::relocate(country) %>%
      dplyr::mutate(
        country_id = paste0(id_group, ".", id),
        .after = 1
      ) %>%
      dplyr::arrange(id_group, id) %>%
      tidyr::drop_na(unique_index)

    admin_levels <-
      admin_regions %>%
      dplyr::select(dplyr::starts_with(paste0("level", 1:4))) %>%
      tidyr::unite("level", dplyr::everything(), sep = "|", na.rm = TRUE) %>%
      magrittr::extract2("level") %>%
      stringr::str_split("\\|", simplify = TRUE) %>%
      as.data.frame() %>%
      replace_empty() %>%
      dplyr::mutate(dplyr::across(.fns = function(x) {
        stringr::str_replace(x, stop_pattern, "")
      })) %>%
      dplyr::group_by(dplyr::across(dplyr::starts_with("V"))) %>%
      dplyr::mutate(
        group_id = dplyr::cur_group_id()
      ) %>%
      cbind(id = admin_regions$country_id, .)

    admin_levels_sub <-
      admin_levels %>%
      dplyr::distinct(dplyr::across(dplyr::starts_with("V")), .keep_all = TRUE)

    z_results <-
      admin_levels_sub %>%
      plyr::colwise(.fun = result_function)(.) %>%
      tidyr::unite("country", dplyr::everything(), sep = "|", na.rm = TRUE) %>%
      cbind(., admin_levels_sub)

    z_results_fill <-
      z_results %>%
      replace_empty() %>%
      dplyr::mutate(
        id_second = stringr::str_extract(id, "(?<=\\.)\\d+") %>% as.integer(),
        id_first = stringr::str_extract(id, "\\d+(?=\\.)") %>% as.integer(),
        .after = id
      ) %>%
      dplyr::group_by(id_first) %>%
      dplyr::arrange(id_first, id_second) %>%
      tidyr::fill(country, .direction = "downup")


    #   ____________________________________________________________________________
    #   Replace Truhart specific countries with uniform definiton               ####

    capital_select <-
      stringr::str_split(z_results_fill$country, "\\|", simplify = TRUE) %>%
      as.data.frame() %>%
      string_squish() %>%
      dplyr::select(country = V1) %>%
      dplyr::mutate(
        country = dplyr::case_when(
          stringr::str_detect(country, "Malayan Peninsula") ~ "Thailand",
          stringr::str_detect(country, "Timuktu") ~ "Mali",
          stringr::str_detect(country, "Turkestan") ~ "Kazakhstan",
          stringr::str_detect(country, "Zaire") ~ "Congo",
          stringr::str_detect(country, "Erythrea") ~ "Eritrea",
          stringr::str_detect(country, "Kalimantan") ~ "Indonesia",
          stringr::str_detect(country, "Kapunduk") ~ "Indonesia",
          stringr::str_detect(country, "Somaliland") ~ "Somalia",
          stringr::str_detect(country, "Burma") ~ "Myanmar",
          stringr::str_detect(country, "Singapur") ~ "Singapore",
          stringr::str_detect(country, "Hawaii") ~ "United States",
          stringr::str_detect(country, "Cote D'ivoire") ~ "Côte d’Ivoire",
          TRUE ~ as.character(country)
        )
      ) %>%
      cbind(group_id = z_results_fill$group_id) %>%
      dplyr::left_join(., country_list, by = "country") %>%
      dplyr::rename(iso3 = iso3c)

    # unique_countries <-
    #   capital_select %>%
    #   dplyr::select(country) %>%
    #   dplyr::distinct() %>%
    #   tidyr::drop_na()

    #   ____________________________________________________________________________
    #   Combining results                                                       ####

    descriptive_rows <-
      admin_base_data %>%
      dplyr::slice(index_unique) %>%
      dplyr::mutate(iso3 = NA_character_)

    admin_regions_id <-
      admin_regions %>%
      cbind(group_id = admin_levels$group_id) %>%
      dplyr::select(-country) %>%
      dplyr::left_join(., capital_select, by = "group_id") %>%
      dplyr::relocate(country) %>%
      dplyr::select(-c(country_id, group_id)) %>%
      rbind(descriptive_rows)



    rlang::env_bind(country_data_environ, allocated_countries = admin_regions_id)
  }

  allocated_countries <- country_data_environ$allocated_countries

  return(allocated_countries)
}
