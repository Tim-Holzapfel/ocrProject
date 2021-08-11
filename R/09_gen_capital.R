#' Extracting capital city names
#'
#' @description Generating capital city names
#'
gen_capital <- function() {

  # Merging the "level" fields created before

  admin_regions_orig <-
    gen_admin_regions()

  admin_regions <-
    admin_regions_orig %>%
    tidyr::unite(
      "capitals",
      tidyr::matches("level\\d+"),
      sep = " | ",
      na.rm = TRUE
    )

  # It makes sense to split the task of extracting the name of the capital into
  # multiple parts: first, find the position/location of the ct.: or ct: string,
  # second, locate the end of the string that contains the name of the capital,
  # usually marked by words like dep., t. etc. and at last extract the string
  # (in this case the name of the capital) that lies between the starting and
  # the ending string.

  capitals <-
    admin_regions %>%
    dplyr::mutate(
      cities = stringr::str_squish(
        stringr::str_extract(capitals, "(?<=ct(\\.:|:))[^|]*")
      )
    ) %>%
    dplyr::select(region, country, continent, cities)

  capitals$id <- vctrs::vec_group_id(capitals)

  capitals_sub <-
    capitals %>%
    dplyr::distinct(id, .keep_all = TRUE)

  # Words that indicate the end of the capital substring

  stop_words <-
    c(
      "t\\.[:]?", "[dD]yn\\.", "dep\\.", "nomin\\.", "Dynasty", "British",
      "Dominion", "Republic", "French"
    )

  for (j in stop_words) {
    stop_words_pattern <-
      paste0(" ", j, ".*")

    locate_stop_words <-
      stringi::stri_locate_first_regex(
        capitals_sub$cities,
        stop_words_pattern
      ) %>%
      tibble::as_tibble() %>%
      magrittr::extract("start")

    names(locate_stop_words) <- j

    capitals_sub <- tibble::add_column(capitals_sub, locate_stop_words)
  }


  stop_words_n <- length(stop_words)

  stop_words_subset <- capitals_sub %>% dplyr::select(5:(4 + stop_words_n))

  min_row <- function(x) min(x, na.rm = TRUE)

  suppressWarnings(capitals_sub$min_col <- apply(stop_words_subset, 1, min_row))

  capitals_sub[capitals_sub$min_col == "Inf", "min_col"] <- NA

  capitals_sub <-
    capitals_sub %>%
    dplyr::mutate(
      cities_sub = dplyr::if_else(
        is.na(min_col),
        cities,
        stringr::str_sub(cities, end = min_col)
      ),
      .after = cities
    )

  # Pattern to split capitals by. It is important that commas are ignored that
  # are within brackets.

  split_pattern <-
    stringr::regex("
    ,         # Match a comma
    (?!       # only if it's not followed by...
    [^\\(]*     # any number of characters except opening parens
    \\)       # followed by a closing parens
    )         # End of lookahead
              ", comments = TRUE)

  # In case the capital has changed over time then the individual capitals
  # (usually accompanied by their respective time period) are usually separated
  # by a comma.

  capitals_split <-
    stringr::str_split(capitals_sub$cities_sub, split_pattern, simplify = TRUE) %>%
    trimws() %>%
    tibble::as_tibble(.name_repair = "universal")

  # Renaming the split capitals

  names(capitals_split) <- paste0("capital", seq_len(ncol(capitals_split)))

  capitals_split_id <- cbind(id = capitals_sub$id, capitals_split[, "capital1"])

  subframe <-
    capitals %>%
    dplyr::select(id) %>%
    dplyr::left_join(capitals_split_id, by = "id")


  final_data <- cbind(admin_regions_orig, capital = subframe$capital1)

  return(final_data)
}




# The capitals need "a bit" of tidying before they are ready to be geocoded.
# For example it is reasonably to assume that the string that comes after the
# closing bracket (given, of course that the are brackets in the concerning
# string) does not contain any information relating to the name of the
# capital. Furthermore, information relating to time like p.1550 or c.1490 are
# not relevant and in fact would distort the geocoding and therefore also need
# to be removed.
#
# tidying_capital <- function(x) {
#
#   # removing all characters that come after the closing bracket
#
#   capital <- stringr::str_replace(x, "(?<=\\)).*", "")
#
#   # removing time related information like c.1490 and p.1550
#
#   capital <- stringr::str_replace_all(capital, "[a-z]\\.\\s?\\d{2,4}", "")
#
#   # removing time periods (first time periods should be removed and then
#   # single years as otherwise the combining - would be remain as a leftover)
#
#   capital <- stringr::str_replace_all(capital, "\\d{1,4}-\\d{1,4}", "")
#
#   # removing individual years
#
#   capital <- stringr::str_replace_all(capital, "\\d{1,4}", "")
#
#   # removing leading or trailing white spaces
#
#   capital <- trimws(capital)
# }
#
# # Using constructed function from above and applying it on the dataset
#
# capitals_tidy <- apply(capitals_split, 2, tidying_capital) %>%
#   tibble::as_tibble()
#
# # The brackets that sometimes follow the capital names usually contain an
# # alternative name for the aforementioned capital and experience has shown
# # that these alternative names are usually easier to geocode (meaning that
# # they get found more often by geocoding services like googlemaps or
# # geonames). The same logic applies to names separated by a forward slash like
# # Denab/Fashoda. However, unlike the alternative names inside the brackets it
# # seems that the alternative names following the forward slash are in fact NOT
# # easier to geocode but rather the other way around.
#
# capital1_alt_names <-
#   capitals_tidy %>%
#   dplyr::transmute(
#     capital1_name1 = stringr::str_extract(capital1, "(?<=\\().*(?=\\))"),
#     capital1 = stringr::str_replace(capital1, "\\(.*\\)", "")
#   )
#
# # Same logic as before: split capitals by forward slash to separate "first"
# # name from alternative name.
#
# capital1_alt_names_split <-
#   stringr::str_split(capital1_alt_names$capital1, "/", simplify = TRUE) %>%
#   tibble::as_tibble(.name_repair = "universal")
#
# # as before: renaming the split capitals
#
# names(capital1_alt_names_split) <-
#   c("capital1", paste0("capital1_name", 2:ncol(capital1_alt_names_split)))
# # }
#








# library(rjson)
# library(geonames)
# library(httr)
# library(rjson)
# library(xml2)
#
#
#
#
#
#
# cities_results <-
#   httr::GET(
#     "http://api.geonames.org/search?",
#     query = list(
#       name = "el-Geneina",
#       featureClass = "P",
#       maxRows = 10,
#       username = "tim_lukas"
#     )
#   ) %>%
#   httr::content() %>%
#   rvest::xml_nodes(xpath = "geoname") %>%
#   xml2::as_list() %>%
#   rlist::list.stack()
