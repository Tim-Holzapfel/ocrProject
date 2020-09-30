#' The main 'geoname' table has the following fields
#'
#' @param geonameid integer id of record in geonames database
#'
#' @param name name of geographical point (utf8) varchar(200)
#'
#' @param asciiname name of geographical point in plain ascii characters,
#'   varchar(200)
#'
#' @param alternatenames alternatenames, comma separated, ascii names
#'   automatically transliterated convenience attribute from alternatename
#'   table, varchar(10000)
#'
#' @param latitude latitude in decimal degrees (wgs84)
#'
#' @param longitude longitude in decimal degrees (wgs84)
#'
#' @param featureclass see http://www.geonames.org/export/codes.html, char(1)
#'
#' @param featurecode see http://www.geonames.org/export/codes.html, varchar(10)
#'
#' @param countrycode ISO-3166 2-letter country code, 2 characters
#'
#' @param cc2 alternate country codes, comma separated, ISO-3166# 2-letter
#'   country code, 200 characters
#'
#' @param admin1code fipscode (subject to change to iso code), see exceptions
#'   below, see file admin1Codes.txt for display names of this code; varchar(20)
#'
#' @param admin2code code for the second administrative division, a county in
#'   the US, see file admin2Codes.txt; varchar(80)
#'
#' @param admin3code code for third level administrative division, varchar(20)
#'
#' @param admin4code code for fourth level administrative division, varchar(20)
#'
#' @param population bigint (8 byte int)
#'
#' @param elevation in meters, integer
#'
#' @param dem digital elevation model, srtm3 or gtopo30, average elevation of
#'   3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer.
#'   srtm processed by cgiar/ciat.
#'
#' @param timezone the iana timezone id (see file timeZone.txt) varchar(40)
#'
#' @param modificationdate date of last modification in yyyy-MM-dd format


#' Continent codes
#'
#' @param AF Africa, geonameId = 6255146
#'
#' @param AS Asia, geonameId = 6255147
#'
#' @param EU Europe, geonameId = 6255148
#'
#' @param NA North America, geonameId = 6255149
#'
#' @param OC Oceania, geonameId = 6255151
#'
#' @param SA South America, geonameId = 6255150
#'
#' @param AN Antarctica, geonameId = 6255152



#' feature classes:
#'
#' @param A country, state, region
#'
#' @param H stream, lake
#'
#' @param L parks,area
#'
#' @param P city, village
#'
#' @param R road, railroad
#'
#' @param S spot, building
#'
#' @param T mountain,hill,rock
#'
#' @param U undersea
#'
#' @param V forest,heath


#' A country, state, region,...
#'
#' @description ADM1	first-order administrative division	a primary
#'   administrative division of a country, such as a state in the United States
#'
#' @param ADM1H historical first-order administrative division	a former
#'   first-order administrative division
#'
#' @param ADM2 second-order administrative division	a subdivision of a
#'   first-order administrative division
#'
#' @param ADM2H historical second-order administrative division	a former
#'   second-order administrative division
#'
#' @param ADM3 third-order administrative division	a subdivision of a
#'   second-order administrative division
#'
#' @param ADM3H historical third-order administrative division	a former
#'   third-order administrative division
#'
#' @param ADM4 fourth-order administrative division	a subdivision of a
#'   third-order administrative division
#'
#' @param ADM4H historical fourth-order administrative division	a former
#'   fourth-order administrative division
#'
#' @param ADM5 fifth-order administrative division	a subdivision of a
#'   fourth-order administrative division
#'
#' @param ADM5H historical fifth-order administrative division	a former
#'   fifth-order administrative division
#'
#' @param ADMD administrative division	an administrative division of a country,
#'   undifferentiated as to administrative level
#'
#' @param ADMDH historical administrative division	a former administrative
#'   division of a political entity, undifferentiated as to administrative level
#'
#' @param LTER leased area	a tract of land leased to another country, usually
#'   for military installations
#'
#' @param PCL political entity
#'
#' @param PCLD dependent political entity
#'
#' @param PCLF freely associated state
#'
#' @param PCLH historical political entity	a former political entity
#'
#' @param PCLI independent political entity
#'
#' @param PCLIX section of independent political entity
#'
#' @param PCLS semi-independent political entity
#'
#' @param PRSH parish	an ecclesiastical district
#'
#' @param TERR territory
#'
#' @param ZN zone
#'
#' @param ZNB buffer zone	a zone recognized as a buffer between two nations in
#'   which military presence is minimal or absent



#' P city, village,...
#'
#' @description PPL	populated place	a city, town, village, or other
#'   agglomeration of buildings where people live and work
#'
#' @param PPLA seat of a first-order administrative division	seat of a
#'   first-order administrative division (PPLC takes precedence over PPLA)
#'
#' @param PPLA2 seat of a second-order administrative division
#'
#' @param PPLA3 seat of a third-order administrative division
#'
#' @param PPLA4 seat of a fourth-order administrative division
#'
#' @param PPLA5 seat of a fifth-order administrative division
#'
#' @param PPLC capital of a political entity
#'
#' @param PPLCH historical capital of a political entity	a former capital of a
#'   political entity
#'
#' @param PPLF farm village	a populated place where the population is largely
#'   engaged in agricultural activities
#'
#' @param PPLG seat of government of a political entity
#'
#' @param PPLH historical populated place	a populated place that no longer
#'   exists
#'
#' @param PPLL populated locality	an area similar to a locality but with a small
#'   group of dwellings or other buildings
#'
#' @param PPLQ abandoned populated place
#'
#' @param PPLR religious populated place	a populated place whose population is
#'   largely engaged in religious occupations
#'
#' @param PPLS populated places	cities, towns, villages, or other agglomerations
#'   of buildings where people live and work
#'
#' @param PPLW destroyed populated place	a village, town or city destroyed by a
#'   natural disaster, or by war
#'
#' @param PPLX section of populated place
#'
#' @param STLMT israeli settlement



clear()

library(data.table)

data.table::setDTthreads(4)

geonames_columns <- c(
  "geonameid", "name", "alternatenames", "latitude", "longitude",
  "feature_class", "feature_code", "iso2c", "cc2"
)


geonames_cities <- data.table::fread(
  file = find_files("allCountries.txt"),
  sep = "\t",
  encoding = "UTF-8",
  keepLeadingZeros = TRUE,
  header = FALSE,
  quote = "",
  col.names = geonames_columns,
  select = c(1, 3, 4, 5, 6, 7, 8, 9, 10),
)


geonames_cities_sub <-
  geonames_cities %>%
  dplyr::slice_head(n = 1000)

geonames_sub <-
  geonames_cities[feature_class %in% c("P", "A"), -c("feature_class")]

# removing geonames_cities and starting a "garbage collection" to free up memory
rm(geonames_cities)
gc()

geonames_feature <-
  geonames_sub %>%
  dplyr::select(feature_code)

summarytools::freq(geonames_feature, feature_code, order = "freq")


##  ............................................................................
##  Start: Replacing missing Country Code                                   ####

codelist_com <-
  countrycode::codelist %>%
  dplyr::mutate(
    iso2c = dplyr::if_else(
      is.na(iso2c),
      eurostat,
      iso2c
    )
  ) %>%
  dplyr::select(
    country = country.name.en, iso2c, region, region23
  ) %>%
  tidyr::drop_na(iso2c)

geonames_join <-
  merge(geonames_sub, codelist_com,
    by = "iso2c",
    all.x = TRUE, all.y = FALSE
  )

# Not all the observations have an associated iso2c code attached. For the cases
# in which the iso2 country code is missing an inverse geocoding approach is
# taken.

missing_iso2c <- geonames_join[which(is.na(geonames_join$iso2c)), ] %>%
  dplyr::mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  ) %>%
  sf::st_as_sf(agr = "constant", coords = c("longitude", "latitude"))

sf::st_crs(missing_iso2c) <-
  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

world_coords <- rnaturalearth::ne_countries(returnclass = "sf")

world_countries <- c(world_coords$name, NA_character_)

coords_co <- sf::st_covered_by(missing_iso2c, world_coords) %>%
  lapply(function(x) {
    x <- ifelse(
      length(x) == 0,
      length(world_countries),
      x
    )
  }) %>%
  unlist()


geonames_join[which(is.na(geonames_join$iso2c)), "country"] <-
  world_countries[coords_co]


##  ............................................................................
##  End: Replacing missing Country Code                                     ####

geonames_join[which(geonames_join$alternatenames == ""), "alternatenames"] <-
  geonames_join$name[which(geonames_join$alternatenames == "")]

# removing geonames_sub and starting a "garbage collection" to free up memory
rm(geonames_sub)
gc()

codelist_continent <-
  countrycode::codelist %>%
  dplyr::select(continent, country = country.name.en) %>%
  tidyr::drop_na(continent)

geonames_join <-
  dplyr::left_join(geonames_join, codelist_continent, by = "country")

geonames_africa <-
  geonames_join %>%
  dplyr::filter(continent == "Africa") %>%
  dplyr::arrange(country, name)

geonames_join_sub <-
  geonames_join %>%
  dplyr::slice_head(n = 10000)

rm(geonames_join)
gc()


library(dplyr)

geonames_join_loop_concat <-
  tibble::tibble(
    countryname = character(),
    latitude = numeric(),
    longitude = numeric(),
    country = character()
  )

library(dplyr)

stepsize <- 10000

for (i in seq(from = 1, to = nrow(geonames_africa), by = stepsize)) {

  print(i)

  geonames_join_loop <-
    geonames_africa[i:(i + (stepsize - 1)), ]

  geonames_join_longer <-
    geonames_join_loop$alternatenames %>%
    stringr::str_split(",", simplify = TRUE) %>%
    as.data.frame() %>%
    mutate_all(list(~na_if(.,""))) %>%
    cbind(geonames_join_loop[, c("latitude", "longitude", "country")]) %>%
    tidyr::pivot_longer(
      data = .,
      cols = dplyr::starts_with("V"),
      values_drop_na = TRUE
    ) %>%
    dplyr::filter(xfun::is_ascii(value) ==  TRUE) %>%
    dplyr::select(countryname = value, latitude, longitude, country)

  geonames_join_loop_concat <-
    rbind(geonames_join_loop_concat,
          geonames_join_longer
          )

}

geonames_join_longer_sum <-
  geonames_join_loop_concat %>%
  dplyr::group_by(countryname, country) %>%
  dplyr::summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    .groups = "drop")



saveRDS(geonames_join_longer_sum, "test_environment/data/geonames_africa.RDS", compress = FALSE)



# geonames_cities_final <-
#   geonames_join[, list(
#     name = alternatenames,
#     feature_code,
#     latitude,
#     longitude,
#     country,
#     region,
#     region23
#   )]





(city_result_index <- stringr::str_which(
  geonames_join_longer_sum$countryname, "Ilorin"))

result <- geonames_join_longer_sum[city_result_index, ]








# geonames_join_longer <-
#   geonames_join_sub$alternatenames %>%
#   stringr::str_split(",", simplify = TRUE) %>%
#   as.data.frame() %>%
#   mutate_all(list(~na_if(.,""))) %>%
#   cbind(geonames_join_sub[, c("latitude", "longitude", "country")]) %>%
#   tidyr::pivot_longer(
#     data = .,
#     cols = dplyr::starts_with("V"),
#     values_drop_na = TRUE
#   ) %>%
#   dplyr::filter(xfun::is_ascii(value) ==  TRUE) %>%
#   dplyr::select(countryname = value, latitude, longitude, country) %>%
#   dplyr::arrange(country, countryname)


#
# geonames_join_longer$city1 <-
#   geonames_join_longer$countryname %>%
#   stringi::stri_trans_general("latin-ascii") %>%
#   stringr::str_replace_all(
#     c("\u0027" = "\u0027", "\u2019" = "\u0027", "\u02BC" = "\u0027", "\u00B4" = "\u0027"),
#   )

# geonames_join_longer_sum <-
#   geonames_join_longer %>%
#   dplyr::group_by(countryname, country) %>%
#   dplyr::summarise(
#     latitude = mean(latitude),
#     longitude = mean(longitude),
#     .groups = "drop")



#
#
# geonames_join_longer <-
#   geonames_join$alternatenames %>%
#   stringr::str_split(",", simplify = TRUE) %>%
#   as.data.frame() %>%
#   mutate_all(list(~na_if(.,""))) %>%
#   cbind(geonames_join[, c("latitude", "longitude", "country")]) %>%
#   tidyr::pivot_longer(
#     data = .,
#     cols = dplyr::starts_with("V"),
#     values_drop_na = TRUE
#   ) %>%
#   dplyr::filter(xfun::is_ascii(value) ==  TRUE) %>%
#   dplyr::select(countryname = value, latitude, longitude, country)
#
#
#
#
#
# t1_sum <-
#   t1 %>%
#   dplyr::group_by(countryname, country) %>%
#   dplyr::summarise(latitude = mean(latitude), longitude = mean(longitude))
#
#









# saveRDS(data_cities_mod, file = "output/geonames_cities.RDS", compress = FALSE)

#' The table 'alternate names'
#'
#' @param alternateNameId the id of this alternate name, int
#'
#' @param geonameid geonameId referring to id in table 'geoname', int
#'
#' @param isolanguage iso 639 language code 2- or 3-characters; 4-characters
#'   'post' for postal codes and 'iata','icao' and faac for airport codes,
#'   fr_1793 for French Revolution names,  abbr for abbreviation, link to a
#'   website (mostly to wikipedia), wkdt for the wikidataid, varchar(7)
#'
#' @param alternatename alternate name or name variant, varchar(400)
#'
#' @param isPreferredName '1', if this alternate name is an official/preferred
#'   name
#'
#' @param isShortName '1', if this is a short name like 'California' for 'State
#'   of California'
#'
#' @param isColloquial '1', if this alternate name is a colloquial or slang
#'   term. Example: 'Big Apple' for 'New York'.
#'
#' @param isHistoric '1', if this alternate name is historic and was used in the
#'   past. Example 'Bombay' for 'Mumbai'.
#'
#' @param from from period when the name was used
#'
#' @param to to period when the name was used

alternatenames_col_names <-
  c(
    "alternateNameId", "geonameid", "isolanguage", "alternatename",
    "isPreferredName", "isShortName", "isColloquial", "isHistoric",
    "from", "to"
  )



alternatenames <- data.table::fread(
  file = find_files("alternateNamesV2.txt"),
  sep = "\t",
  encoding = "UTF-8",
  col.names = alternatenames_col_names,
  key = "geonameid"
)




alternatenames <- data.table::fread(
  file = find_files("alternateNamesV2.txt"),
  sep = "\t",
  select = c(2, 3, 4),
  encoding = "UTF-8",
  col.names = c("geonameid", "isolanguage", "alternate_name"),
  key = "geonameid"
)

isolang <-
  alternatenames %>%
  dplyr::distinct(isolanguage, .keep_all = TRUE) %>%
  tidyr::drop_na(isolanguage) %>%
  dplyr::mutate(
    alternate_name = stringi::stri_trans_general(alternate_name, "latin-ascii"),
  )

n_lang <- dim(isolang)[1]

ascii_test_vec <- vector(length = n_lang)

for (i in 1:dim(isolang)[1]) {
  ascii_test_vec[i] <- all(utf8ToInt(isolang$alternate_name[i]) %in% 1:126)
}

# Names of all the languages that are in another writing system (non-ascii) and
# are therefore not useful for matching the Truhart names since Truhart only
# uses ascii

non_ascii_lang <- isolang[which(!ascii_test_vec), ] %>%
  magrittr::extract2("isolanguage") %>%
  c(., "post", "iata", "icao", "faac", "link", "wkdt") # other non-useful information

alternatenames <- alternatenames[isolanguage %notin% non_ascii_lang]

alter_name_df <-
  alternatenames[, .(
    alternate_name = stringi::stri_trans_general(alternate_name, "latin-ascii"),
    geonameid
  )]

alter_name_unique <- unique(alter_name_df)

alternate_sum <- alter_name_unique[, .(alternate_name = toString(alternate_name)), by = geonameid]


#   ____________________________________________________________________________
#   Merging both tables together                                            ####

geonames_final <-
  merge(geonames_sub, alternate_sum,
    by = "geonameid",
    all.x = TRUE, all.y = FALSE
  )


saveRDS(geonames_final, "output/geonames_cities_exp.RDS", compress = FALSE)
