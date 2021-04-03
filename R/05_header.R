#' Generate header
#'
#' @description The headers contain useful information i.e. the region and the
#'   country of the ruler. To extract these information, use is made of the fact
#'   that the left contains the region and the right header the country.
#'
#' @author Tim Holzapfel
#'
#' @param dev_mode When set to TRUE stops the execution early and returns the
#'   intermediate dataset required for testing
#'
#' @export
#'
gen_header <- function(dev_mode = FALSE) {

  # Sometimes part of the ruler column was mistakenly put into the Reference
  # column or the other way around, therefore the ruler and Reference columns
  # need to be merged in order to create the correct headers. However, a bit of
  # adjustment is required after the function "unite" from "dplyr" has been
  # called. By construction "unite" removes the word or placeholder NA and
  # leaves the entry empty. This can lead to problems, therefore blank cells are
  # replaced with NA. At the end the variable ruler and ID are renamed to
  # country and region respectively because the ruler column contains the header
  # specifying the country and the ID column contains the header specifying the
  # region.

  base_dataset <- gen_reign_summary()

  header <-
    base_dataset %>%
    tidyr::unite(
      ruler_ref,
      c("ruler", "references"),
      na.rm = TRUE, # Removing NA and replacing it with an empty space
      remove = TRUE, # Removing ruler and references after the merge
      sep = " "
    ) %>%
    dplyr::mutate(
      ruler = dplyr::na_if(ruler_ref, ""), # Replacing "" with NA
      ruler_ref = NULL, # dropping the variable ruler_ref
    ) %>%
    dplyr::rename(country = ruler, region = id)

  # The header index is explicitly created because at the end those rows
  # containing a header need to be removed from the final dataset after relevant
  # columns from the header were assigned. To identify the headers, only those
  # rows are kept in which either the left and middle columns and not the right
  # column were empty or in which the right and middle columns were empty and
  # not the left column.

  header_index <-
    header %>%
    dplyr::select(region, period, country, N) %>%
    dplyr::filter(
      (!is.na(region) & is.na(period) & is.na(country)) |
        (is.na(region) & is.na(period) & !is.na(country))
    ) %>%
    magrittr::extract2("N") %>%
    as.integer()

  # Often a bit of adjustment is necessary in order to make sure that some rows
  # were not selected by mistake. Therefore regular expression are written to
  # make sure that the region column starts with the page number and the country
  # column ends with the page number.

  header_cor <-
    header %>%
    dplyr::slice(header_index) %>%
    # select rows containing the header
    dplyr::filter(
      stringi::stri_detect_regex(region, "^\\d{1,4}\\s?[A-Z][a-z]+") |
        stringi::stri_detect_regex(country, "^[A-Z]{1}[A-z]+.*\\d{1,4}$")
    ) %>%
    dplyr::select(
      region, country, excel_row, N,
      excel_sheet, dplyr::everything(), -period
    ) %>%
    dplyr::filter(
      stringi::stri_detect_regex(country, "[sS]\\.", negate = TRUE) %>%
        stringi::stri_replace_na("TRUE") == "TRUE"
    )

  # It makes the debugging a lot easier when it is immediately clear in which
  # Excel-File the mistakes originated. Otherwise 80% of the work while fixing
  # errors is just identifying the right excel sheet and the relevant row.

  header_final <-
    header_cor %>%
    dplyr::mutate(
      page = stringr::str_extract(region, "^\\d{1,4}"),
      page = ifelse(
        is.na(page),
        stringr::str_extract(country, "\\d{1,4}$"),
        page
      ),
      page = as.integer(page),
      region = stringr::str_remove(region, "^\\d{1,4}"),
      country = stringr::str_remove(country, "\\d{1,4}$")
    ) %>%
    string_squish() %>%
    dplyr::relocate(region, country, N, page, excel_sheet)

  # A lot of the variables needed for testing are not strictly necessary for the
  # final dataset. Furthermore, since the next step is a merge it is important
  # to reduce the number of identical variables in the datasets that are to be
  # merged

  if (dev_mode == TRUE) {
    return(header_final)
  } else if (dev_mode == FALSE) {
    header_final <-
      header_final %>%
      dplyr::select(region, country, N, page)
  }

  # The variables region and country are assigned to the rulers in a "cascading"
  # down manner, meaning the last non-missing value is carried forward. ABBYY,
  # the text recognition program sometimes makes the mistake that it takes the
  # number 1 for the small letter L. This next step corrects for that mistake.

  merge_data_with_header <-
    dplyr::left_join(base_dataset, header_final, by = "N") %>%
    dplyr::relocate(region, country, page, startpage, endpage) %>%
    dplyr::arrange(continent, startpage) %>%
    dplyr::mutate(
      region = zoo::na.locf(region, na.rm = FALSE),
      country = zoo::na.locf(country, na.rm = FALSE),
      country = zoo::na.locf(country, na.rm = FALSE, fromLast = TRUE),
      page = zoo::na.locf(page, na.rm = FALSE),
      id = gsub(" ", "", id), # id can't contain empty spaces except headers!
    ) %>%
    dplyr::slice(-header_index) %>%
    # Remove rows containing the header
    dplyr::arrange(continent, startpage, page) # Very important!

  # The last part, the sorting is very important because the page order is not
  # always correct inside the excel files, meaning that even though the correct
  # order of course would be first page 12 and then page 13 in the excel file
  # the order would be page 13 and then page 12. So the first sorting key should
  # be the continent because the page ranges inside the continents are always
  # unique. Then one should sort for the start page of the excel file and then
  # one should sort for the page (meaning in this case the page of the excel
  # file that was taken from the header). This last sorting part controls for
  # the cases when students changed the original page order.

  return(merge_data_with_header)
}
