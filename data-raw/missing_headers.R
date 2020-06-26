## code to prepare `missing_headers` dataset goes here

headers <- gen_header(dev_mode = TRUE)

data.table::setDT(headers)

# The diff() function was used at first but it produced some strange numbers at
# some positions so it is safest to calculate the page increment manually
# (meaning without pre-built functions).

# When using zoo::na.locf() the option na.rm has to be set to FALSE. If na.rm is
# set to TRUE then rows with NAs that have no earlier non-missing entry will be
# omitted. This poses a problem if one wants to use zoo::na.rm to create new
# variables because then one would be the amount of leading NAs too short.

headers_corr <-
  headers %>%
  dplyr::relocate(region, country, page, N, startpage, endpage) %>%
  dplyr::arrange(continent, page) %>%
  dplyr::mutate(
    N = 1:dplyr::n(),
    N_lag = c(2:dplyr::n(), NA),
    page_increment = page[N_lag] - page[N],
    page_missing = page + page_increment - 1,
    last_region = zoo::na.locf(region),
    last_country = zoo::na.locf(country, na.rm = FALSE),
    .after = page
  )

missing_headers <-
  headers_corr %>%
  dplyr::filter(
    page_increment != 1
  ) %>%
  dplyr::mutate(
    pdf_page = page - startpage + 1
  ) %>%
  dplyr::select(page_missing, last_region, last_country,
    last_excel_row = excel_row,
    last_pdf_page = pdf_page,
    excel_sheet, pdf_file
  )

DT::datatable(missing_headers,
  options =
    list(paging = FALSE)
)

usethis::use_data(missing_headers, overwrite = TRUE)
