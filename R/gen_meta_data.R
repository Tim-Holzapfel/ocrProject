#' Generate Meta Data
#'
#' @param input_path Path to the excel files
#'
#' @importFrom utils unzip
#'
#' @importFrom rlang .data
#'
#' @return Data frame with a single row and column names and values
#'   corresponding to the meta data found in the respective excel-file.
#'
gen_meta_data <- function(input_path) {

  ##  ........................................................................
  ##  Meta Data Errors                                                    ####

  missing_meta_data_error <-
    c(
      "x" = paste0(basename(input_path), " does not contain any meta data."),
      "i" = "Please make sure that the comments field contains meta data delimited by a semicolon.",
      "i" = "The required meta data values are 'continent' and 'continent_region'."
    ) %>%
    rlang::format_error_bullets() %>%
    rlang::error_cnd(message = ., class = "missing_meta_data")

  wrong_meta_data_error <-
    c(
      "x" = paste0(basename(input_path), " contains not the required meta data."),
      "i" = "Remember: the meta data required are 'continent' and 'continent_region' delimited by a semicolon."
    ) %>%
    rlang::format_error_bullets() %>%
    rlang::error_cnd(message = ., class = "wrong_meta_data")

  doc <-
    XML::xmlInternalTreeParse(
      unzip(
        input_path,
        files = "docProps/core.xml",
        exdir = temp_dir
      )
    )

  meta_data <-
    XML::xmlValue(XML::getNodeSet(doc, "/*/cp:keywords"))

  if (sjmisc::is_empty(meta_data)) stop(missing_meta_data_error)

  meta_data_col_names <-
    stringr::str_extract_all(meta_data, "(?<=(^|\\;\\s))[^\\;]*(?=\\:)") %>%
    unlist() %>%
    snakecase::to_snake_case()

  if (!all(meta_data_col_names %in% c("continent", "region", "start_page", "end_page"))) {
    stop(wrong_meta_data_error)
  }

  meta_data_tbl <-
    stringi::stri_extract_all_regex(meta_data, "(?<=\\:)[^\\;\\s]*", simplify = TRUE) %>%
    as.data.frame()

  names(meta_data_tbl) <- meta_data_col_names

  meta_data_tbl_return <-
    meta_data_tbl %>%
    tibble::add_column(dir_path = normalizePath(input_path))



  return(meta_data_tbl_return)
}
