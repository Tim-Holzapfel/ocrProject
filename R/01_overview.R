
overview_environ <- new.env(parent = emptyenv())

#' @title Generating overview
#'
#' @importFrom rlang .data
#'
#' @description This function generates an overview of the available files
#'
#' @author Tim Holzapfel
#'
#' @return overview_data
#'
#' @export
#'
gen_overview <- function() {

  # Creating a list of all the available excel files. Important! The rows of the
  # overview function "excel_files" need to be ordered

  # To make the manual adjustment easier it makes sense to couple the
  # excel_files with their pdf counterpart so that the pdf files can be opened
  # faster. The excel and pdf files are joined using the fuzzy join method. To
  # make the strings as comparable as possible, another variable is created that
  # contains only the file name without the path and without the name of the
  # student who created it. The last part is because while most of the pdf files
  # have the name of their creator attached with an underscore this is not
  # always the case for excel-files. After the merge the two additional
  # variables can be dropped again.

  # Only run function if the dataset inside the environment
  # "overview_environ" does not exists. Else skip the computation part to
  # save time.

  if (exists("overview_data", where = overview_environ) == FALSE) {

    # List of available Excel files

    excel_paths <- find_excel_paths(path = project_root)

    # It's easier to allocate the individual continent_regions to the rulers
    # when the relevant information are stored inside the excel sheets as meta
    # data. The function "gen_meta_data" extracts the meta data from the excel
    # sheets and allocates them accordingly. The meta data of excel files are
    # stored in the description file of the excel file and can be extracted by
    # unzipping the excel file. Since the extracted files have to be stored
    # somewhere, it is convenient to store them in a temporary file that by its
    # nature gets deleted after the script has run.

    overview_environ$overview <-
      purrr::map_dfr(excel_paths, gen_meta_data) %>%
      dplyr::mutate(
        startpage =
          stringi::stri_extract_last_regex(
            .data$dir_path,
            "(?<=p)\\d+(?=to|-|_)"
          ) %>% as.integer(),
        endpage =
          stringi::stri_extract_last_regex(
            .data$dir_path,
            "(?<=\\d{1,4}(to|-|_))\\d+"
          ) %>% as.integer(),
        dir_path = normalizePath(.data$dir_path)
      ) %>%
      dplyr::arrange(.data$continent, .data$startpage) %>%
      # Remove leading, trailing and consecutive white spaces.
      string_squish()
  }

  overview_data <- overview_environ$overview

  return(overview_data)
}
