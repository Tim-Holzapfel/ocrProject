
overview_environ <- new.env(parent = emptyenv())

#' Generating overview
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

    overview_excel <-
      list.files(
        pattern = "\\.xlsx$",
        recursive = TRUE,
        full.names = TRUE,
        include.dirs = TRUE,
        all.files = TRUE
      ) %>%
      # Exclude files in the testing directory
      stringr::str_subset("test_environment", negate = TRUE) %>%
      # Remove the ./ at the beginning of the string
      stringr::str_sub(start = 3L) %>%
      tibble::as_tibble() %>%
      dplyr::rename(excel_file = value)

    # It's easier to allocate the individual continent_regions to the rulers
    # when the relevant information are stored inside the excel sheets as meta
    # data. The function "gen_meta_data" extracts the meta data from the excel
    # sheets and allocates them accordingly. The meta data of excel files are
    # stored in the description file of the excel file and can be extracted by
    # unzipping the excel file. Since the extracted files have to be stored
    # somewhere, it is convenient to store them in a temporary file that by its
    # nature gets deleted after the script has run.

    temp_dir <- tempdir()

    gen_meta_data <- function(input_path) {
      path <- input_path[1]

      doc <-
        XML::xmlInternalTreeParse(
          unzip(
            path,
            files = "docProps/core.xml",
            exdir = temp_dir
          )
        )

      meta_data <- XML::xmlValue(XML::getNodeSet(doc, "/*/dc:description"))
    }


    ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    ### Start: Test for the existence of metadata                           ####


    meta_error <-
      simpleError("
      Not all of the Excel-files have the necessary meta data attached.
      Remember, the format for the meta data is for example:
                  'Continent: Africa; Region: South Africa'.
                  ")

    meta_data_exist_test <-
      apply(overview_excel, 1, gen_meta_data)

    if (any(lengths(meta_data_exist_test) == 0)) stop(meta_error)


    ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    ### End: Test for the existence of metadata                             ####


    overview_environ$overview <-
      apply(overview_excel, 1, gen_meta_data) %>%
      stringr::str_split(";", simplify = TRUE) %>%
      as.data.frame() %>%
      tibble::as_tibble() %>%
      dplyr::rename(continent = V1, continent_region = V2) %>%
      dplyr::mutate(
        continent = stringr::str_extract(continent, "(?<=Continent:).*"),
        continent_region =
          stringr::str_extract(continent_region, "(?<=Region:).*")
      ) %>%
      cbind(overview_excel) %>%
      dplyr::mutate(
        startpage =
          stringi::stri_extract_last_regex(
            excel_file,
            "(?<=p)\\d+(?=to|-|_)"
          ) %>% as.integer(),
        endpage =
          stringi::stri_extract_last_regex(
            excel_file,
            "(?<=\\d{1,4}(to|-|_))\\d+"
          ) %>% as.integer()
      ) %>%
      dplyr::arrange(continent, startpage) %>%
      # Remove leading, trailing and consecutive white spaces.
      string_squish()
  }

  overview_data <- overview_environ$overview

  return(overview_data)
}
