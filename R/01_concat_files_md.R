#' @import tibble
#' @import stringr
#' @import stringi
#' @import readxl
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @import DescTools



rm(list = ls())

#' excel_count
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
excel_count <- function(x) {

  dim(readxl::read_xlsx(
    path = paste(x[[1]]),
    sheet = 1,
    col_names = F,
    col_types = "text",
    .name_repair = "minimal"
  ))[1]
  }


#' desc_file
#'
#' @param input
#'
#' @return
#' @export
#'
#' @examples
desc_file <- function() {

  # Combining the individual excel files into one R file

  excel_files <- list.files(
    path = "data/Excel",
    pattern = "xlsx",
    recursive = T,
    full.names = T
  ) %>% tibble::as_tibble()

  # Startpage and Endpage of Excel files

  Pages <- stringi::stri_extract_all(
    stringi::stri_extract(excel_files[[1]], regex = c("[0-9]+(?:to|-)[0-9]+")),
    regex = "[0-9]+", simplify = T
  )

  # Region and Continent of the excel files

  Location <- stringi::stri_extract_all(
    excel_files$value,
    regex = "(?<=\\/)[^\\/\\(]*",
    simplify = T
  )[, 2:3] %>% trimws()


  excel_files %<>% dplyr::mutate(
    Startpage = Pages[, 1] %>% as.integer(),
    Endpage = Pages[, 2] %>% as.integer(),
    Continent = Location[, 1],
    Region = Location[, 2],
    Excel_file = stringi::stri_extract_last(excel_files[[1]], regex = c("(?<=\\/)[^\\/]*"))
  )

  excel_files <- excel_files[order(excel_files$Continent, excel_files$Startpage, decreasing = F), ]

  # Counting the number of entries (rows) per Excel-file

  excel_files$count_entries <- apply(excel_files, 1, excel_count)

  excel_files$cumsum <- cumsum(excel_files$count_entries)

  return(excel_files)

}



#' concat_excel
#'
#' @param path_input The list pointing to the path of the excel files that are to be merged
#'
#' @return
#' @export
#'
#' @examples
concat_excel <- function(desc_path, desc_continent, desc_startpage, desc_endpage) {

  sheet <- readxl::read_xlsx(
    path = desc_path,
    col_names = FALSE,
    sheet = 1,
    col_types = "text",
    trim_ws = TRUE,
    .name_repair = "minimal"
  )

  # Not all of the excel files contain a "References" column, since this column was only implemented
  # later on

  if (length(names(sheet)) <= 3) {
    sheet$References <- as.character(NA)
  }

  names(sheet) <- c("ID", "Period", "Ruler", "References")

  sheet$Excel_path <- desc_path

  sheet$Continent <- desc_continent

  sheet$Startpage <- desc_startpage

  sheet$Endpage <- desc_endpage

  return(sheet)

}

#' create_sheet
#'
#' @param path_input
#'
#' @return
#' @export
#'
#' @examples
create_sheet <- function() {

  file_desc <- desc_file()


  input_list <- list(
    desc_path = file_desc[["value"]],
    desc_continent = file_desc[["Continent"]],
    desc_startpage = file_desc[["Startpage"]],
    desc_endpage =  file_desc[["Endpage"]]
  )


  sheet <- purrr::pmap_dfr(input_list, concat_excel)

  sheet$N <- 1:dim(sheet)[1]

  return(sheet)

}


#' create_header
#'
#' @param path_input
#'
#' @return
#' @export
#'
#' @examples
create_header <- function() {

  sheet <- create_sheet()

  header <- sheet %>%
    tidyr::unite("Ruler_ref", c("Ruler", "References"), na.rm = T, remove = F, sep = " ")

  # By construction "unite" removes the word or placeholder NA and leaves the entry empty. This can lead to problems,
  # therefore blank cells are replaced with NA.

  header[which(header$Ruler_ref == ""), "Ruler_ref"] <- NA

  index_empty <- header %>%
    with(
      which(
        (!is.na(ID) & is.na(Period) & is.na(Ruler_ref)) |
          (is.na(ID) & is.na(Period) & !is.na(Ruler_ref))
      )
    )

  header <- header %>% dplyr::slice(index_empty) %>%
    dplyr::select(Region = ID, Country = Ruler_ref, N, Excel_path, Continent, Startpage, Endpage)


  header %<>% dplyr::filter(
    (stringi::stri_detect_regex(Country, "[A-z ]+[0-9]+$") |
       stringi::stri_detect_regex(Region, "^[0-9]+[A-z ]+"))
  )

  # Header on the left side: Number is explicitly extracted from the right to prevent missmatches

  header$header_right <- stringi::stri_extract_last(header$Country, regex = c("[0-9]+"))

  # Header on the right side: Number is explicitly extracted from the left to prevent missmatches

  header$header_left <- stringi::stri_extract_first(header$Region, regex = c("[0-9]+"))

  header <- tidyr::unite(header, "Page", c("header_right", "header_left"), na.rm = TRUE, remove = TRUE)

  # Page should only be formatted to integer after the left and right header have been united

  header$Page %<>% as.integer()

}

#
# excel_files <- desc_file()
#
# setion <- create_sheet()
#
# header <- create_header()
#
#
#
#
#
#
#
#
#
#
# # Once the number has been extracted from the left adn right header it can or should be removed
#
# header$Region <- stringr::str_remove(header$Region, "[0-9]+") %>% trimws()
#
# header$Country <- stringr::str_remove(header$Country, "[0-9]+") %>% trimws()
#
# header <- header[order(header$Continent, header$Page, decreasing = FALSE), ]
#
# return(header)
#
#
#
#
#
#
#
# assertthat::validate_that()
#
#
#
# library(DescTools)
#
#
# all(header$Page %[]% c(header$Startpage, header$Endpage))
#
#
#
#
# index <- !(header$Page >= header$Startpage & header$Page <= header$Endpage)
#
# header2 <- header[index, ]
#
#
#
#
#
#
#
#
# header$Page %[]% c(header$Startpage, header$Endpage)
#
# header$Page[1] %[]% c(header$Startpage[1], header$Endpage[1])
#




#
#
#
#   sheet %<>% left_join(
#     headers %>%
#       select("Region", "Country", "N", "Page", "Excel_file"),
#     by = "N"
#   ) %>% select(-c("Ruler_ref"))
#
#
#   sheet$Page %<>%
#     zoo::na.locf() %>%
#     as.integer()
#
#
#   sheet <- sheet[order(sheet$Page), ]
#
#
#   sheet$Country %<>%
#     zoo::na.locf(na.rm = F) %>%
#     zoo::na.locf(na.rm = F, fromLast = T) %>%
#     str_squish()
#
#
#   sheet$Region %<>%
#     zoo::na.locf(na.rm = F) %>%
#     zoo::na.locf(na.rm = F, fromLast = T) %>%
#     str_squish()
#
#
#   sheet$Excel_file %<>%
#     zoo::na.locf()
#
#
#   sheet$ID %<>%
#     str_squish()
#
#
#   sheet$Period %<>%
#     str_squish()
#
#
#   sheet$Ruler %<>%
#     str_squish()
#
#
#   sheet <- sheet[-index_empty, ]
#
#
#   header_sheet <- paste("header_sheet", i, sep = "")
#
#
#   excel_sheet <- paste("excel_sheet", i, sep = "")
#
#
#   assign(header_sheet, headers)
#
#
#   assign(excel_sheet, sheet)
#
#
#
#
#
#
#
#
#
































#
#
#
#
#
# # Modifying Excel Sheets --------------------------------------------------
#
#
#
#
#
#   if (length(names(sheet)) <= 3) {
#     sheet$References <- as.character(NA)
#   }
#
#   names(sheet) <- c("ID", "Period", "Ruler", "References")
#
#   sheet$N <- 1:dim(sheet)[1]
#
#   sheet <- sheet %>% unite("Ruler_ref", c("Ruler", "References"), na.rm = T, remove = F, sep = " ")
#
#   sheet[which(sheet$Ruler_ref == ""), "Ruler_ref"] <- NA
#
#
#   index_empty <- sheet %>%
#     select("ID", "Period", "Ruler_ref") %>%
#     with(
#       which(
#         (!is.na(ID) & is.na(Period) & is.na(Ruler_ref)) |
#           (is.na(ID) & is.na(Period) & !is.na(Ruler_ref))
#       )
#     )
#
#   index_empty %>%
#     sheet[., ] %>%
#     select(c("ID", "Ruler_ref", "N")) %>%
#     magrittr::set_names(c("Region", "Country", "N")) -> headers
#
#
#   headers %<>% dplyr::filter(
#     (stri_detect_regex(Country, "[A-z ]+[0-9]+$") |
#        stri_detect_regex(Region, "^[0-9]+[A-z ]+"))
#   )
#
#
#   headers$Page <- unite(headers, "Page", c("Region", "Country"), na.rm = T) %>%
#     `[[`("Page") %>%
#     str_extract("[0-9]+") %>%
#     as.integer()
#
#
#   headers <- tibble::tibble(
#     Page = headers$Page,
#     N = headers$N,
#     Excel_file = excel_files$Excel_file[i],
#     Path = excel_files$value[i],
#     Sheet = i,
#     Region = str_remove(headers$Region, "[0-9]+"),
#     Country = str_remove(headers$Country, "[0-9]+"),
#     .name_repair = "minimal"
#   )
#
#
#   sheet %<>% left_join(
#     headers %>%
#       select("Region", "Country", "N", "Page", "Excel_file"),
#     by = "N"
#   ) %>% select(-c("Ruler_ref"))
#
#
#   sheet$Page %<>%
#     zoo::na.locf() %>%
#     as.integer()
#
#
#   sheet <- sheet[order(sheet$Page), ]
#
#
#   sheet$Country %<>%
#     zoo::na.locf(na.rm = F) %>%
#     zoo::na.locf(na.rm = F, fromLast = T) %>%
#     str_squish()
#
#
#   sheet$Region %<>%
#     zoo::na.locf(na.rm = F) %>%
#     zoo::na.locf(na.rm = F, fromLast = T) %>%
#     str_squish()
#
#
#   sheet$Excel_file %<>%
#     zoo::na.locf()
#
#
#   sheet$ID %<>%
#     str_squish()
#
#
#   sheet$Period %<>%
#     str_squish()
#
#
#   sheet$Ruler %<>%
#     str_squish()
#
#
#   sheet <- sheet[-index_empty, ]
#
#
#   header_sheet <- paste("header_sheet", i, sep = "")
#
#
#   excel_sheet <- paste("excel_sheet", i, sep = "")
#
#
#   assign(header_sheet, headers)
#
#
#   assign(excel_sheet, sheet)
# }
#
# header_concat <- lapply(ls(pattern = "header_sheet[0-9]"), get) %>%
#   rlist::list.stack()
#
# rm(list = ls(pattern = "header_sheet[0-9]"))
#
#
# excel_concat <- lapply(ls(pattern = "excel_sheet[0-9]"), get) %>%
#   rlist::list.stack()
#
#
# rm(list = ls(pattern = "excel_sheet[0-9]"))
#
#
#
# # gvt <- gvisTable(excel_concat) %>% plot()
#
#
# excel_concat$N <- 1:dim(excel_concat)[1]
#
#
#
# index_missing <- which(is.na(excel_concat$ID) & is.na(excel_concat$Period) & is.na(excel_concat$Ruler))
#
#
#
# excel_concat <-  excel_concat[-index_missing, ]
#
#
#
#
#
#
# saveRDS(excel_concat, file = "data/01_excel_concat.RDS")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# # Modifying Excel Sheets --------------------------------------------------
#
#
# i <- 17
#
# for (i in 1:dim(excel_files)[1]) {
#   sheet <- readxl::read_xlsx(
#     path = paste(excel_files[i, 1]),
#     col_names = FALSE,
#     sheet = 1,
#     col_types = "text",
#     trim_ws = T,
#     progress = F
#   )
#
#
#   if (length(names(sheet)) <= 3) {
#     sheet$References <- as.character(NA)
#   }
#
#   names(sheet) <- c("ID", "Period", "Ruler", "References")
#
#   sheet$N <- 1:dim(sheet)[1]
#
#   sheet <- sheet %>% unite("Ruler_ref", c("Ruler", "References"), na.rm = T, remove = F, sep = " ")
#
#   sheet[which(sheet$Ruler_ref == ""), "Ruler_ref"] <- NA
#
#
#   index_empty <- sheet %>%
#     select("ID", "Period", "Ruler_ref") %>%
#     with(
#       which(
#         (!is.na(ID) & is.na(Period) & is.na(Ruler_ref)) |
#           (is.na(ID) & is.na(Period) & !is.na(Ruler_ref))
#       )
#     )
#
#   index_empty %>%
#     sheet[., ] %>%
#     select(c("ID", "Ruler_ref", "N")) %>%
#     magrittr::set_names(c("Region", "Country", "N")) -> headers
#
#
#   headers %<>% dplyr::filter(
#     (stri_detect_regex(Country, "[A-z ]+[0-9]+$") |
#       stri_detect_regex(Region, "^[0-9]+[A-z ]+"))
#   )
#
#
#   headers$Page <- unite(headers, "Page", c("Region", "Country"), na.rm = T) %>%
#     `[[`("Page") %>%
#     str_extract("[0-9]+") %>%
#     as.integer()
#
#
#   headers <- tibble::tibble(
#     Page = headers$Page,
#     N = headers$N,
#     Excel_file = excel_files$Excel_file[i],
#     Path = excel_files$value[i],
#     Sheet = i,
#     Region = str_remove(headers$Region, "[0-9]+"),
#     Country = str_remove(headers$Country, "[0-9]+"),
#     .name_repair = "minimal"
#   )
#
#
#   sheet %<>% left_join(
#     headers %>%
#       select("Region", "Country", "N", "Page", "Excel_file"),
#     by = "N"
#   ) %>% select(-c("Ruler_ref"))
#
#
#   sheet$Page %<>%
#     zoo::na.locf() %>%
#     as.integer()
#
#
#   sheet <- sheet[order(sheet$Page), ]
#
#
#   sheet$Country %<>%
#     zoo::na.locf(na.rm = F) %>%
#     zoo::na.locf(na.rm = F, fromLast = T) %>%
#     str_squish()
#
#
#   sheet$Region %<>%
#     zoo::na.locf(na.rm = F) %>%
#     zoo::na.locf(na.rm = F, fromLast = T) %>%
#     str_squish()
#
#
#   sheet$Excel_file %<>%
#     zoo::na.locf()
#
#
#   sheet$ID %<>%
#     str_squish()
#
#
#   sheet$Period %<>%
#     str_squish()
#
#
#   sheet$Ruler %<>%
#     str_squish()
#
#
#   sheet <- sheet[-index_empty, ]
#
#
#   header_sheet <- paste("header_sheet", i, sep = "")
#
#
#   excel_sheet <- paste("excel_sheet", i, sep = "")
#
#
#   assign(header_sheet, headers)
#
#
#   assign(excel_sheet, sheet)
# }
#
# header_concat <- lapply(ls(pattern = "header_sheet[0-9]"), get) %>%
#   rlist::list.stack()
#
# rm(list = ls(pattern = "header_sheet[0-9]"))
#
#
# excel_concat <- lapply(ls(pattern = "excel_sheet[0-9]"), get) %>%
#   rlist::list.stack()
#
#
# rm(list = ls(pattern = "excel_sheet[0-9]"))
#
#
#
# # gvt <- gvisTable(excel_concat) %>% plot()
#
#
# excel_concat$N <- 1:dim(excel_concat)[1]
#
#
#
# index_missing <- which(is.na(excel_concat$ID) & is.na(excel_concat$Period) & is.na(excel_concat$Ruler))
#
#
#
# excel_concat <-  excel_concat[-index_missing, ]
#
#
#
#
#
#
# saveRDS(excel_concat, file = "data/01_excel_concat.RDS")
#
#
#



