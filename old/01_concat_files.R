#' Import all the excel files
#'
#' @param x The excel files
#' @return A concat table
#' @import magrittr

rm(list = ls())

library(magrittr)
library(tibble)
library(stringi)
library(stringr)
library(haven)
library(here)
library(readxl)


proj_root <- here::here()

setwd(proj_root)



# Function ----------------------------------------------------------------

excel_count <- function(x) {
  dim(readxl::read_xlsx(
    path = paste(x[[1]]),
    sheet = 1,
    col_names = F,
    col_types = "text",
    progress = F
  ))[1]
}


# Generating a list of meta data about the individual excel files ---------

excel_files <- list.files(
  path = "D:/km/Truhart/",
  pattern = "xlsx",
  recursive = T,
  full.names = T
) %>%
  as_tibble()


excel_files[, c("Startpage", "Endpage")] <- stri_extract_all(
  stri_extract(excel_files[[1]], regex = c("[0-9]+(?:to|-)[0-9]+")),
  regex = "[0-9]+", simplify = T
) %>% as.double()


excel_files[, c("Region1", "Region2")] <-
  stri_extract_all(
    excel_files$value,
    regex = "(?<=\\/)[^\\/\\(]*",
    simplify = T
  )[, 4:5]


excel_files$Region1 <- str_replace(excel_files$Region1, " ", "")


excel_files <- excel_files[order(excel_files$Region1, excel_files$Startpage, decreasing = F), ]


excel_files$Excel_file <- stri_extract_last(excel_files[[1]], regex = c("(?<=\\/)[^\\/]*"))


excel_files$count_rows <- apply(excel_files, 1, excel_count)


excel_files$cumsum <- cumsum(excel_files$count_rows)



# Modifying Excel Sheets --------------------------------------------------

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
#
#
#






