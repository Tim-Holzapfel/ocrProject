rm(list = ls())


library(magrittr)
library(stringr)
library(stringi)
library(dplyr)

stata_data <- readRDS("data/01_excel_concat.RDS")



# stata_data <- haven::read_dta(file = "excel_stacked.dta", encoding = "UTF-8")

stata_data$N <- 1:dim(stata_data)[1] # Generating an index variable to merge it later on

pattern <- "(\\([^\\(\\)]*\\d{3,4}[^\\)\\(]*\\))"

df <- stata_data[which((stringr::str_count(stata_data$Ruler, pattern) == 1) & stringr::str_detect(stata_data$ID, "^[0-9][0-9][0-9]$")), ] # all observations in the column Ruler that have at least one three or four digital number inside brackets

df %<>% dplyr::select(-c("References", "Excel_path"))

df$deathyear <- stringr::str_extract(df$Ruler, pattern)

df$count <- stringr::str_count(df$deathyear, "(\\d{3,4})")

# Year of death known, year of birth not known ----------------------------

df_single <- df[df$count == 1, ] %>% dplyr::select(-c("count"))

df_single$deathyear_l <- df_single$deathyear %>% str_extract("([0-9][0-9][0-9][0-9]?)")

df_single$deathyear_u <- df_single$deathyear %>%
  str_extract("([0-9][0-9][0-9][0-9]?\\/?[0-9]?[0-9]?)") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9]$)", "\\1\\2\\3\\5") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9])([0-9])", "\\1\\2\\5\\6")

df_single$deathyear_l <- df_single$deathyear %>% str_extract("([0-9][0-9][0-9][0-9]?)")

# Year of death and year of birth known -----------------------------------

df_double <- df[df$count != 1, ] %>% select(-c("count"))

pattern <- "[0-9]?[0-9][0-9][0-9]\\/?[0-9]?[0-9]?[0-9]?[0-9]?"

df_double <- df_double[which(str_count(df_double$deathyear, pattern) <= 2), ]

# To-do list: some of the observations with only one year (deathyear) accidently got selected into df_double tibble

deathyear_u <- df_double$deathyear %>%
  str_extract_all("[0-9]?[0-9][0-9][0-9]\\/?[0-9]?[0-9]?[0-9]?[0-9]?", simplify = T) %>%
  as_tibble()

names(deathyear_u) <- c("birthyear_l", "deathyear_l")

df_double <- cbind(df_double, deathyear_u)

df_double$birthyear_u <- df_double$birthyear_l %>%
  str_replace("([0-9])([0-9])([0-9])\\/([0-9]$)", "\\1\\2\\4") %>%
  str_replace("([0-9])([0-9])([0-9])\\/([0-9])([0-9]$)", "\\1\\4\\5") %>%
  str_replace("([0-9])([0-9])([0-9])\\/([0-9])([0-9])([0-9]$)", "\\4\\5\\6") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9]$)", "\\1\\2\\3\\5") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9])([0-9]$)", "\\1\\2\\5\\6") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9])([0-9])([0-9]$)", "\\1\\5\\6\\7") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9])([0-9])([0-9])([0-9]$)", "\\5\\6\\7\\8")

df_double$deathyear_u <- df_double$deathyear_l %>%
  str_replace("([0-9])([0-9])([0-9])\\/([0-9]$)", "\\1\\2\\4") %>%
  str_replace("([0-9])([0-9])([0-9])\\/([0-9])([0-9]$)", "\\1\\4\\5") %>%
  str_replace("([0-9])([0-9])([0-9])\\/([0-9])([0-9])([0-9]$)", "\\4\\5\\6") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9]$)", "\\1\\2\\3\\5") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9])([0-9]$)", "\\1\\2\\5\\6") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9])([0-9])([0-9]$)", "\\1\\5\\6\\7") %>%
  str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9])([0-9])([0-9])([0-9]$)", "\\5\\6\\7\\8")

df_double$birthyear_l <- df_double$birthyear_l %>% str_extract("\\d{3,4}")

df_double$deathyear_l <- df_double$deathyear_l %>% str_extract("\\d{3,4}")

# Combining with the original data set ------------------------------------

df_single_split <- select(df_single, c("deathyear_l", "deathyear_u", "N"))

df_single_split[c("birthyear_l", "birthyear_u")] <- NA

df_double_split <- select(df_double, c("deathyear_l", "birthyear_l", "deathyear_u", "birthyear_u", "N"))

df_split <- rbind(df_single_split, df_double_split)

rdata <- left_join(stata_data, df_split, by = "N")





rdata$deathyear_l <- stringi::stri_extract(rdata$deathyear_l, regex = "[0-9]+") %>% as.integer()
rdata$deathyear_u <- stringi::stri_extract(rdata$deathyear_u, regex = "[0-9]+") %>% as.integer()
rdata$birthyear_l <- stringi::stri_extract(rdata$birthyear_l, regex = "[0-9]+") %>% as.integer()
rdata$birthyear_u <- stringi::stri_extract(rdata$birthyear_u, regex = "[0-9]+") %>% as.integer()

rdata$age_l <- rdata$deathyear_l - rdata$birthyear_l
rdata$age_u <- rdata$deathyear_u - rdata$birthyear_u


rdata[which((0 > rdata$age_l ) | (rdata$age_l > 100)), "age_l"] <- NA
rdata[which((0 > rdata$age_u ) | (rdata$age_u > 100)), "age_u"] <- NA

rdata <- rdata %>% select(-Excel_path, -Startpage, -Endpage, -Page, -N)





haven::write_dta(data = rdata, version = 14, path = "data/Truhart.dta")


# Für Svenja:


stata_svenja <- rdata

stata_svenja$Ruler <- substr(stata_svenja$Ruler, 1, 200)

haven::write_dta(data = stata_svenja, version = 12, path = "data/Truhart_svenja.dta")


















