

rm(list = ls())


# Modifications for Students ----------------------------------------------

library(stringi)

df <- readRDS("data/02_death_year.RDS")


library(Hmisc)
stata_data <- df %>% select(-c("Excel_file", "N", "Page"))

stata_data$Death <- 0
stata_data$Murdered <- 0

stata_data[which(stri_detect(stata_data$Ruler, regex = c("†"))), "Death"] <- 1
stata_data[which(stri_detect(stata_data$Ruler, regex = c("‡"))), "Murdered"] <- 1

stata_data <- within(stata_data, Age_Ruler_l <- deathyear_l - birthyear_l)

stata_data <- within(stata_data, Age_Ruler_u <- deathyear_u - birthyear_u)



label(stata_data[["ID"]]) <- "ID of the Ruler. Three digits indicate actual person and the rest time periods."

label(stata_data[["Death"]]) <- "Binary, 1 if the death year of the Ruler was known and 0 otherwise"

label(stata_data[["Murdered"]]) <- "Binary, 1 if the Ruler was murdered or commited suicide, 0 otherwise"

label(stata_data[["birthyear_l"]]) <- "Lower estimate of the birthyear if an interval was given"

label(stata_data[["birthyear_u"]]) <- "Upper estimate of the birthyear if an interval was given"

label(stata_data[["deathyear_l"]]) <- "Lower estimate of the deathyear if an interval was given"

label(stata_data[["deathyear_u"]]) <- "Upper estimate of the deathyear if an interval was given"

label(stata_data[["Age_Ruler_l"]]) <- "Lower estimate of the age if time frame is not exact"

label(stata_data[["Age_Ruler_u"]]) <- "Upper estimate of the age if time frame is not exact"



haven::write_dta(stata_data, path = "data/Truhart.dta", version = 14)





stata_svenja <- stata_data
stata_svenja$Ruler <- substr(stata_svenja$Ruler, start = 1, stop = 200)

haven::write_dta(stata_svenja, path = "data/Truhart_Svenja.dta", version = 12)
