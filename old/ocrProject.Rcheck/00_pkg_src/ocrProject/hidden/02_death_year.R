
rm(list = ls())


library(stringi)

df_raw <- readRDS("data/01_excel_concat.RDS")

df_raw$N <- 1:dim(df_raw)[1]


df_raw$Death <- 0
df_raw$Murdered <- 0


df_raw[which(stri_detect(df_raw$Ruler, regex = c("†"))), "Death"] <- 1
df_raw[which(stri_detect(df_raw$Ruler, regex = c("‡"))), "Murdered"] <- 1


pattern <- "(\\([^\\(\\)]*\\d{3,4}[^\\)\\(]*\\))"
# regular expression for a three or for digit number
# surrounded by opening and closing brackets


df <- df_raw$ID %>%
  str_which("^[0-9][0-9][0-9]$") %>%
  df_raw[., ] %>%
  select("ID", "Period", "Ruler", "N", "Death", "Murdered") %>%
  drop_na(Ruler)
# subset of dataframe of IDs with three digits (ruler)


df %>% `$`(Ruler) %>%
  str_count(pattern) %in% c(1, 2) %>%
  which() %>%
  df[., ] -> df
# count of years inside brackets per observation.
# most of the cases include only one bracket with year.
# the observation is dropped if the count is higher than 2 because
# those cases are rather rare but very difficult to allocate.


df <- which(with(df, Death == 1 | Murdered == 1)) %>% df[., ]
# Ruler which were either murdered or for which a death year is given


df$year <- df$Ruler %>%
  stri_extract_last(regex = pattern)
# For those cases with a count of two the first year inside a bracket
# is mostly a year that should not have been included inside a bracket.
# In cases with a count equal to 2 the first bracket is therefore ignored.



# Cases where two periods are rather meant as an estimate than as an interval
index_or <- which(
  (stri_detect(df$year, regex = c("\\s0\\.[0-9]")) |
    stri_detect(df$year, regex = c("\\so\\."))) &
    !stri_detect(df$year, regex = c("-"))
)



df_index <- df[index_or, ]



























df$ID %<>% as.integer()



# index_or <- stri_detect(df$Ruler, regex = c("\\/or"))

# df_index <- df[index_or,]










index <- which(str_count(df$Ruler, pattern) == 1) # Index for count == 1


df$deathyear <- str_extract(df$Ruler, pattern)

df$count <- str_count(df$deathyear, "(\\d{3,4})")

# One Period --------------------------------------------------------------

# df_single <- df[index, ]

# Year of death known, year of birth not known ----------------------------



df_single <- df[df$count == 1, ] %>% select(-c("count"))

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

df_single_split <- df_single %>% select("deathyear_l", "deathyear_u", "N")

df_single_split[c("birthyear_l", "birthyear_u")] <- NA

df_single_split <- df_single_split %>% select("birthyear_l", "birthyear_u", "deathyear_l", "deathyear_u", "N")

df_double_split <- df_double %>% select("birthyear_l", "birthyear_u", "deathyear_l", "deathyear_u", "N")

df_split <- rbind(df_single_split, df_double_split)

df_merge <- left_join(df_raw, df_split, by = "N")


df_merge$birthyear_l %<>% as.integer()
df_merge$birthyear_u %<>% as.integer()

df_merge$deathyear_l %<>% as.integer()
df_merge$deathyear_u %<>% as.integer()


saveRDS(df_merge, file = "data/02_death_year.RDS")


























































