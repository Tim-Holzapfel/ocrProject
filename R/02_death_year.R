#' @import stringr
#' @import stringi
#' @import tidyr
#' @import dplyr

library(magrittr)


rm(list = ls())

index <- new.env(parent = emptyenv())

#' Preprocessing data frame
#'
#' @return
#' @export
#'
#' @examples
preprocess_data <- function() {

  df <- readRDS("data/01_excel_concat.RDS")

  df <- df %>% dplyr::mutate(
    N = 1:dim(df)[1],
    Death = 0,
    Murdered = 0
  )

  df[stringr::str_which(df$Ruler, "†"), "Death"] <- 1

  df[stringr::str_which(df$Ruler, "‡"), "Murdered"] <- 1

  return(df)

}


#' subsetting data frame to only include IDs with three digits
#'
#' @return
#' @export
#'
#' @examples
create_subset_df <- function() {

  df <- preprocess_data()

  # subset of dataframe of IDs with three digits (ruler)

  index <- stringr::str_which(df$ID, "^[0-9][0-9][0-9]$")

  df_sub <- df %>% dplyr::slice(index) %>%
    dplyr::select("ID", "Period", "Ruler", "N", "Death", "Murdered")

  df_sub$ID %<>% as.integer()

  df_sub <- tidyr::drop_na(df_sub, Ruler)

  return(df_sub)

}




#' Calculating the death year
#'
#' @return
#' @export
#'
#' @examples
create_base_frame <- function() {

  df_sub <- create_subset_df()

  # index <- "[†‡]"
  #
  # index_which <- stringr::str_which(df_sub$Ruler, index)
  #
  # Ruler <- df_sub[index_which, "Ruler"] %>% tibble::as_tibble()

  # regular expression to test for the existence of at least a three or four digit
  # number surrounded by opening and closing brackets. The first expression specifies cases
  # in which the cross appeared before the year and the second expression after the year.

  cross_before <- "(\\([^\\(\\)]*[†‡][^†‡\\(\\)]*\\d{3,4}[^\\)\\(]*\\))"

  cross_after <- "(\\([^\\(\\)]*\\d{3,4}[^†‡\\(\\)]*[†‡][^\\)\\(]*\\))"

  index_cross <- which(stringr::str_detect(df_sub$Ruler, cross_after) | stringr::str_detect(df_sub$Ruler, cross_before))

  # subsetting the data frame

  df_double <- df_sub %>% dplyr::slice(index_cross)

  df_double$cross_after <- stringr::str_extract(df_double$Ruler, cross_after)

  df_double$cross_before <- stringr::str_extract(df_double$Ruler, cross_before)

  df_double <-  df_double %>%
    dplyr::mutate(cross_before = ifelse(is.na(cross_before), cross_after, cross_before),
                  cross_after = ifelse(is.na(cross_after), cross_before, cross_after)) %>%
    dplyr::select(-cross_after, deathyear = cross_before)

  # The month march, which written in Truhart style using roman numerals as III, was often recognized as 111.

  df_double$deathyear <- stringr::str_replace(df_double$deathyear, "([0-9])\\.(111)\\.([0-9])", "\\1\\.III\\.\\3")


  # Splitting the variable deathyear by single and double cross. If a time period was given, meaning if Truhart specified
  # a birthyear and a deathyer then those are usually seperated by a cross. If the left side of the string contains no
  # year then only the deathyear was given.

  dy_split <- stringi::stri_split_regex(df_double$deathyear, "(†)|(‡)", simplify = TRUE) %>% tibble::as_tibble() %>%
    dplyr::select(Birthyear = V1, Deathyear = V2)

  # index of observations containing a time period (birthyear and deathyear)

  index$by_index <- stringi::stri_detect_regex(dy_split$Birthyear, "\\d{3,4}") %>% which()

  return(df_double)

}




index_div <- new.env(parent = emptyenv())

create_frame_single_deathyear <- function() {

  df_double <- create_base_frame()

  index <- rlang::env_get(index, "by_index")

  # observations for which there exists no double index, hence the deathyear

  df_single <- df_double[-index, ]

  dy <- stringi::stri_extract_all_regex(df_single$deathyear, "\\d{3,4}", simplify = TRUE) %>%
    tibble::as_tibble()

  est_names <- tail(names(dy), -1)

  # Index where the first estimate and the second estimate do not have the same string length.
  # String length in this sense means same number of digits. Diverging number of digits means
  # different centuries and these observations therefore have not been correctly included and
  # are dropped

  for (i in est_names) {

    index_div[[paste(i)]] <- which((dy[[i]] != "") & (stringr::str_length(dy[[i]]) != stringr::str_length(dy$V1)))

  }

  index_div_con <- c(rlang::env_get(index_div, "V2"), rlang::env_get(index_div, "V3"))


  dy <- dy[-index_div_con, ]


  dy[which(dy[, ncol(dy) - 1] != ""), 2] <- dy[which(dy[, ncol(dy) - 1] != ""), 3]

  dy <- dy %>% dplyr::select(Deathyear_l = V1, Deathyear_u = V2, N)

  df_single <- dplyr::left_join(df_single, dy, by = "N")






}


test <- create_frame_single_deathyear()





































create_frame_double <- function() {

  df_double <- create_base_frame() %>% dplyr::slice(rlang::env_get(index, "by_index"))














  %>%
    dplyr::slice(which(year_count == 2)) %>%
    dplyr::select(-year_count)


  # Splitting the variable deathyear by "-" to divide it into birthyears and deathyears. Unfortunately, the "-"
  # is not included all the time. Dropping observations which do not have a "-".

  dy_split <- stringi::stri_split_regex(df_double$deathyear, "(?<=[^A-z])-(?=[^A-z])", simplify = TRUE) %>%
    tibble::as_tibble() %>%
    cbind(df_double[, "N"]) %>%
    dplyr::select(Birthyear = V1, Deathyear = V2, N) %>%
    dplyr::filter(Deathyear != "")






  test1 <- dplyr::filter(dy_split, Deathyear != "")




    dplyr::filter((!is.na(Birthyear) & !is.na(Deathyear)))








  birthyear <- stringi::stri_extract_all_regex(dy_split$Birthyear, "\\d{3,4}", simplify = TRUE) %>%
    tibble::as_tibble() %>% cbind(df_double[, "N"]) %>% dplyr::select(Birthyear_l = V1, Birthyear_u = V2, N)


  df_double <- dplyr::left_join(df_double, birthyear, by = "N")













  test <- cbind(df_double, test) %>% dplyr::filter(or_count == 1) %>% dplyr::select(Birthyear, Deathyear, N)

  birthyear <- stringi::stri_extract_all_regex(test$Birthyear, "\\d{3,4}", simplify = TRUE) %>%
    tibble::as_tibble() %>% cbind(test[, "N"])
  names(birthyear) <- c("Birthyear_l", "Birthyear_u", "N")


  deathyear <- stringi::stri_extract_all_regex(test$Deathyear, "\\d{3,4}", simplify = TRUE) %>%
    tibble::as_tibble() %>% cbind(test[, "N"])
  names(deathyear) <- c("Deathyear_l", "Deathyear_u", "N")



  df_double <- df_double %>% dplyr::filter(or_count == 0)

  test2 <- stringi::stri_extract_all_regex(df_double$deathyear, "\\d{3,4}", simplify = TRUE)




























  deathyear_double <- df_double$deathyear %>%
    stringi::stri_extract_all_regex(pattern = c("\\d{3,4}"), simplify = TRUE)






  colnames(deathyear_double) <- c("Birthyear", "Deathyear", "T")











}





# Important Code ----------------------------------------------------------


# Count of the years inside the brackets. A single year (count = 1) is usually a deathyear
# or, more rarely, a birthyear. A count of 2 usually indicates a birthyear and a deathyear.
# If the count is higher than 3 the observation is dropped. These cases were usually selected by mistake

df_double$year_count <- stringr::str_count(df_double$deathyear, "[0-9]?[0-9][0-9][0-9]\\/?[0-9]?[0-9]?[0-9]?[0-9]?")

# the year count for deathyears which include an "o." which is short for "or", a " 0." which is an "o." that was
# mistakenly recognized as a zero or an "/or" needs to be adjusted. The occurence count of these "or" will simply
# be substracted from the year count. Special attention needs to be paid for those cases where the year count is 1
# and the occurence count of "or" is also 1 or higher. In these cases something else was mistaken for an "or" and
# year counts of 1 are therefore excluded from the substraction

df_double$or_count <- stringr::str_count(df_double$deathyear, "(o\\.)|( 0\\.)|(/or)")

index_sub <- which((df_double$or_count > 0) & (df_double$year_count > 1))

df_double[index_sub, "year_count"] <- df_double[index_sub, "year_count"] - df_double[index_sub, "or_count"]

df_double <- df_double %>% dplyr::slice(which(df_double$year_count %in% c(1, 2)))



























































#' Year of death known, year of birth not known
#'
#' @return
#' @export
#'
#' @examples
create_time_frame_single_dy <- function() {

  df_single_dy <- create_deathyear_index()

  df_single_dy <- df_single_dy[which(df_single_dy$period_count == 1), ]

  df_single_dy$deathyear_l <- df_single_dy$deathyear %>% stringi::stri_extract(regex = "([0-9][0-9][0-9][0-9]?)")

  df_single_dy$deathyear_u <- df_single_dy$deathyear %>%
    stringi::stri_extract(regex = "([0-9][0-9][0-9][0-9]?\\/?[0-9]?[0-9]?)") %>%
    stringr::str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9]$)", "\\1\\2\\3\\5") %>%
    stringr::str_replace("([0-9])([0-9])([0-9])([0-9])\\/([0-9])([0-9])", "\\1\\2\\5\\6")

  return(df_single_dy)

}





#' Year of birth known, year of death not known
#'
#' @return
#' @export
#'
#' @examples
create_time_frame_single_by <- function() {

  df_sub <- create_subset_df()

  pattern <- "(\\([^\\(\\)]*\\d{3,4}-$)"

  # data frame single birthyear

  index_by <- stringr::str_which(df_sub$Ruler, pattern)

  # Remark: the estimated age could maybe be interpolated by using the
  # given birth year and the time period the ruler ruled in

  df_single_by <- df_sub %>% dplyr::slice(index_by)

  # subsetting the data frame to only include ruler with a birth year

  df_single_by$birthyear <- df_single_by$Ruler %>% stringi::stri_extract(regex = pattern)

  # extracting the birth year

  df_single_by$birthyear_l <- df_single_by$birthyear %>% stringi::stri_extract_last(regex = "\\d{3,4}(?=-)")

  return(df_single_by)

}





#' Year of death and year of birth known
#'
#' @return
#' @export
#'
#' @examples
create_time_frame_double <- function() {

  df_double <- create_deathyear_index()

  df_double <- df_double[which(df_double$period_count == 2), ]








}

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


























































































# Year of death and year of birth known -----------------------------------



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
