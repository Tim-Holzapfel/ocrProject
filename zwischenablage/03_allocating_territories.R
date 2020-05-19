

library(magrittr)

rm(list = ls())



# index <- new.env(parent = emptyenv())








preprocess_data <-
  function() {

  df <- calculate_death_year()



    readRDS("data/01_excel_concat.RDS") %>%
    dplyr::select("ID", "Period", "Ruler", "References")

  df$N <- 1:dim(df)[1]

  df$ID <- stringr::str_replace(df$ID, " ", "")

  df$ID_group <- stringr::str_extract(df$ID, "(?<=\\d\\.\\d{3}\\/)\\d{2,3}(?=\\.[0-9])") %>%
    zoo::na.locf(na.rm = F) %>%
    zoo::na.locf(na.rm = F, fromLast = T) %>%
    as.integer()

  return(df)

}


subset_data <- function() {

  df <- preprocess_data()

  index <- stringr::str_which(df$ID, "^[0-9][0-9][0-9]$")

  df_sub <- df %>% dplyr::slice(index)

  df_sub$ID %<>% as.integer()

  return(df_sub)

}






#' Creating index for all observations at the highest level
#'
#' @return
#' @export
#'
#' @examples
create_level5_index <- function() {

  df <- preprocess_data()

  level5_index <- stringr::str_which(df$ID, "([0-9][0-9]\\.[0-9][0-9][0-9])")

  level5 <- df %>% dplyr::slice(level5_index) %>% dplyr::select("ID", "Period", "Ruler", "N", "ID_group")

  level5$ID_start <- stringi::stri_extract_last(level5$ID, regex = "(?<=[0-9][0-9]\\.)[0-9][0-9][0-9]") %>%
    as.integer()

  level5$ID_end <- stringr::str_extract(level5$ID, "(?<=[0-9][0-9]\\.[0-9][0-9][0-9]-)[0-9][0-9][0-9]$") %>%
    as.integer()

  # ID_end cannot be empty, if it is it's being replaced by ID_start

  level5 <- level5 %>%
    within(ID_end[is.na(ID_end)] <- ID_start[is.na(ID_end)]) %>%
    within(Range <- ID_end - ID_start) %>%
    within(order(ID_group, ID_start, -Range))

  # index$level5 <- level5_index

  return(level5)

}

#' Creating index for all level 4 observations
#'
#' @return
#' @export
#'
#' @examples
create_level4_index <- function() {

  df <- preprocess_data()

  level4_index <- stringr::str_which(df$ID, "([0-9][0-9]\\.[0-9][0-9]-[0-9][0-9]$)|([0-9][0-9]\\.[0-9][0-9]$)")

  level4 <- df %>% dplyr::slice(level4_index) %>% dplyr::select("ID", "Period", "Ruler", "N")

  level4$ID_group <- stringi::stri_extract_last(level4$ID, regex = c("[0-9]?[0-9][0-9](?=\\.[0-9][0-9])")) %>%
    as.integer()

  level4$ID_start <- stringi::stri_extract_last(level4$ID, regex = c("(?<=[0-9][0-9]\\.)[0-9][0-9]")) %>%
    as.integer()

  level4$ID_end <- stringr::str_extract(level4$ID, "(?<=[0-9][0-9]\\.[0-9][0-9]-)[0-9][0-9]$") %>%
    as.integer()

  level4 <- level4 %>%
    within(ID_end[is.na(ID_end)] <-  ID_start[is.na(ID_end)]) %>%
    within(Range <- ID_end - ID_start) %>%
    within(order(ID_group, ID_start, -Range))

  # index$level4 <- level4_index

  return(level4)

}


create_level3_index <- function() {

  df <- preprocess_data()

  level3_index <- stringr::str_which(df$ID, "([0-9][0-9]\\.[0-9]-[0-9])|([0-9][0-9]\\.[0-9]$)")

  level3 <- df %>% dplyr::slice(level3_index) %>% dplyr::select("ID", "Period", "Ruler", "N")

  level3$ID_group <- stringi::stri_extract_last(level3$ID, regex = c("[0-9]?[0-9][0-9](?=\\.[0-9])")) %>%
    as.integer()

  level3$ID_start <- stringi::stri_extract_last(level3$ID, regex = c("(?<=[0-9][0-9]\\.)[0-9]")) %>%
    as.integer()

  level3$ID_end <- stringi::stri_extract_last(level3$ID, regex = c("(?<=[0-9][0-9]\\.[0-9]-)[0-9]")) %>%
    as.integer()

  level3 <- level3 %>%
    within(ID_end[is.na(ID_end)] <- ID_start[is.na(ID_end)]) %>%
    within(Range <- ID_end - ID_start) %>%
    within(order(ID_group, ID_start, -Range))

  # index$level3 <- level3_index

  return(level3)
}



create_level2_index <- function() {

  df <- preprocess_data()

  level2_index <- stringr::str_which(df$ID, "^[0-9][0-9]-[0-9][0-9]$")

  level2 <- df %>% dplyr::slice(level2_index) %>% dplyr::select("ID", "Period", "Ruler", "N")

  level2$ID_group <- level2$ID

  level2$ID_start <- substr(level2$ID_group, 1, 2) %>% as.integer()

  level2$ID_end <- substr(level2$ID_group, 4, 5) %>% as.integer()

  level2 <- within(level2, Range <- ID_end - ID_start)

  level2 <- within(level2, order(ID_group, ID_start, -Range))

  # index$level2 <- level2_index

  return(level2)

}



create_level1_index <- function() {

  df <- preprocess_data()

  level1_index <- stringr::str_which(df$ID, "^[0-9][0-9]\\.$")

  level1 <- df %>% dplyr::slice(level1_index) %>% dplyr::select("ID", "Period", "Ruler", "N")

  level1$ID_group <- substr(level1$ID, 1, 2) %>% as.integer()

  # index$level1_index

  return(level1)

}



# level5 <- create_level5_index()
# level4 <- create_level4_index()
# level3 <- create_level3_index()
# level2 <- create_level2_index()
# level1 <- create_level1_index()
# level1 <- level5


level_index <- new.env(parent = emptyenv())

#' Title
#'
#' @return
#' @export
#'
#' @examples
create_unique_index_range <- function() {

  for (j in 3:5) {

    level <- do.call(paste("create_level", j, "_index", sep = ""), list())

    i = 1
    repeat {

      index <- which((level$ID_start >= dplyr::lag(level$ID_start)) & (level$ID_end <= dplyr::lag(level$ID_end)) &
                       (level$ID_group == dplyr::lag(level$ID_group)))

      if (rlang::is_empty(index)) break

      level_index[[paste("level", j, i,  sep = "")]] <- level[-index, ]

      level <- level[index, ]

      i = i + 1

    }

  }

  index_names <- rlang::env_names(level_index)

  return(index_names)

}



#' Title
#'
#' @return
#' @export
#'
#' @examples
assign_admin_to_ruler <- function() {

  df_sub <- subset_data()

  index_names <- create_unique_index_range()

  for (j in index_names) {

    level <- rlang::env_get(level_index, nm = j)

    n <- dim(level)[1]

    index <- logical(length = n) # Initializing empty logical vector

    level_depth <- substr(j, 6, 6) %>%
      as.integer()

    if (level_depth == 5) {
      depth <- 3
    } else if (level_depth == 4) {
      depth <- 2
    } else if (level_depth == 3) {
      depth <- 1
    }

    df_ID <- df_sub$ID %>%
      substr(1, depth) %>%
      as.integer()


    for (i in 1:n) {

      index <- dplyr::between(df_ID, level$ID_start[i], level$ID_end[i]) & (df_sub$ID_group == level$ID_group[i])

      df_sub[index, j] <- level[i, "Ruler"]

    }

  }

  return(df_sub)

}






df_test <- assign_admin_to_ruler()
















































for (i in 1:1000) {

  index <- dplyr::between(df_sub$ID, level$ID_start[i], level$ID_end[i]) & (df_sub$ID_group == level$ID_group[i])

  df_sub[index, "index_test"] <- level[i, "Ruler"]

}














for (i in 1:n) {
  df_ID <- df_admin$ID %>%
    substr(1, depth) %>%
    as.integer()

  index <- with(level, (df_ID %[]% c(ID_start[i], ID_end[i])) & (df_admin$ID_group == ID_group[i])) %>%
    which()

  df_admin[index, admin_level] <- level[i, "Ruler"]
}





















































library(codetools)










codetools::checkUsage(create_level5_index)





level <- new.env(parent = emptyenv())
df_sub <- subset_data()
test <- level5





for (i in 4:5) {

  test <- do.call(paste("create_level", j, "_index", sep = ""), list())


}


  i = 1

  while (nrow(test) != 0) {

    index <- which((test$ID_start >= dplyr::lag(test$ID_start)) & (test$ID_end <= dplyr::lag(test$ID_end)) &
                     (test$ID_group == dplyr::lag(test$ID_group)))

    level[[paste("level", i,  sep = "")]] <- test[-index, ]

    test <- test[index, ]

    print(i)

    i = i + 1

  }


list2env(x = level5, parent = emptyenv())



  test_env <- as.list.environment(level)

  !!! test_env





df_sub$ID >= level5$ID_start[5]



rlang::set_env(level, new_env = .GlobalEnv)



dplyr::between(df_sub$ID, level5$ID_start[5], level5$ID_end[5])





rlang::current_env(level)


rlang::set_env(!!! level)



rlang::env_poke_parent(level, new_env = .GlobalEnv)




  print(depth)

  for (i in 1:n) {
    df_ID <- df_admin$ID %>%
      substr(1, depth) %>%
      as.integer()

    index <- with(level, (df_ID %[]% c(ID_start[i], ID_end[i])) & (df_admin$ID_group == ID_group[i])) %>%
      which()

    df_admin[index, admin_level] <- level[i, "Ruler"]
  }














































environment(create_level1_index)

rlang::as_environment()



test2 <- paste("neu")


eval(test2) <- "test"



eval(parse(text = "neu")) <- "test"










new.env()


















level61 <- test_env$level62

length(test)













  print(depth)

  for (i in 1:n) {
    df_ID <- df_admin$ID %>%
      substr(1, depth) %>%
      as.integer()

    index <- with(level, (df_ID %[]% c(ID_start[i], ID_end[i])) & (df_admin$ID_group == ID_group[i])) %>%
      which()

    df_admin[index, admin_level] <- level[i, "Ruler"]
  }


































































































































































level_overlap <- function(level_input) {

  level <- level_input


  n <- dim(level)[1]


  index <- logical(length = n)


  level_check <- deparse(substitute(level_input)) %>% substr(1, 6) == "level2"


  if (level_check == FALSE) {
    for (i in 1:n) {
      index[i] <- with(
        level,
        (c(ID_start[i], ID_end[i]) %overlaps% c(ID_start[i + 1], ID_end[i + 1]))
        & (ID_group[i] == ID_group[i + 1])
      )
    }
  } else if (level_check == TRUE) {
    for (i in 1:n) {
      index[i] <- with(
        level,
        c(ID_start[i], ID_end[i]) %overlaps% c(ID_start[i + 1], ID_end[i + 1])
      )
    }
  }

  index <- which(index)


  level_sub <- level[index, ]


  level <- level[-index, ]


  eval.parent(substitute(level_input <- level))


  return(level_sub)
}


level_assign <- function(level_input) {

  level <- level_input

  df_admin <- df_sub

  n <- dim(level)[1]

  index <- logical(length = n) # Initializing empty logical vector

  admin_level <- deparse(substitute(level_input))

  level_check <- deparse(substitute(level_input)) %>%
    substr(1, 6) == "level2"

  print(level_check)

  if (level_check == FALSE) {

    level_depth <- deparse(substitute(level_input)) %>%
      substr(6, 6) %>%
      as.integer()

    print(level_depth)


    if (level_depth == 5) {
      depth <- 3
    } else if (level_depth == 4) {
      depth <- 2
    } else if (level_depth == 3) {
      depth <- 1
    }

    print(depth)

    for (i in 1:n) {
      df_ID <- df_admin$ID %>%
        substr(1, depth) %>%
        as.integer()

      index <- with(level, (df_ID %[]% c(ID_start[i], ID_end[i])) & (df_admin$ID_group == ID_group[i])) %>%
        which()

      df_admin[index, admin_level] <- level[i, "Ruler"]
    }
  } else if (level_check == TRUE) {
    for (i in 1:n) {
      index <- with(level, df_admin$ID_group %[]% c(ID_start[i], ID_end[i]))

      df_admin[index, admin_level] <- level[i, "Ruler"]
    }
  }

  eval.parent(substitute(df_sub <- df_admin))

}




# Test Code ---------------------------------------------------------------


level51 <- level_overlap(level5)
level52 <- level_overlap(level51)
level53 <- level_overlap(level52)
level54 <- level_overlap(level53)


level41 <- level_overlap(level4)
level42 <- level_overlap(level41)
level43 <- level_overlap(level42)
level44 <- level_overlap(level43)
level45 <- level_overlap(level44)


level31 <- level_overlap(level3)
level32 <- level_overlap(level31)


level21 <- level_overlap(level2)
level22 <- level_overlap(level21)





level_assign(level5)
level_assign(level51)
level_assign(level52)
level_assign(level53)

level_assign(level4)
level_assign(level41)
level_assign(level42)
level_assign(level43)
level_assign(level44)
level_assign(level45)















df_sub


















df_sub <- level_assign(level2)


test5 <- left_join(df, df_sub[, c("level2", "N")], by = "N")










index <- which(data$ID_group %[]% admin_data[i, c("ID_start", "ID_end")])




















admin_assign <- function(admin_data, df, depth) {

  admin_level_str <- deparse(substitute(admin_data))
  admin_level <- paste("admin", str_extract(admin_level_str, "[0-9]?[0-9]"), sep = "")

  data <- df
  data$admin_level <- NA

  for (i in 1:dim(admin_data)[1]) {

    index <- which(as.double(substr(data$ID, 1, depth)) %[]% admin_data[i, c("ID_start", "ID_end")] &
                     (data$ID_group == admin_data$ID_group[i]))
    data[index, admin_level] = admin_data[i, "Ruler"]

  }

  return(data)

}































overlap_fun_admin0 <- function(level3) {

  admin_level <- level3

  for (i in 1:dim(admin_level)[1]) {
    admin_level[i, "index"] <-  (admin_level[i, c("ID_start", "ID_end")] %overlaps% admin_level[i + 1, c("ID_start", "ID_end")])
    admin_level$index <- replace(admin_level$index, length(admin_level$index), F)
  }

  level4 <- admin_level[which(admin_level$index), ] %>% select(-c("index"))
  admin_level <- admin_level[which(!admin_level$index), ] %>% select(-c("index"))

  eval.parent(substitute(level3 <- admin_level))

  return(level4)

}

assign_admin0 <- function(admin_data, df) {

  admin_level_str <- deparse(substitute(admin_data))
  admin_level <- paste("admin", str_extract(admin_level_str, "[0-9]?[0-9]"), sep = "")

  data <- df
  #data$admin_level <- NA

  for (i in 1:dim(admin_data)[1]) {

    index <- which(data$ID_group %[]% admin_data[i, c("ID_start", "ID_end")])
    data[index, admin_level] = admin_data[i, "Ruler"]

  }

  return(data)

}

overlap_fun <- function(level3) {

  admin_level <- level3

  for (i in 1:dim(admin_level)[1]) {
    admin_level[i, "index"] <-  (admin_level[i, c("ID_start", "ID_end")] %overlaps% admin_level[i + 1, c("ID_start", "ID_end")]) &
      (admin_level[i, "ID_group"] == admin_level[i + 1, "ID_group"])[1]
    admin_level$index <- replace(admin_level$index, length(admin_level$index), F)
  }

  level4 <- admin_level[which(admin_level$index), ] %>% select(-c("index"))
  admin_level <- admin_level[which(!admin_level$index), ] %>% select(-c("index"))

  eval.parent(substitute(level3 <- admin_level))

  return(level4)

}

admin_assign <- function(admin_data, df, depth) {

  admin_level_str <- deparse(substitute(admin_data))
  admin_level <- paste("admin", str_extract(admin_level_str, "[0-9]?[0-9]"), sep = "")

  data <- df
  data$admin_level <- NA

  for (i in 1:dim(admin_data)[1]) {

    index <- which(as.double(substr(data$ID, 1, depth)) %[]% admin_data[i, c("ID_start", "ID_end")] &
                     (data$ID_group == admin_data$ID_group[i]))
    data[index, admin_level] = admin_data[i, "Ruler"]

  }

  return(data)

}

# Administrative Level 00 --------------------------------------------------

for (i in 1:dim(admin_level00)[1]) {
  index <- which(df_sub$ID_group == admin_level00$ID_group[i])
  df_sub[index, "admin00"] <- admin_level00$Ruler[i]
}

# Administrative Level 0 --------------------------------------------------

admin_level01 <- overlap_fun_admin0(admin_level0)
admin_level02 <- overlap_fun_admin0(admin_level01)
admin_level03 <- overlap_fun_admin0(admin_level02)

df_sub <- assign_admin0(admin_level0, df_sub)
df_sub <- assign_admin0(admin_level01, df_sub)
df_sub <- assign_admin0(admin_level02, df_sub)
df_sub <- assign_admin0(admin_level03, df_sub)

# Administrative Level 1 --------------------------------------------------

admin_level11 <- overlap_fun(admin_level1)
admin_level12 <- overlap_fun(admin_level11)
admin_level13 <- overlap_fun(admin_level12)
admin_level14 <- overlap_fun(admin_level13)

df_sub <- admin_assign(admin_level1, df_sub, 1)
df_sub <- admin_assign(admin_level11, df_sub, 1)
df_sub <- admin_assign(admin_level12, df_sub, 1)
df_sub <- admin_assign(admin_level13, df_sub, 1)
df_sub <- admin_assign(admin_level14, df_sub, 1)

# Administrative Level 2 --------------------------------------------------

admin_level21 <- overlap_fun(admin_level2)
admin_level22 <- overlap_fun(admin_level21)
admin_level23 <- overlap_fun(admin_level22)
admin_level24 <- overlap_fun(admin_level23)
admin_level25 <- overlap_fun(admin_level24)

df_sub <- admin_assign(admin_level2, df_sub, 2)
df_sub <- admin_assign(admin_level21, df_sub, 2)
df_sub <- admin_assign(admin_level22, df_sub, 2)
df_sub <- admin_assign(admin_level23, df_sub, 2)
df_sub <- admin_assign(admin_level24, df_sub, 2)

# Administrative Level 3 --------------------------------------------------

admin_level31 <- overlap_fun(admin_level3)
admin_level32 <- overlap_fun(admin_level31)
admin_level33 <- overlap_fun(admin_level32)

df_sub <- admin_assign(admin_level3, df_sub, 3)
df_sub <- admin_assign(admin_level31, df_sub, 3)
df_sub <- admin_assign(admin_level32, df_sub, 3)
df_sub <- admin_assign(admin_level33, df_sub, 3)


# Merging Results ---------------------------------------------------------


index_concat <- c(admin_level0_index, admin_level00_index, admin_level1_index, admin_level2_index, admin_level3_index) %>% sort()
stata_test <- df[-index_concat, ] # subset of data

unassigned <- anti_join(stata_test, df_sub, by = "N")




# stata_merge <- left_join(x = df, y = df_sub[, c(str_subset(names(df_sub), "admin[0-9][0-9]?"), "N")], by = "N")



haven::write_dta(df_sub, "data/03_territories.dta")




test <- str_subset(unassigned$Ruler, "[A-Z][A-Z][A-Z][A-Z][A-z]") %>% as_tibble()























# Old Code ----------------------------------------------------------------



#
# for (i in 1:dim(admin_level2)[1]) {
#   index <- which(as.double(substr(df_sub$ID, 1, 2)) %[]% admin_level2[i, c("ID_start", "ID_end")] &
#                    (df_sub$ID_group == admin_level2$ID_group[i]))
#   print(index)
#   df_sub[index, "admin2"] = admin_level2[i, "Ruler"]
# }
#
#
# df_sub$N2 <- 1:dim(df_sub)[1]
#
# for (i in 1:dim(admin_level2)[1]) {
#   index <- df_sub[which(df_sub$ID_group == admin_level2$ID_group[i]), c("N2", "ID")]
#
#   index <- index[as.double(substr(index$ID, 1, 2))  %[]% admin_level2[i, c("ID_start", "ID_end")], ]
#
#   df_sub[index$N2, ] = admin_level2[i, "Ruler"]
#
#
# }
#
#
#
# for (i in 1:dim(admin_level2)[1]) {
#   admin_level2[i, "index"] <-  (admin_level2[i, c("ID_start", "ID_end")] %overlaps% admin_level2[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level2[i, "ID_group"] == admin_level2[i + 1, "ID_group"])[1]
# }
#
# admin_level21 <- admin_level2[which(admin_level2$index), ] %>% select(-c("index"))
# admin_level2 <- admin_level2[which(!admin_level2$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level21)[1]) {
#   admin_level21[i, "index"] <-  (admin_level21[i, c("ID_start", "ID_end")] %overlaps% admin_level21[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level21[i, "ID_group"] == admin_level21[i + 1, "ID_group"])[1]
# }
#
# admin_level22 <- admin_level21[which(admin_level21$index), ] %>% select(-c("index"))
# admin_level21 <- admin_level21[which(!admin_level21$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level22)[1]) {
#   admin_level22[i, "index"] <-  (admin_level22[i, c("ID_start", "ID_end")] %overlaps% admin_level22[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level22[i, "ID_group"] == admin_level22[i + 1, "ID_group"])[1]
# }
#
# admin_level23 <- admin_level22[which(admin_level22$index), ] %>% select(-c("index"))
# admin_level22 <- admin_level22[which(!admin_level22$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level23)[1]) {
#   admin_level23[i, "index"] <-  (admin_level23[i, c("ID_start", "ID_end")] %overlaps% admin_level23[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level23[i, "ID_group"] == admin_level23[i + 1, "ID_group"])[1]
# }
#
# admin_level24 <- admin_level23[which(admin_level23$index), ] %>% select(-c("index"))
# admin_level23 <- admin_level23[which(!admin_level23$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level24)[1]) {
#   admin_level24[i, "index"] <-  (admin_level24[i, c("ID_start", "ID_end")] %overlaps% admin_level24[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level24[i, "ID_group"] == admin_level24[i + 1, "ID_group"])[1]
# }
#
# admin_level25 <- admin_level24[which(admin_level24$index), ] %>% select(-c("index"))
# admin_level24 <- admin_level24[which(!admin_level24$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level25)[1]) {
#   admin_level25[i, "index"] <-  (admin_level25[i, c("ID_start", "ID_end")] %overlaps% admin_level25[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level25[i, "ID_group"] == admin_level25[i + 1, "ID_group"])[1]
# }
#
#
#
#
#
# for (i in 1:dim(admin_level2)[1]) {
#   index <- which(as.double(substr(df_sub$ID, 1, 2)) %[]% admin_level2[i, c("ID_start", "ID_end")] &
#                    (df_sub$ID_group == admin_level2$ID_group[i]))
#   df_sub[index, "admin2"] = admin_level2[i, "Ruler"]
# }
#
# for (i in 1:dim(admin_level21)[1]) {
#   index <- which(as.double(substr(df_sub$ID, 1, 2)) %[]% admin_level21[i, c("ID_start", "ID_end")] &
#                    (df_sub$ID_group == admin_level21$ID_group[i]))
#   df_sub[index, "admin21"] = admin_level21[i, "Ruler"]
# }
#
#
#
#
# stata_merge <- left_join(x = df, y = df_sub[, c("N", "admin2", "admin21")], by = "N")
#
#
#
#
#
# for (i in 1:dim(admin_level2)[1]) {
#   index =
#     (df_sub$ID_group == admin_level2_un$ID_group[i]) &
#     (as.double(substr(df_sub$ID, 1, 2)) %[]% admin_level2_un[i, c("ID_start", "ID_end")])
#   df_sub[which(index), "admin2"] = admin_level2_un$Ruler[i]
# }
#
#
# stata_merge <- left_join(x = df, y = df_sub[, c("N", "admin2")], by = "N")





# admin level 0 -----------------------------------------------------------

# for (i in 1:dim(admin_level0)[1]) {
#   admin_level0[i, "index"] <-  (admin_level0[i, c("ID_start", "ID_end")] %overlaps% admin_level0[i + 1, c("ID_start", "ID_end")])
#   admin_level0$index <- replace(admin_level0$index, length(admin_level0$index), F)
# }
#
# admin_level01 <- admin_level0[which(admin_level0$index), ] %>% select(-c("index"))
# admin_level0 <- admin_level0[which(!admin_level0$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level01)[1]) {
#   admin_level01[i, "index"] <-  (admin_level01[i, c("ID_start", "ID_end")] %overlaps% admin_level01[i + 1, c("ID_start", "ID_end")])
#   admin_level01$index <- replace(admin_level01$index, length(admin_level01$index), F)
# }
#
# admin_level02 <- admin_level01[which(admin_level01$index), ] %>% select(-c("index"))
# admin_level01 <- admin_level01[which(!admin_level01$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level02)[1]) {
#   admin_level02[i, "index"] <-  (admin_level02[i, c("ID_start", "ID_end")] %overlaps% admin_level02[i + 1, c("ID_start", "ID_end")])
#   admin_level02$index <- replace(admin_level02$index, length(admin_level02$index), F)
# }
#
# admin_level03 <- admin_level02[which(admin_level02$index), ] %>% select(-c("index"))
# admin_level02 <- admin_level02[which(!admin_level02$index), ] %>% select(-c("index"))







# for (i in 1:dim(admin_level3)[1]) {
#   admin_level3[i, "index"] <-  (admin_level3[i, c("ID_start", "ID_end")] %overlaps% admin_level3[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level3[i, "ID_group"] == admin_level3[i + 1, "ID_group"])[1]
# }
#
# admin_level31 <- admin_level3[which(admin_level3$index), ] %>% select(-c("index"))
# admin_level3 <- admin_level3[which(!admin_level3$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level31)[1]) {
#   admin_level31[i, "index"] <-  (admin_level31[i, c("ID_start", "ID_end")] %overlaps% admin_level31[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level31[i, "ID_group"] == admin_level31[i + 1, "ID_group"])[1]
# }
#
# admin_level32 <- admin_level31[which(admin_level31$index), ] %>% select(-c("index"))
# admin_level31 <- admin_level31[which(!admin_level31$index), ] %>% select(-c("index"))
#
# for (i in 1:dim(admin_level32)[1]) {
#   admin_level32[i, "index"] <-  (admin_level32[i, c("ID_start", "ID_end")] %overlaps% admin_level32[i + 1, c("ID_start", "ID_end")]) &
#     (admin_level32[i, "ID_group"] == admin_level32[i + 1, "ID_group"])[1]
# }
#
# admin_level33 <- admin_level32[which(admin_level32$index), ] %>% select(-c("index"))
# admin_level32 <- admin_level32[which(!admin_level32$index), ] %>% select(-c("index"))


# df_sub$admin0 <- NA
#
# for (i in 1:dim(admin_level0)[1]) {
#   index <- which(df_sub$ID_group %[]% admin_level0[i, c("ID_start", "ID_end")])
#   df_sub[index, "admin0"] = admin_level0[i, "Ruler"]
# }
#
# for (i in 1:dim(admin_level01)[1]) {
#   index <- which(df_sub$ID_group %[]% admin_level01[i, c("ID_start", "ID_end")])
#   df_sub[index, "admin01"] = admin_level01[i, "Ruler"]
# }
#
# for (i in 1:dim(admin_level02)[1]) {
#   index <- which(df_sub$ID_group %[]% admin_level02[i, c("ID_start", "ID_end")])
#   df_sub[index, "admin02"] = admin_level02[i, "Ruler"]
# }
#
# for (i in 1:dim(admin_level03)[1]) {
#   index <- which(df_sub$ID_group %[]% admin_level03[i, c("ID_start", "ID_end")])
#   df_sub[index, "admin03"] = admin_level03[i, "Ruler"]
# }



















