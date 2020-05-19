
rm(list = ls())


df <- readRDS("data/01_excel_concat.RDS")

df$N <- 1:dim(df)[1]
df$ID <- str_replace(df$ID, " ", "")

df$ID_group <- str_extract(df$ID, "(?<=\\d\\.\\d{3}\\/)\\d{2,3}(?=\\.[0-9])") %>%
  zoo::na.locf(na.rm = F) %>% as.double()



# Administrative Level 3 --------------------------------------------------

admin_level3_index <- str_which(df$ID, "([0-9][0-9]\\.[0-9][0-9][0-9])")


admin_level3 <- df[admin_level3_index, ]






















admin_level3$ID_start <- stri_extract_last(admin_level3$ID, regex = c("(?<=[0-9][0-9]\\.)[0-9][0-9][0-9]")) %>% as.double()


admin_level3$ID_end <- str_extract(admin_level3$ID, "(?<=[0-9][0-9]\\.[0-9][0-9][0-9]-)[0-9][0-9][0-9]$") %>% as.double()


admin_level3 <- within(admin_level3, ID_end[is.na(ID_end)] <-  ID_start[is.na(ID_end)])


admin_level3$Range <- admin_level3$ID_end - admin_level3$ID_start


admin_level3 <- within(admin_level3, order(ID_group, ID_start, -Range))

# Administrative Level 2 --------------------------------------------------
#
admin_level2_index <- str_which(df$ID, "([0-9][0-9]\\.[0-9][0-9]-[0-9][0-9]$)|([0-9][0-9]\\.[0-9][0-9]$)")


admin_level2 <- df[admin_level2_index, ] %>% select(-c("ID_group"))


admin_level2$ID_group <- stri_extract_last(admin_level2$ID, regex = c("[0-9]?[0-9][0-9](?=\\.[0-9][0-9])")) %>% as.double()


admin_level2$ID_start <- stri_extract_last(admin_level2$ID, regex = c("(?<=[0-9][0-9]\\.)[0-9][0-9]")) %>% as.double()


admin_level2$ID_end <- str_extract(admin_level2$ID, "(?<=[0-9][0-9]\\.[0-9][0-9]-)[0-9][0-9]$") %>% as.double()


admin_level2 <- within(admin_level2, ID_end[is.na(ID_end)] <-  ID_start[is.na(ID_end)])


admin_level2$Range <- admin_level2$ID_end - admin_level2$ID_start


admin_level2 <- within(admin_level2, order(ID_group, ID_start, -Range))


# Administrative Level 1 --------------------------------------------------

admin_level1_index <- str_which(df$ID, "([0-9][0-9]\\.[0-9]-[0-9])|([0-9][0-9]\\.[0-9]$)")


admin_level1 <- df[admin_level1_index, ] %>% select(-c("ID_group"))


admin_level1$ID_group <- stri_extract_last(admin_level1$ID, regex = c("[0-9]?[0-9][0-9](?=\\.[0-9])")) %>% as.double()


admin_level1$ID_start <- stri_extract_last(admin_level1$ID, regex = c("(?<=[0-9][0-9]\\.)[0-9]")) %>% as.double()


admin_level1$ID_end <- stri_extract_last(admin_level1$ID, regex = c("(?<=[0-9][0-9]\\.[0-9]-)[0-9]")) %>% as.double()


admin_level1 <- within(admin_level1, ID_end[is.na(ID_end)] <-  ID_start[is.na(ID_end)])


admin_level1$Range <- admin_level1$ID_end - admin_level1$ID_start


admin_level1 <- within(admin_level1, order(ID_group, ID_start, -Range))


# Administrative Level 0 --------------------------------------------------

admin_level0_index <- str_which(df$ID, "^[0-9][0-9]-[0-9][0-9]$")


admin_level0 <- df[admin_level0_index, ] %>% select(-c("ID_group"))


admin_level0$ID_group <- admin_level0$ID


admin_level0$ID_start <- substr(admin_level0$ID_group, 1, 2) %>% as.double()


admin_level0$ID_end <- substr(admin_level0$ID_group, 4, 5) %>% as.double()


admin_level0$Range <- admin_level0$ID_end - admin_level0$ID_start


admin_level0 <- within(admin_level0, order(ID_group, ID_start, -Range))


# Administrative Level 00 --------------------------------------------------

admin_level00_index <- str_which(df$ID, "^[0-9][0-9]\\.$")


admin_level00 <- df[admin_level00_index, ] %>% select(-c("ID_group"))


admin_level00$ID_group <- substr(admin_level00$ID, 1, 2) %>% as.double()


# Subsetting Data ---------------------------------------------------------

df_sub <- df[str_which(df$ID, "^[0-9][0-9][0-9]$"), ]


df_sub$ID %<>% as.integer()

# Functions ---------------------------------------------------------------

overlap_fun_admin0 <- function(level1) {

  admin_level <- level1

  for (i in 1:dim(admin_level)[1]) {
    admin_level[i, "index"] <-  (admin_level[i, c("ID_start", "ID_end")] %overlaps% admin_level[i + 1, c("ID_start", "ID_end")])
    admin_level$index <- replace(admin_level$index, length(admin_level$index), F)
  }

  level2 <- admin_level[which(admin_level$index), ] %>% select(-c("index"))
  admin_level <- admin_level[which(!admin_level$index), ] %>% select(-c("index"))

  eval.parent(substitute(level1 <- admin_level))

  return(level2)

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

overlap_fun <- function(level1) {

  admin_level <- level1

  for (i in 1:dim(admin_level)[1]) {
    admin_level[i, "index"] <-  (admin_level[i, c("ID_start", "ID_end")] %overlaps% admin_level[i + 1, c("ID_start", "ID_end")]) &
      (admin_level[i, "ID_group"] == admin_level[i + 1, "ID_group"])[1]
    admin_level$index <- replace(admin_level$index, length(admin_level$index), F)
  }

  level2 <- admin_level[which(admin_level$index), ] %>% select(-c("index"))
  admin_level <- admin_level[which(!admin_level$index), ] %>% select(-c("index"))

  eval.parent(substitute(level1 <- admin_level))

  return(level2)

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



















