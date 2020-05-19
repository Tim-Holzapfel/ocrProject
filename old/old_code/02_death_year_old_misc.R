

deathyear <-
  df_split$Deathyear %>%
  stringi::stri_extract_all_regex("\\d{3,4}", simplify = TRUE) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    V3 = ifelse(V3 == "", V2, V3),
    test = stringr::str_count(V3)
  )


#%>% dplyr::select(V1, V3)


dplyr::mutate(
  base_year = stringi::stri_extract_last_regex(Deathyear, "\\d{3,4}"),
  est_year = stringr::str_extract(Deathyear, "(?<=\\/)\\d+"),
  est_count = stringi::stri_count_regex(Deathyear, "\\d{3,4}"),
  length_est = stringr::str_count(est_year),
  deathyear_u = stringi::stri_sub_replace(
    base_year,
    from = -length_est,
    replacement = est_year) %>% as.integer(),
  deathyear_l = base_year %>% as.integer()
)
# ) %>%
# dplyr::select(deathyear_l, deathyear_u)


deathyear <-
  df_split["Deathyear"] %>%
  dplyr::mutate(
    base_year = stringi::stri_extract_last_regex(Deathyear, "\\d{3,4}"),
    est_year = stringr::str_extract(Deathyear, "(?<=\\/)\\d+"),
    est_count = stringi::stri_count_regex(Deathyear, "\\d{3,4}"),
    length_est = stringr::str_count(est_year),
    deathyear_u = stringi::stri_sub_replace(
      base_year,
      from = -length_est,
      replacement = est_year) %>% as.integer(),
    deathyear_l = base_year %>% as.integer()
  )
# ) %>%
# dplyr::select(deathyear_l, deathyear_u)


deathyear <-
  df_split["Deathyear"] %>%
  dplyr::mutate(
    base_year = stringi::stri_extract_all_regex(
      Deathyear, "\\d{3,4}", simplify = T
    ))


base_year <- stringi::stri_extract_first_regex(string, c("\\d+", "(?<=\\/)\\d+"), simplify = TRUE)



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

deathyear <-
  df_split$Deathyear %>%
  stringi::stri_extract_all_regex("\\d{3,4}", simplify = TRUE) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    V3 = ifelse(V3 == "", V2, V3)
  )











dplyr::mutate(
  V3 = tidyr::replace_na(list(V3 = V2))
)





deathyear$test <- tidyr::unite()
