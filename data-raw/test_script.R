
clear()

a_gen_overview_test <- gen_overview()

b_gen_base_dataset_test <- gen_base_dataset()

c_gen_deathyear_test <- gen_deathyear()

d_gen_reign_summary_test <- gen_reign_summary()

e_gen_header_test <- gen_header()

f_gen_unique_index_range_test <- gen_unique_index_range()

g_gen_group_id_test <- gen_group_id()

h_gen_admin_regions_test <- gen_admin_regions()

i_gen_country_test <- gen_country()

j_gen_estimation_test <- gen_estimation()



names(j_gen_estimation_test)



t1 <- j_gen_estimation_test %>%
  dplyr::select(-c(
    "id_group", "id", "id_old", "unique_index", "original_sort", "page",
    "startpage", "endpage", "excel_row", "N", "excel_sheet", "period_count",
    "period_contains_dots", "period_dots_replaced", "dots_replaced_true", "region_id"
  )) %>%
  dplyr::relocate(period, ruler, references, country, iso3, continent, continent_region, region)



haven::write_dta(t1, "truhart_data.dta", version = 13)



j <- 2

level <- gen_level2_admin_id() %>%
  dplyr::arrange(admin_id_start, admin_id_end) %>%
  dplyr::filter(admin_id_start <= admin_id_end)

data.table::setDT(level)
data.table::setkey(level, admin_id_start, admin_id_end)


data.table::setkey(level, id_group, admin_id_start, admin_id_end)
level_overlap <- data.table::foverlaps(level, level, which = TRUE) %>%
  dplyr::filter(xid != yid) %>%
  dplyr::mutate(
    lin_com = xid + yid,
    lin_com_inv = abs(xid - yid)
  ) %>%
  dplyr::distinct(lin_com, lin_com_inv, .keep_all = TRUE) %>%
  dplyr::filter(
    lin_com_inv == 1
  ) %>%
  dplyr::mutate(
    long_range_id = dplyr::if_else(
      dplyr::lead(xid) == yid, 1, 0
    )
  ) %>%
  dplyr::filter(
    long_range_id == 0
  )



overlap_index <- which((level_overlap$xid %notin% level_overlap$yid))

level <- level[overlap_index, ]

unique(level_overlap$xid[index])
