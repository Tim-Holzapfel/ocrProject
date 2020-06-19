

#' @title Data Export
#'
#' @description Exporting the final dataset to Stata.
#'
#' @keywords internal
#'
#' @import dplyr
#'
export_df_final <- function() {


  df_final <- calculate_death_year() %>%
    select(-c("Page", "Startpage", "Endpage", "Excel_sheet", "Excel_Row", "N"))

  Hmisc::label(df_final$birthyear_l) <-
    "Lower bound if the birth year was estimated"

  Hmisc::label(df_final$birthyear_u) <-
    "Upper bound if the birth year was estimated"

  Hmisc::label(df_final$deathyear_l) <-
    "Lower bound if the death year was estimated"

  Hmisc::label(df_final$deathyear_u) <-
    "Upper bound if the death year was estimated"

  Hmisc::label(df_final$Murdered) <-
    "Binary, 1 if the Ruler was murdered, 0 else"

  Hmisc::label(df_final$Death) <-
    "Binary, 1 if the death year of the Ruler is known, 0 else"

  Hmisc::label(df_final$Period) <- "Period in which the Ruler reigned"


  haven::write_dta(df_final, path = "data/stata/Truhart_data.dta")

  df_final_svenja <- df_final

  df_final_svenja$Ruler <- stringr::str_sub(df_final_svenja$Ruler, 1, 190)

  haven::write_dta(df_final_svenja,
                   path = "data/stata/Truhart_data_svenja.dta", version = 12)

}




