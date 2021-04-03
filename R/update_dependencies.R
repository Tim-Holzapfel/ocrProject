update_dependencies <- function(main_dir = project_root) {

  requireNamespace("rlang")
  requireNamespace("dplyr")

  unlink("DESCRIPTION")
  usethis::use_description()
  cat("\f")
  deps <- renv::dependencies(path = "R", progress = FALSE) %>%
    dplyr::select("package" = "Package") %>%
    unique() %>%
    dplyr::mutate(
      type = "Imports",
      version = "*"
    ) %>%
    tibble::add_row(
      package = "R", type = "Depends", version = ">= 4.0.0"
    ) %>%
    dplyr::arrange(package)

  invisible(desc::desc_set_deps(deps))
}
