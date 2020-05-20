tryCatch(startup::startup(), error = function(ex) message(".Rprofile error: ", conditionMessage(ex)))

.First <- function() {
  Sys.setlocale(category = "LC_ALL", locale = "English_US")
  Sys.setenv(LANG = "English_US")
}

if (interactive()) {
  suppressMessages(require(devtools))

  suppressMessages(require(usethis))

  suppressMessages(require(reprex))

  suppressMessages(require(magrittr))

  suppressMessages(use_package("readxl"))

  suppressMessages(use_package("stringr"))

  suppressMessages(use_package("stringi"))

  suppressMessages(use_package("dplyr"))

  suppressMessages(use_package("tidyr"))

  suppressMessages(use_package("purrr"))

  suppressMessages(use_package("zoo"))

  suppressMessages(use_package("usethis"))

  suppressMessages(use_lifecycle())

  suppressMessages(use_lifecycle_badge("stable"))

}

options(
  usethis.full_name = "Tim Holzapfel",
  usethis.protocol = "ssh",
  usethis.description = list(
    `Authors@R` = 'person(
      "Tim", "Holzapfel",
      email = "timholzapfel@outlook.com",
      role = c("aut", "cre"),
      comment = c(ORCID = "0000-0002-2019-520X"))',
    License = "CC BY-NC-ND 4.0",
    Version = "0.0.0.9000",
    Language = "en"
  )
)
