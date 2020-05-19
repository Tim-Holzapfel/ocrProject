tryCatch(startup::startup(), error = function(ex) message(".Rprofile error: ", conditionMessage(ex)))

.First <- function() {
  Sys.setlocale(category = "LC_ALL", locale = "English_US")
  Sys.setenv(LANG = "English_US")
}

if (interactive()) {
  suppressMessages(require(devtools))
}

if (interactive()) {
  suppressMessages(require(usethis))
}

if (interactive()) {
  suppressMessages(require(reprex))
}

if (interactive()) {
  suppressMessages(require(magrittr))
}


