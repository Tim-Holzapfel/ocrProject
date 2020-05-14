
tryCatch(startup::startup(), error = function(ex) message(".Rprofile error: ", conditionMessage(ex)))

.First <- function() {
  Sys.setlocale(category = "LC_ALL", locale = "English_US")
  Sys.setenv(LANG = "English_US")
}

# warn on partial matches
options(
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE
)

# enable autocompletions for package names in
# `require()`, `library()`
utils::rc.settings(ipck = TRUE)

# warnings are errors
options(warn = 2)

# fancy quotes are annoying and lead to
# 'copy + paste' bugs / frustrations
options(useFancyQuotes = FALSE)

.__Rprofile_env__. <- new.env(parent = emptyenv())

## ... fill .__Rprofile_env__. with stuff ...

attach(.__Rprofile_env__.)
