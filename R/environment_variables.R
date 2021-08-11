

env_var <- new.env(parent = baseenv())


withr::with_environment(
  env = env_var,
  code = {
    project_root <- rprojroot::find_root(rprojroot::as.root_criterion(".Rprofile"))
    temp_dir <- normalizePath(tempdir())
    truhart_dir <- file.path(project_root, "Truhart")
  }
)

env_var$project_root <- rprojroot::find_root(rprojroot::as.root_criterion(".Rprofile"))
env_var$temp_dir <- normalizePath(tempdir())
env_var$truhart_dir <- file.path(env_var$project_root)

eval(env_var$temp_dir)
