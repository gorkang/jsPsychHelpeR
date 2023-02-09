options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/jammy/latest"))

Sys.setenv(
  RENV_PATHS_LIBRARY = file.path("renv/lib"),
  RENV_PATHS_CACHE = file.path("renv/cache"),
  RENV_PATHS_LIBRARY_ROOT = file.path("renv/cache")
)

options(encoding = "UTF-8")

source("renv/activate.R")
renv::restore(prompt = FALSE, clean = TRUE)
