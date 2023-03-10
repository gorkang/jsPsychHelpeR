options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/jammy/latest"))

Sys.setenv(
  RENV_PATHS_LIBRARY = file.path("renv/lib"),
  RENV_PATHS_CACHE = file.path("renv/cache"),
  RENV_PATHS_LIBRARY_ROOT = file.path("renv/cache")
)

options(encoding = "UTF-8")

get_branch = purrr::safely(gert::git_branch)
branch = get_branch()
if (!is.null(branch$result)) cli::cli_alert_info("Git active, branch: [{branch$result}]")
rm("get_branch", "branch")


# source("renv/activate.R")
# renv::restore(prompt = FALSE, clean = TRUE)
