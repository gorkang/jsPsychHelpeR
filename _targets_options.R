
# Libraries ---------------------------------------------------------------

if(!require(targets)) install.packages("targets"); library(targets)
if(!require(tarchetypes)) install.packages("tarchetypes"); library(tarchetypes)


# Set options, load packages -----------------------------------------------

# Source all /R files
targets::tar_source("R")
suppressWarnings(targets::tar_source("R_tasks"))
options(pillar.sigfig = 5)

# Packages to load
essential_packages = c(
                  # Main
                  "cli", "renv", "tarchetypes", "targets", "testthat", "visNetwork",
                  
                  # Data preparation
                  "dplyr", "forcats", "here", "janitor", "purrr", "readr", "stringr", "tibble", "tidyr",
                  
                  # Data analysis and visualization
                  "gtsummary", "patchwork", "psych", "DT", "ggplot2"
                  )

extra_packages = c(
                  # Extras
                  "furrr", "writexl", "broom", "broom.mixed", "emmeans", "gt", "irr", "lme4", "parameters", "performance", "ggalluvial", "ggridges",
                  
                  # Non declared dependencies
                  "diffviewer", "qs", "webshot", "xml2", "jquerylib",
                  
                  # Admin
                  "devtools", "gtools", "odbc"
)


packages_to_load = purrr::discard(c(essential_packages), is.na)

# Uncomment this for more functionality, and add your needed packages to extra_packages
# packages_to_load = purrr::discard(c(essential_packages, extra_packages), is.na)


# target options (packages, errors...)
tar_option_set(
  
  # Load packages for all targets
  packages = packages_to_load,
  
  # If there is an error:
  # tar_workspaces() # Lists the available workspaces (e.g. DF_clean)
  # tar_workspace(DF_clean) # Loads the errored workspace
  workspace_on_error = TRUE
  )


# Make sure tests run always
if (file.exists("_targets/objects/TESTS") == TRUE ) targets::tar_invalidate(matches("TESTS"))
