
# Libraries ---------------------------------------------------------------

library(targets) 
library(tarchetypes) 


# Set options, load packages -----------------------------------------------

# Source all /R files
lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source)
lapply(list.files("./R_tasks/", full.names = TRUE, pattern = ".R$"), source)
options(pillar.sigfig = 5)

# Packages to load
main_packages = c("cli", "furrr", "patchwork", "renv", "tarchetypes", "targets", "testthat")
data_preparation_packages = c("dplyr", "forcats", "here", "janitor", "purrr", "readr", "stringr", "tibble", "tidyr", "writexl") #"safer", 
data_analysis_packages = c("broom", "broom.mixed", "emmeans", "gt", "gtsummary", "irr", "lme4", "parameters", "performance", "psych", "sjPlot") #"report" "gmodels"
data_visualization_packages = c("DT", "ggalluvial", "ggridges")
non_declared_dependencies = c("qs", "visNetwork", "webshot", "performance", "shinyWidgets", "diffviewer")
extra_packages = ifelse (Sys.info()["sysname"] %in% c("Linux"), c("shrtcts"), NA)
packages_to_load = purrr::discard(c(main_packages, data_preparation_packages, data_analysis_packages, data_visualization_packages, non_declared_dependencies, extra_packages), is.na)


# target options (packages, errors...)
tar_option_set(packages = packages_to_load, # Load packages for all targets
               workspace_on_error = TRUE) # Needed to load workspace on error to debug


# Make sure tests run always
if (file.exists("_targets/objects/TESTS") == TRUE ) targets::tar_invalidate(matches("TESTS"))
