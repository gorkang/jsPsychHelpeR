# jsPsychHelpeR with {targets}

# Parameters --------------------------------------------------------------

options(pillar.sigfig = 5)


# Libraries ---------------------------------------------------------------

library(targets) 
library(tarchetypes) 


# Set options, load packages -----------------------------------------------

# Source all /R files
lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
lapply(list.files("./R_tasks/", full.names = TRUE, pattern = ".R"), source)


# Packages to load
main_packages = c("cli", "crayon", "patchwork", "renv", "tarchetypes", "targets", "testthat")
data_preparation_packages = c("data.table", "dplyr", "forcats", "here", "janitor", "purrr", "readr", "stringr", "tibble", "tidyr")
data_analysis_packages = c("broom", "broom.mixed", "DT", "emmeans", "gmodels", "gt", "gtsummary", "irr", "kableExtra", "lme4", "parameters", "performance", "psych", "sjPlot", "skimr")
data_visualization_packages = c("ggalluvial", "ggridges")
non_declared_dependencies = c("qs", "visNetwork", "webshot", "performance", "bs4Dash", "pingr", "shiny", "shinycssloaders")
extra_packages = c("shrtcts", "httr")
packages_to_load = c(main_packages, data_preparation_packages, data_analysis_packages, data_visualization_packages, non_declared_dependencies, extra_packages)

# target options (packages, errors...)
tar_option_set(packages = packages_to_load, # Load packages for all targets
               error = "workspace") # Needed to load workspace on error to debug

# Make sure tests run always
# if (file.exists("_targets/objects/TESTS") == TRUE ) targets::tar_invalidate(matches("TESTS"))


# Declare pipeline --------------------------------------------------------

source("targets/targets_main.R")
# source("targets/targets_report.R")

targets
