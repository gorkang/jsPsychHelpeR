# FONDECYT 2020 Huepe project working with {targets}


# Parameters --------------------------------------------------------------

options(pillar.sigfig = 5)


# Libraries ---------------------------------------------------------------

# remotes::install_github("wlandau/targets")
# remotes::install_github("wlandau/tarchetypes")

library(targets) 
library(tarchetypes) 


# Targets -----------------------------------------------------------------

# Define custom functions and other global objects.
lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)

# Packages
main_packages = c("broom", "broom.mixed", "crayon", "dplyr", "emmeans", "forcats", "ggalluvial", "ggridges", "gmodels", "gt", "gtsummary", "irr", "janitor", "lme4", "parameters", "patchwork", "performance", "psych", "purrr", "readr", "report", "sjPlot", "shrtcts", "stringr", "parameters", "performance", "renv", "tarchetypes", "targets", "testthat", "tidyr")
non_declared_dependencies = c("qs", "visNetwork", "webshot")
packages_to_load = c(main_packages, non_declared_dependencies)

# target options (packages, errors...)
tar_option_set(packages = packages_to_load, # Load packages for all targets
               error = "save") # Needed to load workspace on error to debug

# Recreates _packages.R with the above packages (so renv founds them). Should be launched after tar_option_set()
targets::tar_renv(ask = FALSE) # Need to run renv::init() if anything changes



# Define targets
targets <- list(
  
  # Read file --------------------------------------------------------------
  tar_target(input_files, list.files(path = "data", pattern="*.csv", full.names = TRUE), format = "file"),
  tar_target(DF, read_data(input_files)),


  # Process tasks -----------------------------------------------------------
  tar_target(df_SBS, prepare_SBS(DF)),

   
  # Tests -------------------------------------------------------------------

  # NOT sure which of the alternatives is better. The one without dependencies is simpler, but makes tests non-dependent on the other targets. 
  # Right now the function does not use any argument... :(
  tar_target(TESTS, test_testhat(DF, df_SBS))
  # tar_target(TESTS, test_testhat())
  
  
)

tar_pipeline(targets)
