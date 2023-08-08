# THIS IS A WIP. The idea us to have a small cache folder so:
# 1) jsPsychAdmin::create_jsPsychHelpeR_zip(add_renv_cache = TRUE) will create smaller files
# 2) when creating a new project, on Ubuntu, it will be much faster
# 3) when we create a  docker project, the docker file is as small as possible

# jsPsychAdmin::create_jsPsychHelpeR_zip() cleans unnecessary files from renv/cache


# REVIEW clean_renv_cache() as the R version is included. It just gets rid of cache packages not in lib


# To have a minimal renv cache (necessary to create small docker projects):
create_renv_cache <- function() {
  
  
  # Restore library to the lock file
  # renv::restore(lockfile = "renv.lock", prompt = FALSE)
  
  
  # TO have a minimal renv cache:
  # 0) DELETE renv cache and lib
  unlink(x = c("renv/cache/", "renv/lib/") , recursive = TRUE) # REVIEW: we need to delete lib? why?
  # 1) install only the packages explicitly mentioned _targets_options.R 
  # essential_packages after deleting renv/ cache and lib 
  source("_targets_options.R")
  renv::install(packages = essential_packages, prompt = FALSE)
  # 2) Recreate _targets_packages.R: 
  targets::tar_renv(extras = c("rmarkdown", "rstudioapi","visNetwork"))
  # 3) Create lockfile with the installed subset
  renv::snapshot()
  
  # Make sure project 999 runs: targets::tar_make()
  

}