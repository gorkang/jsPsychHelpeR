# Can set a dev profile and a production one? Maybe also a docker one?
# https://rstudio.github.io/renv/articles/profiles.html


# renv::install("gorkang/jsPsychAdmin")


# Step by step instructions to build and test package
  # https://r-pkgs.org/structure.html

# Make sure we have the minimum dependencies ------------------------------

  # To have a minimal renv cache (necessary for the docker creation):
  renv::restore(lockfile = "renv.lock", prompt = FALSE)
  # See admin/create_renv_cache.R
  

# Prepare files -----------------------------------------------------------

  # .Rprofile: make sure source("renv/activate.R") is UNCOMMENTED
      # The issue with this being uncommented is that renv will install all the necessary packages
      # in the first RStudio run, but without any warning, so it would seem RStudio is stuck
  source("R/helper_functions_extra.R")
  activate_deactivate_renv(activate_deactivate = "activate")

  # DO THIS ALWAYS so jsPsychHelpeR.zip is updated!
  # Create jsPsychHelpeR.zip
  jsPsychAdmin::create_jsPsychHelpeR_zip(add_renv_cache = FALSE)
  # NO CACHE: 355KB

  # add_renv_cache = TRUE creates a zip file with the renv cache 
  # (initially jsPSychHelper package ~230MB, after cleaning up a lot, 75.6MB)
  # Very useful to:
      # - avoid downloading all renv cache again on Ubuntu
      # - faster docker container creation
  # MAX Github uploads 100MB
  # jsPsychAdmin::create_jsPsychHelpeR_zip(add_renv_cache = TRUE)



# Build package -----------------------------------------------------------

  # .Rprofile: make sure source("renv/activate.R") is COMMENTED
  source("R/helper_functions_extra.R")
  activate_deactivate_renv(activate_deactivate = "deactivate")
  
  # Build and install
  devtools::document()
  devtools::load_all()
  
  
  # Documentation
  devtools::spell_check() # Spell check    
  pkgdown::build_site() # Create documentation! # pkgdown::build_news()
  
  # Only one time?
  # usethis::use_pkgdown() # build-ignore the docs directory and _pkgdown.yml file to avoid NOTE in CRAN checks
  # pkgdown::deploy_site_github() # CHECK HOWTO (only first time?)
  
  
# Install package ---------------------------------------------------------

  # Build
  # devtools::build()
  # devtools::install()
  pak::local_install()
  # pak::pak_cleanup()


# CHECK functions ----------------------------------------------------------

  # CHECK Main function 
  rstudioapi::restartSession()
  jsPsychHelpeR::run_initial_setup(pid = 999, data_location = "~/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR/data/999", dont_ask = TRUE)
  
  # This will create and open a NEW RStudio project. 
  # It will take a long time to open, as it is downloading and installing all the necessary packages
  
  ## IN THE NEW PROJECT##
  # CHECK project works
  targets::tar_make()
  
  # Check docker works
  # rstudioapi::navigateToFile("admin/create_docker.R")
  jsPsychHelpeR::create_docker_container()
  PID = 999
  file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
  system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID))

  # ***DEBUG*** 
  # docker run --rm -ti -v ~/Downloads/jsPsychHelpeR999/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid999 /bin/bash
  
  
# CHECK package -----------------------------------------------------------
  
  devtools::test() # Run all tests for the package (This includes test-0run_initial_setup.R, which will create a full project and run the protocol tests in the tmp/project) (~47s): [ FAIL 0 | WARN 0 | SKIP 0 | PASS 115 ]
  devtools::check() # Check package # tests will fail. See Monkeys for solutions 
  # Package Tests are in tests/testthat/
  # Protocol tests are in inst/templates/tests/testthat/
  # Snapshots in inst/templates/tests/testthat/_snaps/snapshots/


devtools::test_coverage()
# Not necesary because we have a use_github_action???
# covr::codecov(token = "6c8a8848-9175-446c-9cb8-131378f96356") # UPLOAD coverage reports to https://codecov.io/gh/gorkang/jsPsychMaker/



# Code coverage -----------------------------------------------------------

# FIRST TIME
# 1) usethis::use_coverage(type = c("codecov"))
# 2) usethis::use_github_action("test-coverage") # To continuosly monitor code coverage
# 3) Go to website: codecov.io using the GitHub account and setup the repo

COV_REPORT = covr::package_coverage(); COV_REPORT # Test coverage report. If a testthat tests fails, this FAILS!
covr::report(COV_REPORT) # Check local shiny app with Coverage report    
# TOKEN FROM step 3)
covr::codecov(token = "6c8a8848-9175-446c-9cb8-131378f96356") # UPLOAD coverage reports to https://codecov.io/gh/gorkang/jsPsychMaker/


# REMEMBER ----------------------------------------------------------------

# If warning about non-ASCII characters. Find character and replace 
# https://altcodeunicode.com/alt-codes-letter-o-with-accents/
# tools::showNonASCIIfile(file = "R/create_task.R")
# stringi::stri_escape_unicode("ó")
# e.g. ó -> \u00F3




# READ files in package
# system.file("extdata", "999.zip", package = "jsPsychHelpeR")
# system.file("templates", "jsPsychHelpeR.zip", package = "jsPsychHelpeR")
