
# Prepare files -----------------------------------------------------------


# List of core files for ext/templates/jsPsychHelpeR.zip

root_files = c("jsPsychHelpeR.Rproj", "renv.lock", "run.R", "renv/activate.R", ".vault/README.md", "targets/_targets_TEMPLATE.R", "_targets_options.R", ".Rprofile", "README.md", "NEWS.md", "DESCRIPTION")
R_folder = c("R/list_input_files.R", "R/helper_functions_minimal.R", "R/run_initial_setup.R")
tasks = list.files("R_tasks", full.names = TRUE)
reports = list.files("Rmd", full.names = TRUE)
analysis = list.files("R", pattern = "^analysis", full.names = TRUE)
create = list.files("R", pattern = "^create", full.names = TRUE)
read = list.files("R", pattern = "^read", full.names = TRUE)
tests = c(list.files("R", pattern = "^test", full.names = TRUE), "tests/testthat.R", list.files("tests/testthat/", full.names = TRUE))
renv_cache = list.files("renv/cache", full.names = TRUE, recursive = TRUE)
renv_lib = list.files("renv/lib/", full.names = TRUE, recursive = TRUE)


all_files = c(root_files, R_folder, tasks, reports, analysis, create, read, tests, renv_cache, renv_lib)
# all_files = c(root_files, R_folder, tasks, reports, analysis, create, read, tests) # NO RENV CACHE

# Create new jsPsychHelpeR.zip (~22 sec)
file.remove("inst/templates/jsPsychHelpeR.zip")
utils::zip(zipfile = "inst/templates/jsPsychHelpeR.zip", files = all_files, flags = "-q")




devtools::document()
devtools::load_all()
devtools::check() # Check package (~70s)

# ASCII CHARS
tools::showNonASCIIfile(file = "R/check_missing_prepare_TASK.R")
tools::showNonASCIIfile(file = "R/get_dimensions_googledoc.R")
tools::showNonASCIIfile(file = "R/helper_functions_minimal.R")
# stringi::stri_escape_unicode("·")


# Build package -----------------------------------------------------------

# Build and install
devtools::document()
devtools::load_all()
devtools::build()
devtools::install()

# Install package
renv::install("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR_0.2.0.tar.gz")



  # CHECK Main function 
  jsPsychHelpeR::run_initial_setup(pid = 999, download_files = TRUE, dont_ask = FALSE)
  jsPsychHelpeR::run_initial_setup(pid = 999, data_location = "~/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR/data/999", dont_ask = FALSE)
  
  jsPsychHelpeR::run_initial_setup(pid = 999, download_files = TRUE, dont_ask = TRUE, folder = "~/Downloads/XXX")
  jsPsychHelpeR::run_initial_setup(pid = 999, data_location = "~/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR/data/999", dont_ask = TRUE, folder = "~/Downloads/XXX2")

  # CHECK project works
  targets::tar_make()
  
  # Check docker works
  renv::install("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR_0.2.0.tar.gz")
  jsPsychHelpeR::create_docker_container()
  PID = 999
  file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
  system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID))
  
  



# CHECKS
devtools::check() # Check package (~70s)
devtools::test() # Run all tests (~35s)

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
# e.g. ó -> \u00F3
