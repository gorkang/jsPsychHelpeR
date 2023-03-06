testthat::test_that("Check that the project with 999.zip runs OK with run_initial_setup()", {
  
  # REVIEW: 
    # - 3 of these tests fail when doing devtools::check()
    # - They work fine with devtools::test(), including the Snapshots!
  
  
  testthat::local_edition(3)

  # Create temp dir to run everything
  TEMP_dir = tempdir()
  
  suppressMessages(
    run_initial_setup(
      pid = 999,
      data_location = system.file("extdata", package = "jsPsychHelpeR"),
      folder = TEMP_dir,
      dont_ask = TRUE,
      open_rstudio = FALSE
    )
  )
  
  
  # Gets rid of Tests in _targets.R to avoid circularity
  # ONLY NEEDED IF THE TEST WOULD RUN in the Package project
    # targets_file = paste0(TEMP_dir, "/_targets.R")
    # TARGETS = readLines(targets_file)
    # 
    # begin_tests = grep("# Tests ---", TARGETS)
    # end_tests = grep("# Reports ---", TARGETS)
    # 
    # TARGETS = TARGETS[-(begin_tests:end_tests)]
    # cat(TARGETS, file = targets_file, sep = "\n")
    # 
  
  # Run tar_make() in the TEMP_dir
  system(paste0("cd ", TEMP_dir, "; R ", TEMP_dir, " -e targets::tar_make\\(\\)"))
  
  FILES_root = list.files(TEMP_dir, recursive = FALSE)
  FILES_outputs = list.files(paste0(TEMP_dir, "/outputs/data/"), recursive = FALSE)
  FILES_tasks = list.files(paste0(TEMP_dir, "/outputs/data/"), pattern = "df.*_sp", recursive = FALSE)
  

  # Check that both _targets.R and output/ are created  
  testthat::expect_equal(sum(grepl("_targets.R|outputs", FILES_root)), 2)
  
  # Check that tar_make() created some of the expected files
  testthat::expect_equal(sum(grepl("df_AIM.csv", FILES_outputs)), 1)
  testthat::expect_equal(sum(grepl("DF_analysis.csv", FILES_outputs)), 1)
  testthat::expect_equal(sum(grepl("DF_joined.csv", FILES_outputs)), 1)
  
  
  # prepare_TASKS
  # IRI
  # IRI_snap = readr::read_csv("inst/templates/tests/testthat/_snaps/snapshots/df_IRI.csv") #, col_types = readr::cols(id = readr::col_character())
  # IRI = readr::read_csv(paste0(TEMP_dir, "/outputs/data/df_IRI.csv"))
  # expect_equal(IRI_snap, IRI)
  
  seq_along(FILES_tasks) |> 
    purrr::walk(~{
      TASK = gsub("df_(.*)_sp\\.csv", "\\1", FILES_tasks[.x])
      cli::cli_h1("{.x} - {TASK}")
      SNAP = readr::read_csv(paste0(TEMP_dir, "/inst/templates/tests/testthat/_snaps/snapshots/df_", TASK,".csv")) #, col_types = readr::cols(id = readr::col_character())
      OUT = readr::read_csv2(paste0(TEMP_dir, "/outputs/data/", FILES_tasks[.x]))
      expect_equal(SNAP, OUT)
    })
  
  
  # Check that the snapshots we have in /inst/templates/tests/_snaps/snapshots/ EQUAL to the ones created
  # SNAPS_new = list.files(paste0(TEMP_dir, "/tests/testthat/_snaps/snapshots/"), recursive = TRUE, full.names = TRUE)
  # SNAPS_stored = list.files(paste0(TEMP_dir, "/inst/templates/tests/testthat/_snaps/snapshots/"), recursive = TRUE, full.names = TRUE)
  # SNAPS_new =  SNAPS_stored
  
  # testthat::expect_equal(length(SNAPS_stored), length(SNAPS_new))
  
  # seq_len(length(SNAPS_stored)) |> 
  #   purrr::walk(~ testthat::expect_equal(readLines(SNAPS_stored[.x]), readLines(SNAPS_new[.x])))
  
})
