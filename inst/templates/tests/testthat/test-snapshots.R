testthat::test_that('Check if the snapshots do not change', {
  # https://testthat.r-lib.org/articles/snapshotting.html
  
  # IF Test was skiped because: "On CRAN".
  Sys.setenv(NOT_CRAN = "true")
  
  name_of_test = "snapshots"
  
  
  testthat::local_edition(3)
  
  expect_snapshot_file_safely = purrr::safely(testthat::expect_snapshot_file)
  
  # Automatically create snapshots and test existent outputs
  TARGETS = list.files(here::here("outputs/data/"), pattern = "^df_(.*)csv$", ignore.case = FALSE, full.names = TRUE)
  SNAPS <- 1:length(TARGETS)  |>  
    purrr::map(~ {
      test_snap = expect_snapshot_file_safely(here::here(TARGETS[.x]))
      if(!is.null(test_snap$error)) cli::cli_alert_danger("SNAP {basename(TARGETS[.x])} CHANGED, use `testthat::snapshot_review()`")
      test_snap
    })
  
  SNAPSHOTS_changed = 1:length(SNAPS) |> 
    purrr::map(~ testthat::expect_no_error(SNAPS[[.x]]$error)) |> 
    purrr::compact()
  
  testthat::expect_length(SNAPSHOTS_changed, 0)
  
  # TODO: blacklist tests with random components
  # BART, EmpaTom, HRPVB, INFCONS, PRFBM, Goodbye
  
})
