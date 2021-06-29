testthat::test_that('Check if the snapshots do not change', {
  # https://testthat.r-lib.org/articles/snapshotting.html
  
  # FIRST TIME ---------------------------------------------------------------
  
  # CREATE SNAPSHOTS TEST LINES
    # FILES_TO_SNAPSHOT = list.files(path = "_targets/objects", pattern = "df_*", full.names = TRUE, ignore.case = FALSE)
    # FILES_TO_SNAPSHOT %>% walk(~ cat(paste0('testthat::expect_snapshot_file(here::here("', .x, '"))\n')))
  
  # IF Test was skiped because: "On CRAN".
    Sys.setenv(NOT_CRAN = "true")
  
  
  # IF THERE IN AN ERROR (eg. in df_AIM): -------------------------------------
  
    # TARGET = read_rds("_targets/objects/df_AIM")
    # SNAPSHOT = read_rds("tests/testthat/_snaps/snapshots/df_AIM")
    # waldo::compare(TARGET, SNAPSHOT)
    # If NEW snapshot is OK: # testthat::snapshot_accept()
  
  
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "snapshots"
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  
  # Actual expectation -------------------------------------------------------------
  
  # testthat edition
  testthat::local_edition(3)
  
  testthat::expect_snapshot_file(here::here("_targets/objects/df_AIM"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_BNT"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_bRCOPE"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_Cov19Q"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_COVIDCONTROL"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_CRS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_CRT7"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_CRTMCQ4"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_CRTv"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_DEBRIEF"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_DEMOGR"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_EAR"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_ERQ"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_FDMQ"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_GHQ12"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_Goodbye"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_HRPVB"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_HRPVBpost"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_IDQ"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_IEC"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_IRI"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_IRS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_MIS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_OBJNUM"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_OTRASRELIG"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_PBS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_PRFBM"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_PRFBMpost"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_PSETPP"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_PSPPC"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_PSS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_PWb"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_REI40"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_RSS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_RTS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SASS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SBS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SCSORF"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SDG"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SRA"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SRSav"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SWBQ"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_WEBEXEC"))
  
})
