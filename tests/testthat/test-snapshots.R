testthat::test_that('Check if the snapshots do not change', {
  # https://testthat.r-lib.org/articles/snapshotting.html
  
  # FIRST TIME ---------------------------------------------------------------
  
  # CREATE SNAPSHOTS TEST LINES
    # FILES_TO_SNAPSHOT = list.files(path = "outputs/data", pattern = "df_.*csv$", full.names = TRUE, ignore.case = FALSE)
    # FILES_TO_SNAPSHOT %>% walk(~ cat(paste0('testthat::expect_snapshot_file(here::here("', .x, '"))\n')))
  
  # IF Test was skiped because: "On CRAN".
    Sys.setenv(NOT_CRAN = "true")
    
  
  # IF THERE IS AN ERROR (eg. in df_AIM): -------------------------------------
  
    # TARGET = readr::read_csv("outputs/data/df_AIM.csv")
    # SNAPSHOT = readr::read_csv("tests/testthat/_snaps/snapshots/df_AIM.csv")
    # waldo::compare(TARGET, SNAPSHOT)
    # If NEW snapshot is OK: # testthat::snapshot_accept()
  
  # If multiple snapshots fail
    # options(testthat.progress.max_fails = 100)
    # testthat::test_file("tests/testthat/test-snapshots.R")
    # testthat::snapshot_accept()
  
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "snapshots"
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  
  # Actual expectation -------------------------------------------------------------
  
  # testthat edition
  testthat::local_edition(3)
  
  # COMMENTED: Tests with random components
  
  # Automatically create snapshots and test existent outputs
  TARGETS = list.files(here::here("outputs/data/"), pattern = "^df_(.*)csv$", ignore.case = FALSE, full.names = TRUE)
  1:length(TARGETS) %>% purrr::walk(~ testthat::expect_snapshot_file(here::here(TARGETS[.x])))
  
  # TODO: blacklist tests with random components
  # BART, EmpaTom, HRPVB, INFCONS, PRFBM, Goodbye
  
  # # COMMENTED: Tests with random components
  # testthat::expect_snapshot_file(here::here("outputs/data/df_AIM.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_BART.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_BNT.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_bRCOPE.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_CAS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_Consent.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_Cov19Q.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_COVIDCONTROL.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_CRS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_CRT7.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_CRTMCQ4.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_CRTv.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_DASS21.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_DEBRIEF.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_DEMOGR.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_EAR.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_EmpaTom.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_ERQ.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_ESM.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_FDMQ.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_FONDECYT.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_GHQ12.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_Goodbye.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_HRPVB.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_HRPVBpost.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_IBT.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_IDQ.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_IEC.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_INFCONS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_IRI.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_IRS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_MIS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_OBJNUM.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_OTRASRELIG.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_PBS.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_PRFBM.csv"))
  # # testthat::expect_snapshot_file(here::here("outputs/data/df_PRFBMpost.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_PSETPP.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_PSPPC.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_PSS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_PVC.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_PWb.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_REI40.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_Report.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_RSS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_RTS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_SASS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_SBS.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_SCSORF.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_SDG.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_SRA.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_SRBQP.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_SRSav.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_STAI.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_SWBQ.csv"))
  # testthat::expect_snapshot_file(here::here("outputs/data/df_WEBEXEC.csv"))
  
})
