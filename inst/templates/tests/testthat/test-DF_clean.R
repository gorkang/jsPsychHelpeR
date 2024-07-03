testthat::test_that('Check if any of the items appear more or less than the other items for each given test', code = {
  
  # DEBUG
  # targets::tar_load(c(DF_clean))
  
  testthat::local_edition(3)

  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "DF_clean"
  
  # In general, these SHOULD be IDQ_09_1, IDQ_11_1, IDQ_13_1 ???
  white_list = c("AIM_04", "AIM_05", "AIM_06", "AIM_07", "AIM_08", "AIM_09", "AIM_10",
                 "DEMOGR24_12", "DEMOGR24_14",
                 "fauxPasEv_003", "fauxPasEv_004", "fauxPasEv_005", "fauxPasEv_006", "fauxPasEv_007", "fauxPasEv_012", "fauxPasEv_013", "fauxPasEv_014", "fauxPasEv_015", "fauxPasEv_016", "fauxPasEv_021", "fauxPasEv_022", "fauxPasEv_023", "fauxPasEv_024", "fauxPasEv_025", "fauxPasEv_030", "fauxPasEv_031", "fauxPasEv_032", "fauxPasEv_033", "fauxPasEv_034", "fauxPasEv_039", "fauxPasEv_040", "fauxPasEv_041", "fauxPasEv_042", "fauxPasEv_043", "fauxPasEv_048", "fauxPasEv_049", "fauxPasEv_050", "fauxPasEv_051", "fauxPasEv_052", "fauxPasEv_057", "fauxPasEv_058", "fauxPasEv_059", "fauxPasEv_060", "fauxPasEv_061", "fauxPasEv_066", "fauxPasEv_067", "fauxPasEv_068", "fauxPasEv_069", "fauxPasEv_070", "fauxPasEv_075", "fauxPasEv_076", "fauxPasEv_077", "fauxPasEv_078", "fauxPasEv_079", "fauxPasEv_084", "fauxPasEv_085", "fauxPasEv_086", "fauxPasEv_087", "fauxPasEv_088", "fauxPasEv_093", "fauxPasEv_094", "fauxPasEv_095", "fauxPasEv_096", "fauxPasEv_097", "fauxPasEv_102", "fauxPasEv_103", "fauxPasEv_104", "fauxPasEv_105", "fauxPasEv_106", "fauxPasEv_111", "fauxPasEv_112", "fauxPasEv_113", "fauxPasEv_114", "fauxPasEv_115", "fauxPasEv_120", "fauxPasEv_121", "fauxPasEv_122", "fauxPasEv_123", "fauxPasEv_124", "fauxPasEv_129", "fauxPasEv_130", "fauxPasEv_131", "fauxPasEv_132", "fauxPasEv_133", "fauxPasEv_138", "fauxPasEv_139", "fauxPasEv_140", "fauxPasEv_141", "fauxPasEv_142", "fauxPasEv_147", "fauxPasEv_148", "fauxPasEv_149", "fauxPasEv_150", "fauxPasEv_151", "fauxPasEv_156", "fauxPasEv_157", "fauxPasEv_158", "fauxPasEv_159", "fauxPasEv_160", "fauxPasEv_165", "fauxPasEv_166", "fauxPasEv_167", "fauxPasEv_168", "fauxPasEv_169", "fauxPasEv_174", "fauxPasEv_175", "fauxPasEv_176", "fauxPasEv_177", "fauxPasEv_178",
                 "IDQ_10", "IDQ_12", "IDQ_14",
                 "INFCONS_01", "INFCONS_02", "INFCONS_03", "INFCONS_04", "INFCONS_05", "INFCONS_06", "INFCONS_07", "INFCONS_08", "INFCONS_09", "INFCONS_10", "INFCONS_11", "INFCONS_12", "INFCONS_13", "INFCONS_14", "INFCONS_15", "INFCONS_16", "INFCONS_17", "INFCONS_18", "INFCONS_19", "INFCONS_20", "INFCONS_21", "INFCONS_22", 
                 "DMW_01", "DMW_02", "DMW_03", "DMW_04", "DMW_05",
                 "OTRASRELIG_07", "OTRASRELIG_09",
                 "PRFBM_02", "PRFBM_03", "PRFBMpost_02", "PRFBMpost_03",
                 "PVC_001_1",
                 "Report_001_1", "Report_001_2",
                 "SASS_02", "SASS_03", "SDG_05", "SDG_07", "SDG_08"
                 ) # Items where there is a conditional controlling if the question is shown
  # cat(cli::style_underline(cli::col_yellow(paste0("\n\nRunning: ", cli::col_silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  # Test --------------------------------------------------------------------
  
  DF_temp = 
    DF_clean  |>  
    dplyr::group_by(experiment) |> 
    dplyr::count(trialid, name = "times_item_shown") |> 
    dplyr::filter(!trialid %in% white_list) |> 
    dplyr::count(times_item_shown) |> 
    dplyr::count() # How many times an experiment appears?
    
  DF_clean_offender_tests =
    DF_temp |> 
    dplyr::filter(n > 1) |> 
    dplyr::pull(experiment)
  
  checks_DF = 
    DF_clean |> 
    dplyr::group_by(experiment) |> 
    dplyr::count(trialid) |> 
    dplyr::filter(experiment %in% DF_clean_offender_tests)
  
  
  # Warning and log ---------------------------------------------------------
  
  if (length(DF_clean_offender_tests) > 0) {
    
    data.table::fwrite(checks_DF, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(cli::col_red("\nERROR in", paste0("test-", name_of_test), "\n"),
        cli::col_red("  - Some of the items appear more than others in the same tests:"), DF_clean_offender_tests, "\n",
        cli::col_green("  - # of Issues: "), cli::col_red(length(DF_clean_offender_tests)), "\n",
        cli::col_silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_gt(nrow(DF_temp), 1) # Checks that we have some rows in the DF
  testthat::expect_length(DF_clean_offender_tests, 0)
  
})
