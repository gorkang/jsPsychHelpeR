#' create_DF_analysis
#'
#' @param DF_joined DF
#' @param last_task name of last task
#' @param save_output TRUE / FALSE
#' @param DVars vector with variable names
#'
#' @return DF_analysis
#' @export
create_DF_analysis <- function(DF_joined, last_task, save_output = TRUE, DVars = c("")) {
  
  # debug_function("create_DF_analysis")
  # Selects all STDt, STDd, DIRt and DIRd scales
  # all_scales also used in "tests/testthat/test-missing_data.R"
  all_scales = grep(".*_DIRt$|.*_STDt$|.*_DIRd$|.*RELd$|.*STDd$", names(DF_joined), value = TRUE, perl = TRUE)
    
  DF_analysis = 
    DF_joined %>% 
   dplyr::select(id, dplyr::all_of(all_scales)) %>% 
    
    # Remove people that did not finish protocol
   tidyr::drop_na(dplyr::all_of(last_task))
  
  # Randomize order of DVars & Save as DF_analysis_blinded
  # DVars = c("SRSav_ORA_DIRd", "SBS_DIRt")
  
  if (all(DVars %in% names(DF_analysis))) {
    
    DF_analysis_blinded = 
      DF_analysis |> 
      dplyr::mutate(dplyr::across(dplyr::all_of(DVars), sort))
    
    if (save_output == TRUE) save_files(DF_analysis_blinded, short_name_scale = "analysis_blinded", is_scale = FALSE)  
    cli::cli_alert_info("DF_analysis_blinded created for the DV's: {DVars}")
    
  } else if (DVars == "") {
    
    cli::cli_alert_info("`create_DF_analysys(DVars)` is empty. Will not create DF_analysis_blinded. \nFor more information about blinded analysis: https://gorkang.github.io/jsPsychRmanual/qmd/05-jsPsychHelpeR.html#blinded-analysis")
    DF_analysis_blinded = NULL
    
  } else {
    
    cli::cli_abort("Some of the vars in `DVars` not found in DF_analysis")
    
  }
  
  # Save files --------------------------------------------------------------
  if (save_output == TRUE) save_files(DF_analysis, short_name_scale = "analysis", is_scale = FALSE)
  
  
  
  # Output of function ---------------------------------------------------------
  OUTPUT = list(DF_analysis = DF_analysis,
                DF_analysis_blinded = DF_analysis_blinded)
  return(OUTPUT)

}
