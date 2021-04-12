##' create_DF_analysis
##'
##' .. content for \details{} ..
##'
##' @title
##' @param DF_joined
##' @return
##' @author gorkang
##' @export
create_DF_analysis <- function(DF_joined) {

  # Selects all STDt, STDd, DIRt and DIRd scales
  # all_scales also used in "tests/testthat/test-missing_data.R"
  all_scales = grep(".*_DIRt$|.*_STDt$|.*_DIRd$|.*STDd$", names(DF_joined), value = TRUE, perl = TRUE)
    
  DF_analysis = 
    DF_joined %>% 
    select(id, all_of(all_scales))
  
  # Save files --------------------------------------------------------------
  save_files(DF_analysis, short_name_scale = "analysis", is_scale = FALSE)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_analysis)

}
