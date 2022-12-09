##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param model
##' @return
##' @author gorkang
##' @export
analysis_model_E1_table <- function(model) {

  table1_model_E1 = 
    gtsummary::tbl_regression(model, intercept = TRUE) %>% 
    # add_global_p() %>%
    bold_labels() %>% 
    italicize_levels() %>% 
    add_glance_table(include = c("nobs", "df.residual", "r.squared", "adj.r.squared"))
  
  # Check also sjPlot::tab_model()
  # table1_model_E1 = sjPlot::tab_model(model, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE
  
  
  return(table1_model_E1)
  

}
