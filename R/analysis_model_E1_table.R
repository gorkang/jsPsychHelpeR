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

  table1_model_E1 = sjPlot::tab_model(model, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE
  
  return(table1_model_E1)
  

}
