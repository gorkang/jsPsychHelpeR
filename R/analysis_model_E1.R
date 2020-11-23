##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param df_analysis
##' @return
##' @author gorkang
##' @export
analysis_model_E1 <- function(DF_analysis) {

  model_E1 = lm(CRTMCQ4_Reflectiveness_DIRd ~ REI40_Experiential_DIRd, DF_analysis)

  return(model_E1)

}
