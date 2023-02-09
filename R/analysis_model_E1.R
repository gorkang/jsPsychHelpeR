#' analysis_model_E1
#'
#' @param DF_analysis DF_analysis
#'
#' @return lm model
#' @export
analysis_model_E1 <- function(DF_analysis) {

  model_E1 = stats::lm(CRTMCQ4_Reflectiveness_DIRd ~ REI40_Experiential_DIRd, DF_analysis)

  return(model_E1)

}
