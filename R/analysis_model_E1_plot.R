##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param model_XXX
##' @return
##' @author gorkang
##' @export
analysis_model_E1_plot <- function(model) {

  # We can use the model data directly: model_XXX$model OR SIMILAR
  
  model$model %>% 
    ggplot(aes(CRTMCQ4_Reflectiveness_DIRd, REI40_Experiential_DIRd)) +
    geom_point() +
    theme_minimal()
  
  # model_XXX$na.action

}
