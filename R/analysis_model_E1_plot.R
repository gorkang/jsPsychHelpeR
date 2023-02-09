#' analysis_model_E1_plot
#'
#' @param model model object
#'
#' @return ggplot
#' @export
analysis_model_E1_plot <- function(model) {

  # We can use the model data directly: model_XXX$model OR SIMILAR
  
  model$model %>% 
    ggplot2::ggplot(ggplot2::aes(CRTMCQ4_Reflectiveness_DIRd, REI40_Experiential_DIRd)) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal()
  
}
