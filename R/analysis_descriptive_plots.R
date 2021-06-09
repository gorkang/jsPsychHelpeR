##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param df_analysis
##' @return
##' @author gorkang
##' @export
analysis_descriptive_plots <- function(DF_joined, DF_raw) {
  
  # DEBUG
  # debug_function(analysis_descriptive_plots)
  
  # DF_joined
  all_scales = grep(".*_DIRt$|.*_STDt$|.*_RELt$|.*_REdt$|.*_DIRd$|.*STDd$", names(DF_joined), value = TRUE, perl = TRUE)
  
  
  # Puntajes ----------------------------------------------------------------
  
  DF_plot = DF_joined %>% 
    select(id, all_of(all_scales))
  
  d <- 
    DF_plot %>% 
    select_if(is.numeric) %>% 
    pivot_longer(1:ncol(.)) %>% 
    filter(value != 9999) %>% # Si existiera algun codigo para missing values, filtrar
    drop_na(value)
  
  # Plot numeric variables
  plot1 = d %>% 
    ggplot(aes(value)) + 
    facet_wrap(~ name, scales = "free") + 
    geom_histogram(bins = 15) +
    theme_minimal()
  
  ggsave("outputs/plots/plot_descriptive_numeric.png", plot1, dpi = 150, height = 12, width = 20)
  
  
  d2 <- 
    DF_plot %>% 
    select_if(is.character) %>% 
    # select(-id) %>% 
    pivot_longer(1:ncol(.)) %>% 
    filter(value != 9999) %>% # Si existiera algun codigo para missing values, filtrar
    drop_na(value)
  
  # If we only have id, do not create plot
  if(length(unique(d2$name)) != 1) {
    # Plot character variables
    plot2 = d2 %>% 
      ggplot(aes(value)) + 
      facet_wrap(~ name, scales = "free") + 
      geom_bar() +
      coord_flip() +
      theme_minimal()
    
    ggsave("outputs/plots/plot_descriptive_categorical.png", plot2, dpi = 150, height = 12, width = 20)
  } else {
    plot2 = NULL
  }
  
  
  
  # Tiempos -----------------------------------------------------------------
  
  options(scipen = 999)
  
  plot_tiempos = DF_raw %>% 
    select(id, experimento, time_elapsed) %>% 
    group_by(id, experimento) %>% 
    summarise(TIME = round(max(time_elapsed)/60000, 2), 
              N = n(), 
              .groups = "keep") %>% 
    ggplot(aes(TIME)) +
    geom_histogram(bins = 30) +
    facet_wrap(~experimento, scales = "free", ncol = 7) + 
    theme_minimal() +
    scale_x_log10(n.breaks = 10)
  
  ggsave("outputs/plots/plot_tiempos.png", plot_tiempos, dpi = 150, height = 12, width = 20)
  
  
  
  
  
  
  plots_descriptive = list(plot_descriptive_numeric = plot1, 
                           plot_descriptive_categorical = plot2,
                           plot_tiempos = plot_tiempos)
  
  return(plots_descriptive)
  
}
