##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param df_analysis
##' @return
##' @author gorkang
##' @export
analysis_descriptive_plots <- function(DF_joined, DF_raw, DF_clean, save_plots = FALSE) {
  
  # DEBUG
  # debug_function(analysis_descriptive_plots)
  
  # DF_joined
  all_scales = grep(".*_DIRt$|.*_STDt$|.*_RELt$|.*_REdt$|.*_DIRd$|.*STDd$", names(DF_joined), value = TRUE, perl = TRUE)
  
  
  # Scores ----------------------------------------------------------------
  
  DF_plot = DF_joined %>% 
    select(id, all_of(all_scales))
  
  # NUMERIC values
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
  
  
  # CHARACTER values
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
    
    
  } else {
    plot2 = NULL
  }
  
  
  
  # Times -----------------------------------------------------------------
  
  options(scipen = 999)
  
  plot_time_participants = DF_raw %>% 
    select(id, experimento, rt) %>% 
    mutate(rt = as.numeric(rt)/60000) %>% 
    group_by(id, experimento) %>% 
    summarise(TIME = round(max(rt), 2), 
              N = n(), 
              .groups = "keep") %>% 
    ggplot(aes(TIME)) +
    geom_histogram(bins = 30) +
    facet_wrap(~experimento, scales = "free") + 
    theme_minimal() +
    labs(title = "time of participants by task",
         x = "time (in minutes)",
         y = "number of participants") 
    # scale_x_log10(n.breaks = 10)
  
  
  plot_time_responses = DF_clean %>% 
    mutate(rt = as.numeric(rt)/1000) %>% 
    ggplot(aes(rt)) +
    geom_histogram() +
    theme_minimal() +
    labs(title = "rt of all the responses by task",
         x = "rt (in seconds)",
         y = "number of responses") +
    facet_wrap(~ experimento, scales = "free")
  
  

  # SAVE --------------------------------------------------------------------

  if (save_plots == TRUE) {
    
    cli::cli_alert_info("Saving descriptive plots")
    ggsave(paste0("outputs/plots/plot_", pid_target, "_descriptive_numeric.png"), plot1, dpi = 150, height = 12, width = 20, bg = "white")
    ggsave(paste0("outputs/plots/plot_", pid_target, "_descriptive_categorical.png"), plot2, dpi = 150, height = 12, width = 20, bg = "white")
    ggsave(paste0("outputs/plots/plot_", pid_target, "_time_participants.png"), plot_time_participants, dpi = 150, height = 12, width = 20, bg = "white")
    ggsave(paste0("outputs/plots/plot_", pid_target, "_time_responses.png"), plot_time_responses, dpi = 150, height = 12, width = 20, bg = "white")
  
  }
  
  
  

  # Output ------------------------------------------------------------------

  plots_descriptive = list(plot_descriptive_numeric = plot1, 
                           plot_descriptive_categorical = plot2,
                           plot_time_participants = plot_time_participants,
                           plot_time_responses = plot_time_responses)
  
  return(plots_descriptive)
  
}
