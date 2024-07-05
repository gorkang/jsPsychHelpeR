#' analysis_descriptive_plots
#'
#' Generate histograms for main variables
#'
#' @param DF_joined .
#' @param DF_raw .
#' @param DF_clean .
#' @param save_plots TRUE / FALSE
#'
#' @return list with ggplots
#' @export
analysis_descriptive_plots <- function(DF_joined, DF_raw, DF_clean, save_plots = FALSE) {
  
  # DEBUG
  # targets::tar_load_globals()
  # jsPsychHelpeR::debug_function(analysis_descriptive_plots)
  
  # DF_joined
  all_scales = grep(".*_DIRt$|.*_STDt$|.*_RELt$|.*_REdt$|.*_DIRd$|.*STDd$", names(DF_joined), value = TRUE, perl = TRUE)
  
  
  # Scores ----------------------------------------------------------------
  
  DF_plot = DF_joined %>% 
   dplyr::select(id, dplyr::all_of(all_scales))
  
  # NUMERIC values
  d <- 
    DF_plot %>% 
    dplyr::select_if(is.numeric) %>% 
    tidyr::pivot_longer(1:ncol(.)) %>% 
    dplyr::filter(value != 9999) %>% # Si existiera algun codigo para missing values, filtrar
   tidyr::drop_na(value)
  
  # Plot numeric variables
  plot1 = d %>% 
    ggplot2::ggplot(ggplot2::aes(value)) + 
    ggplot2::facet_wrap(~ name, scales = "free") + 
    ggplot2::geom_histogram(bins = 15) +
    ggplot2::theme_minimal()
  
  
  # CHARACTER values
  d2 <- 
    DF_plot %>% 
    dplyr::select_if(is.character) %>% 
    #dplyr::select(-id) %>% 
    tidyr::pivot_longer(1:ncol(.)) %>% 
    dplyr::filter(value != 9999) %>% # Si existiera algun codigo para missing values, filtrar
   tidyr::drop_na(value)
  
  # If we only have id, do not create plot
  if(length(unique(d2$name)) != 1) {
    # Plot character variables
    plot2 = d2 %>% 
      ggplot2::ggplot(ggplot2::aes(value)) + 
      ggplot2::facet_wrap(~ name, scales = "free") + 
      ggplot2::geom_bar() +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal()
    
    
  } else {
    plot2 = NULL
  }
  
  
  
  # Times -----------------------------------------------------------------
  
  options(scipen = 999)
  
  plot_time_participants = DF_raw %>% 
   dplyr::select(id, experiment, rt) %>% 
    dplyr::mutate(rt = as.numeric(rt)/60000) %>% 
    dplyr::group_by(id, experiment) %>% 
    dplyr::summarise(TIME = round(max(rt), 2), 
              N = dplyr::n(), 
              .groups = "keep") %>% 
    ggplot2::ggplot(ggplot2::aes(TIME)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::facet_wrap(~experiment, scales = "free") + 
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "time of participants by task",
         x = "time (in minutes)",
         y = "number of participants") 
    # scale_x_log10(n.breaks = 10)
  
  
  plot_time_responses = DF_clean %>% 
    dplyr::mutate(rt = as.numeric(rt)/1000) %>% 
    ggplot2::ggplot(ggplot2::aes(rt)) +
    ggplot2::geom_histogram() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "rt of all the responses by task",
         x = "rt (in seconds)",
         y = "number of responses") +
    ggplot2::facet_wrap(~ experiment, scales = "free")
  
  

  # SAVE --------------------------------------------------------------------

  if (save_plots == TRUE) {
    
    cli::cli_alert_info("Saving descriptive plots")
    ggplot2::ggsave(paste0("outputs/plots/plot_", pid_target, "_descriptive_numeric.png"), plot1, dpi = 150, height = 12, width = 20, bg = "white")
    ggplot2::ggsave(paste0("outputs/plots/plot_", pid_target, "_descriptive_categorical.png"), plot2, dpi = 150, height = 12, width = 20, bg = "white")
    ggplot2::ggsave(paste0("outputs/plots/plot_", pid_target, "_time_participants.png"), plot_time_participants, dpi = 150, height = 12, width = 20, bg = "white")
    ggplot2::ggsave(paste0("outputs/plots/plot_", pid_target, "_time_responses.png"), plot_time_responses, dpi = 150, height = 12, width = 20, bg = "white")
  
  }
  
  
  

  # Output ------------------------------------------------------------------

  plots_descriptive = list(plot_descriptive_numeric = plot1, 
                           plot_descriptive_categorical = plot2,
                           plot_time_participants = plot_time_participants,
                           plot_time_responses = plot_time_responses)
  
  return(plots_descriptive)
  
}
