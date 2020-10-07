##' Prepare Supernatural Belief Scale
##'
##' .. content for \details{} ..
##'
##' @title prepare_SBS
##' @param DF
##' @return
##' @author gorkang
##' @export
prepare_SBS <- function(DF) {
  
  name_scale = "Supernatural_Belief_Scale"
  short_name_scale = "Supernatural"
  
  df_SBS_RAW =
    DF %>% 
    filter(experimento == name_scale) %>% 
    select(id, experimento, rt, trialid, question_text, response) %>% 
    mutate(response = as.numeric(response)) %>% 
    drop_na(trialid)
  
  
  
  # Wide: direct responses --------------------------------------------------
  
  df_SBS_wide_direct =
    df_SBS_RAW %>% 
    select(id, trialid, response) %>% 
    mutate(trialid = paste0(trialid, "_RAW")) %>% 
    pivot_wider(names_from = trialid, values_from = response) %>% 
    mutate(SBS_RAW_NA = rowSums(is.na(select(., matches(short_name_scale)))))
  
  # df_SBS_wide_direct
  
  
  
  # Wide: processed responses --------------------------------------------------
  
  # [REMEMBER] : THIS IS BULLSHIT NOW!!!!! JUST AN EXAMPLE TO HAVE A REASONABLE TEMPLATE
  
  df_SBS_wide_processed =
    df_SBS_RAW %>% 
    select(id, trialid, response) %>% 
    
    # Process data
    mutate(response = 
             case_when(
               response < 0 ~ response^2,
               response > 0 ~ response^3,
               TRUE ~ response)) %>% 

    mutate(trialid = paste0(trialid, "_PROC")) %>%
    pivot_wider(names_from = trialid, values_from = response) %>% 
    mutate(SBS_PROC = rowSums(select(., matches(short_name_scale)), na.rm = TRUE),
           SBS_PROC_NA = rowSums(is.na(select(., matches(short_name_scale)))))
  
  # df_SBS_wide_processed
  
  
  # JOIN ALL ------------------------------------------------------------------------------------
  
  df_SBS = 
    df_SBS_wide_direct %>% 
    left_join(df_SBS_wide_processed, by = c("id"))
  
  
  
  # CHECK -------------------------------------------------------------------
  
  check_NAs(df_SBS)
  
  
  
  # Save files --------------------------------------------------------------
  
  write_csv(df_SBS, "output/data/df_SBS.csv")
  write_rds(df_SBS, "output/data/df_SBS.rds")
  
  return(df_SBS)
  
}
