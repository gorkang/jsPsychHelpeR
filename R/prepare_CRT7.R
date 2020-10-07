##' Prepare CRT7
##'
##' .. content for \details{} ..
##'
##' @title prepare_CRT7
##' @param DF
##' @return
##' @author gorkang
##' @export
##' 
prepare_CRT7 <- function(DF) {

  # REMEMBER: find and replace CRT7 by the shortname of the new test.
  
  name_scale = "CRT_7"
  short_name_scale = "CRT_7"
  
  
  df_CRT7_RAW =
    DF %>% 
    filter(experimento == name_scale) %>% 
    
    # [TODO]: DELETE THIS ONCE input files are OK
    mutate(response = response_x) %>% 
    
    select(id, experimento, rt, trialid, question_text, response) %>% 
    # mutate(response = as.character(response)) %>% 
    drop_na(trialid)
  
  
  
  # Wide: direct responses --------------------------------------------------
  
  df_CRT7_wide_direct =
    df_CRT7_RAW %>% 
    select(id, trialid, response) %>% 
    mutate(trialid = paste0(trialid, "_RAW")) %>% 
    pivot_wider(names_from = trialid, values_from = response) %>% 
    mutate(CRT7_RAW_NA = rowSums(is.na(select(., matches(short_name_scale)))))
  
  # df_CRT7_wide_direct
  
  
  
  # Wide: processed responses --------------------------------------------------
  
  # [REMEMBER] : THIS IS BULLSHIT NOW!!!!! JUST AN EXAMPLE TO HAVE A REASONABLE TEMPLATE
  
  df_CRT7_wide_processed =
    df_CRT7_RAW %>% 
    select(id, trialid, response) %>% 

    # Process data
    mutate(response =
             case_when(
               trialid == "CRT_7_1" & response == "50" ~ 1,
               trialid == "CRT_7_2" & response == "5" ~ 1,
               trialid == "CRT_7_3" & response == "47" ~ 1,
               trialid == "CRT_7_4" & response == "4" ~ 1,
               trialid == "CRT_7_5" & response == "29" ~ 1,
               trialid == "CRT_7_6" & response == "20" ~ 1,
               trialid == "CRT_7_7" & response == "Ha perdido dinero" ~ 1,
               TRUE ~ 0)) %>% 
    
    mutate(trialid = paste0(trialid, "_PROC")) %>%    
    pivot_wider(names_from = trialid, values_from = response) %>% 
    mutate(CRT7_PROC = rowSums(select(., matches(short_name_scale)), na.rm = TRUE),
           CRT7_PROC_NA = rowSums(is.na(select(., matches(short_name_scale)))))
  
  # df_CRT7_wide_processed
  
  
  # JOIN ALL ------------------------------------------------------------------------------------
  
  df_CRT7 = 
    df_CRT7_wide_direct %>% 
    left_join(df_CRT7_wide_processed, by = c("id"))
  
  
  
  # CHECK -------------------------------------------------------------------
  
  check_NAs(df_CRT7)
  
  
  
  # Save files --------------------------------------------------------------
  
  write_csv(df_CRT7, "output/data/df_CRT7.csv")
  write_rds(df_CRT7, "output/data/df_CRT7.rds")
  
  return(df_CRT7)  
 
}