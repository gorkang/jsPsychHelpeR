##' Prepare BART
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_PRFBM -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_PRFBM
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_BART <- function(DF_clean, short_name_scale_str) {
  
  # DEBUG
  # debug_function(prepare_BART)
  
  # Numero de infladas de cada globo que no revento
  # en que inflada reventaron 
  # recaudacion total
  # cantidad de globos que revientan
  # 
   
  # dimensions = c("InflatesSafe", "InflatesExplode", "TotalIncome", "NumberExplode")
  # BART_01_round_money
  # BART_01_rounds
  # BART_01_status
  # BART_01_total_money

  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                     # dimensions = c("InflatesSafe", "InflatesExplode", "TotalIncome", "NumberExplode"), # Use names of dimensions, "" or comment out line
                     dimensions = c("meanRoundsSafe", "meanRoundsExplode", "numberSafe", "numberExplode", "totalMoney"),
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, help_prepare = FALSE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("00|00") # Ignore the following items: If nothing to ignore, keep "00|00"
  items_to_reverse = c("00|00") # Reverse the following items: If nothing to ignore, keep "00|00"
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  
  
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
    # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
  # Transformations
  mutate(
    DIR = RAW
  ) 
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Create DF_wide_RAW_DIR -----------------------------------------------------
  
  
  # dimensions = c("MeanRounds_safe", "MeanRounds_explode", "N_safe", "N_explode", "TotalMoney")
  # BART_01_round_money
  # BART_01_rounds
  # BART_01_status
  # BART_01_total_money
  
  
  
  
  # METHOD 1 -----------------------
  
  DF_temp =
    DF_long_RAW %>%
    select(id, trialid, RAW) %>%

    # Should be fixed
    # mutate(trialid =
    #          case_when(
    #            grepl("round_money", trialid) ~ gsub("(.*)round_money", "\\1RoundMoney", trialid),
    #            grepl("total_money", trialid) ~ gsub("(.*)total_money", "\\1TotalMoney", trialid),
    #            grepl("explode_rounds", trialid) ~ gsub("(.*)explode_rounds", "\\1ExplodeRounds", trialid),
    #            TRUE ~ trialid)) %>%
    separate(trialid, into = c("BART", "trialnum", "variable"), sep = "_") %>%
    pivot_wider(names_from = variable, values_from = RAW) %>% 
    mutate(status = stringr::str_to_sentence(status))


  DF_wide_Dimensions =

    DF_temp %>%
      group_by(id, status) %>%
      summarise(BART_meanRounds = mean(as.numeric(rounds), na.rm = TRUE),
                BART_number = n(), 
                .groups = "drop") %>%
      pivot_wider(names_from = status, values_from = c(BART_meanRounds, BART_number), names_sep = "") %>%
      left_join(DF_temp %>%
                  group_by(id) %>%
                  summarise(BART_totalMoney = max(as.numeric(totalMoney)), .groups = "drop"), 
                by = "id"
      ) %>% rename_with(~paste0(., "_DIRd"), BART_meanRoundsExplode:BART_totalMoney)

  
  
  # DF_wide_Dimensions
    
  
  # USE name_DIRd1, name_DIRd2, name_DIRd3, name_DIRd4, name_DIRd5

  
  
  DF_wide_RAW_DIR =
    DF_long_DIR %>% 
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_RAW")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(items_to_ignore) & matches("_DIR"))))) %>% 
    
    
    # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
  # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

    
    left_join(DF_wide_Dimensions, by = "id")  
  # mutate(
  #   
  #   # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
  #   !!names_list$name_DIRd[1] := rowMeans(select(., matches("01") & matches("_DIR$")), na.rm = TRUE), 
  #   !!names_list$name_DIRd[2] := rowMeans(select(., matches("02|03") & matches("_DIR$")), na.rm = TRUE), 
  #   !!names_list$name_DIRd[3] := rowMeans(select(., matches("04_beneficio|06_beneficio") & matches("_DIR$")), na.rm = TRUE), 
  #   !!names_list$name_DIRd[4] := rowMeans(select(., matches("05_beneficio|07_beneficio") & matches("_DIR$")), na.rm = TRUE)
  #   
  #   
  #   # Score Scale
  #   # !!names_list$name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
  #   
  # )  
      
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
  
}
