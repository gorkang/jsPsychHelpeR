##' Prepare fauxPasEv
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_FauxPasEv -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_FauxPasEv
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_fauxPasEv <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_fauxPasEv)

  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("")
  items_DIRd2 = c("")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions, # Use names of dimensions, "" or comment out line
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, is_experiment = FALSE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
    # Transformations
    mutate(
      DIR =
        case_when(
          
        # Pregunta 1 / Item 2/9
          # Historias con Faux Pas
          # Si!
          trialid %in% c('faux_pas_ev_011', 'faux_pas_ev_029', 'faux_pas_ev_056',
                         'faux_pas_ev_092', 'faux_pas_ev_101', 'faux_pas_ev_110', 
                         'faux_pas_ev_119', 'faux_pas_ev_128', 'faux_pas_ev_137', 
                         'faux_pas_ev_155') & RAW == 'Si' ~ 1,
          
          # No!
          trialid %in% c('faux_pas_ev_011', 'faux_pas_ev_029', 'faux_pas_ev_056', 
                         'faux_pas_ev_092', 'faux_pas_ev_101', 'faux_pas_ev_110', 
                         'faux_pas_ev_119', 'faux_pas_ev_128', 'faux_pas_ev_137',
                         'faux_pas_ev_155') & RAW == 'No' ~ 0,
          
          # Resto de preguntas con respuestas especificas
          trialid == 'faux_pas_ev_011' & RAW == 'No' ~ 2, # historia 1
          trialid == 'faux_pas_ev_021' & RAW == 'Si' ~ 1, # historia 2
          trialid == 'faux_pas_ev_022' & RAW == 'Sara' ~ 1, # historia 2
          trialid == 'faux_pas_ev_025' & RAW == 'No' ~ 1, # historia 2
          trialid == 'faux_pas_ev_031' & RAW == 'No' ~ 2, # historia 3
          trialid == 'faux_pas_ev_041' & RAW == 'Si' ~ 1, # historia 4
          trialid == 'faux_pas_ev_042' & RAW == 'Alicia' ~ 1, # historia 4
          trialid == 'faux_pas_ev_045' & RAW == 'No' ~ 1, # historia 4
          trialid == 'faux_pas_ev_051' & RAW == 'No' ~ 2, # historia 5
          trialid == 'faux_pas_ev_061' & RAW == 'No' ~ 2, # historia 6
          trialid == 'faux_pas_ev_071' & RAW == 'Si' ~ 1, # historia 7
          trialid == 'faux_pas_ev_072' & RAW == 'La vecina María' ~ 1, # historia 7
          trialid == 'faux_pas_ev_075' & RAW == 'No' ~ 1, # historia 7
          trialid == 'faux_pas_ev_081' & RAW == 'No' ~ 2, # historia 8
          trialid == 'faux_pas_ev_091' & RAW == 'No' ~ 2, # historia 9
          trialid == 'faux_pas_ev_101' & RAW == 'No' ~ 2, # historia 10
          trialid == 'faux_pas_ev_111' & RAW == 'Si' ~ 1, # historia 11
          trialid == 'faux_pas_ev_112' & RAW == 'Roberto, el ingeniero' ~ 1, # historia 11
          trialid == 'faux_pas_ev_115' & RAW == 'No' ~ 1, # historia 11
          trialid == 'faux_pas_ev_121' & RAW == 'Si' ~ 1, # historia 12
          trialid == 'faux_pas_ev_122' & RAW == 'Jose' ~ 1, # historia 12
          trialid == 'faux_pas_ev_122' & RAW == 'Pedro' ~ 1, # historia 12
          trialid == 'faux_pas_ev_125' & RAW == 'No' ~ 1, # historia 12
          trialid == 'faux_pas_ev_131' & RAW == 'Si' ~ 1, # historia 13
          trialid == 'faux_pas_ev_132' & RAW == 'Sergio, el primo de karina' ~ 1, # historia 13
          trialid == 'faux_pas_ev_135' & RAW == 'No' ~ 1, # historia 13
          trialid == 'faux_pas_ev_141' & RAW == 'Si' ~ 1, # historia 14
          trialid == 'faux_pas_ev_142' & RAW == 'Ana ' ~ 1, # historia 14
          trialid == 'faux_pas_ev_145' & RAW == 'No' ~ 1, # historia 14
          trialid == 'faux_pas_ev_151' & RAW == 'Si' ~ 1, # historia 15
          trialid == 'faux_pas_ev_152' & RAW == 'Ana' ~ 1, # historia 15
          trialid == 'faux_pas_ev_155' & RAW == 'No' ~ 1, # historia 15
          trialid == 'faux_pas_ev_161' & RAW == 'Si' ~ 1, # historia 16
          trialid == 'faux_pas_ev_162' & RAW == 'Tito' ~ 1, # historia 16
          trialid == 'faux_pas_ev_165' & RAW == 'No' ~ 1, # historia 16
          trialid == 'faux_pas_ev_171' & RAW == 'No' ~ 2, # historia 17
          trialid == 'faux_pas_ev_181' & RAW == 'Si' ~ 1, # historia 18
          trialid == 'faux_pas_ev_182' & RAW == 'Clara' ~ 1, # historia 18
          trialid == 'faux_pas_ev_185' & RAW == 'No' ~ 1, # historia 18
          trialid == 'faux_pas_ev_191' & RAW == 'No' ~ 2, # historia 19
          trialid == 'faux_pas_ev_201' & RAW == 'No' ~ 2, # historia 20
          
          
          # Historias sin Faux Pas
          trialid == 'faux_pas_ev_002' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_020' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_038' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_047' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_065' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_074' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_083' & RAW == 'No' ~ 2,
          trialid == 'faux_pas_ev_146' & RAW == 'No' ~ 2,

          # Preguntas que NO se puntuan
          trialid %in% c('faux_pas_ev_012', 'faux_pas_ev_015', 'faux_pas_ev_018',
                         'faux_pas_ev_027', 'faux_pas_ev_028', 'faux_pas_ev_032',
                         'faux_pas_ev_035', 'faux_pas_ev_052', 'faux_pas_ev_055',
                         'faux_pas_ev_062', 'faux_pas_ev_065', 'faux_pas_ev_082',
                         'faux_pas_ev_085', 'faux_pas_ev_092', 'faux_pas_ev_095',
                         'faux_pas_ev_102', 'faux_pas_ev_105', 'faux_pas_ev_157',
                         'faux_pas_ev_158', 'faux_pas_ev_172', 'faux_pas_ev_175',
                         'faux_pas_ev_192', 'faux_pas_ev_195', 'faux_pas_ev_202',
                         'faux_pas_ev_205') ~ 0, 
          
          # Para correccion manual
          trialid %in% c('faux_pas_ev_013', 'faux_pas_ev_014', 'faux_pas_ev_016',
                         'faux_pas_ev_017', 'faux_pas_ev_023', 'faux_pas_ev_024', 
                         'faux_pas_ev_026', 'faux_pas_ev_033', 'faux_pas_ev_034', 
                         'faux_pas_ev_036', 'faux_pas_ev_037', 'faux_pas_ev_038',
                         'faux_pas_ev_043', 'faux_pas_ev_044', 'faux_pas_ev_046',
                         'faux_pas_ev_047', 'faux_pas_ev_048', 'faux_pas_ev_053',
                         'faux_pas_ev_054', 'faux_pas_ev_056', 'faux_pas_ev_057',
                         'faux_pas_ev_058', 'faux_pas_ev_063', 'faux_pas_ev_064', 
                         'faux_pas_ev_066', 'faux_pas_ev_067', 'faux_pas_ev_068',
                         'faux_pas_ev_073', 'faux_pas_ev_074', 'faux_pas_ev_076',
                         'faux_pas_ev_077', 'faux_pas_ev_078', 'faux_pas_ev_083', 
                         'faux_pas_ev_084', 'faux_pas_ev_086', 'faux_pas_ev_087', 
                         'faux_pas_ev_088', 'faux_pas_ev_093', 'faux_pas_ev_094',
                         'faux_pas_ev_096', 'faux_pas_ev_097', 'faux_pas_ev_098',
                         'faux_pas_ev_103', 'faux_pas_ev_104', 'faux_pas_ev_106',
                         'faux_pas_ev_107', 'faux_pas_ev_108', 'faux_pas_ev_113',
                         'faux_pas_ev_114', 'faux_pas_ev_116', 'faux_pas_ev_117',
                         'faux_pas_ev_118', 'faux_pas_ev_123', 'faux_pas_ev_124', 
                         'faux_pas_ev_126', 'faux_pas_ev_127', 'faux_pas_ev_128',
                         'faux_pas_ev_133', 'faux_pas_ev_134', 'faux_pas_ev_136', 
                         'faux_pas_ev_137', 'faux_pas_ev_138', 'faux_pas_ev_143', 
                         'faux_pas_ev_144', 'faux_pas_ev_146', 'faux_pas_ev_147', 
                         'faux_pas_ev_148', 'faux_pas_ev_153', 'faux_pas_ev_154', 
                         'faux_pas_ev_156', 'faux_pas_ev_163', 'faux_pas_ev_164', 
                         'faux_pas_ev_166', 'faux_pas_ev_167', 'faux_pas_ev_168',
                         'faux_pas_ev_173', 'faux_pas_ev_174', 'faux_pas_ev_176',
                         'faux_pas_ev_177', 'faux_pas_ev_178', 'faux_pas_ev_183', 
                         'faux_pas_ev_184', 'faux_pas_ev_186', 'faux_pas_ev_187',
                         'faux_pas_ev_188', 'faux_pas_ev_193', 'faux_pas_ev_194', 
                         'faux_pas_ev_196', 'faux_pas_ev_197', 'faux_pas_ev_198', 
                         'faux_pas_ev_203', 'faux_pas_ev_204', 'faux_pas_ev_206', 
                        'faux_pas_ev_207', 'faux_pas_ev_208') ~ 123456789,
          
          is.na(RAW) ~ NA_real_,
          grepl(items_to_ignore, trialid) ~ NA_real_,
          TRUE ~ 9999
        )
    ) %>% 
    
    # Invert items
    mutate(
      DIR = 
        case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
          TRUE ~ DIR
        )
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  

  # MANUAL CORRECTION -------------------------------------------------------
  
  manual_correction_output = "outputs/data/manual_correction/fauxPasEv_manual_correction.xlsx"
  manual_correction_input = "data/manual_correction/fauxPasEv_manual_correction.xlsx"
  
  
  # Save DF with OPEN responses for manual correction ---
  
  if (!file.exists(dirname(here::here(manual_correction_output)))) dir.create(dirname(here::here(manual_correction_output)))
  DF_long_DIR %>% 
    filter(DIR == 123456789) %>% 
    writexl::write_xlsx(here::here(manual_correction_output))
  
  
  # Read manual correction file ---
  if (file.exists(manual_correction_input)) {
    DF_manual_correction = readxl::read_excel(here::here("data/manual_correction/fauxPasEv_manual_correction.xlsx")) 
  } else {
    cli::cli_abort(c("Correction file NOT found.",
                     "You need to:", 
                     "1) Copy '{manual_correction_output}' to '{manual_correction_input}'", 
                     "2) manually correct responses in '{manual_correction_input}'",
                     "- Look for '123456789' in the DIR column and replace with numeric value"))
  }
  
  
  # CHECKS ---
  
  uncorrected_responses = nrow(DF_manual_correction %>% filter(DIR == 123456789))
  if (uncorrected_responses > 0) cli::cli_abort(c("{uncorrected_responses} responses not corrected in:",
                                                  "- '{manual_correction_input}'",
                                                  "- *Look for '123456789' in the DIR column and replace with numeric value*"))
  
  
  # Add manual correction file back ---
  
  DF_long_DIR_manually_corrected = 
    DF_long_DIR %>% 
    # Read DF_long_DIR without the manually corrected items
    filter(DIR != 123456789) %>% 
    # Add manually corrected items from manually corrected file
    bind_rows(DF_manual_correction)
  
  # ****************************************************************************
  

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR_manually_corrected %>% 
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
  
  
  # Reliability -------------------------------------------------------------
  
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd1)
  # items_RELd1 = REL1$item_selection_string
    
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    mutate(

      # Make sure to use the correct formula: rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!name_DIRd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      # !!name_DIRd2 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!name_RELd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 

      # Score Scale
      # !!name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
    )
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
