# SENSITIVE DATA
# Function to deal with sensitive data (input files/credentials will be stored in .vault/, and NOT shared)
# We output df_AIM, which is run in a Google FORM

run_sensitive_data <- function(df_SDG, run_online_FORM = FALSE) {


  # DEBUG
  # if (!exists("run_online_FORM")) run_online_FORM = TRUE


  # Set options, load packages -----------------------------------------------
  lapply(list.files(".vault/R", full.names = TRUE, pattern = ".R"), source)
  
  

  # Online Google Form: ONLY RUN IF NECESSARY (API LIMITS) -------------------
  # Activate API: https://console.cloud.google.com/apis/credentials
  # See .vault/config/gs4_TEMPLATE.R
  if (run_online_FORM == TRUE) {
    cat(crayon::yellow("Fetching online FORM Google sheet...\n"))
    df_FORM = prepare_FORM(df_SDG, short_name_scale_str = "FORM")  
  } else {
    cat(crayon::yellow("Reading offline FORM Google sheet...\n"))
    df_FORM = read_rds(".vault/data/df_FORM_RAW.rds")
  }
  
  df_AIM = prepare_AIM_gsheet(df_FORM, short_name_scale_str = "AIM")
  
  
  # Report ------------------------------------------------------------------
  cat(crayon::yellow("Preparando report_candidatos...\n"))
  rmarkdown::render(".vault/doc/report_candidatos.Rmd", "html_document", quiet = TRUE, clean = TRUE, envir = new.env())
  
 return(df_AIM)
  
}