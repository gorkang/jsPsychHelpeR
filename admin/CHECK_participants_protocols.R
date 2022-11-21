# CHECKS the mysql database and extracts the number of participants completed, discarded and assigned per protocol


list_credentials = source(".vault/.credentials")
PASSWORD = list_credentials$value$password

source("admin/mysql_extract_tables.R")
source("admin/mysql_helper_functions.R")

#Load encrypted data
data_encrypted <- readRDS(".vault/data_encrypted.rds")

#Public key (its okay to hardcode it here, public keys are designed for sharing)
key_public = "39 0a 2a dc b7 1b 82 9d 95 d2 cd 98 d6 32 19 ff 91 33 ce 06 80 88 1e 9d 21 ac 0a 19 9d 1a 70 19"


#Private key
key_private <- sodium::sha256(charToRaw(PASSWORD))

#Check if private key provided is correct
if(paste(sodium::pubkey(key_private), collapse = " ") == key_public) {
  

  #Unencrypt data and make it available elsewhere
  data_unencrypted <- unserialize(sodium::simple_decrypt(data_encrypted, key_private))
  
  # Extract tables using data_unencrypted
  LIST_tables = extract_tables(list_credentials = data_unencrypted, serial_parallel = "parallel") # ~7s
  
  DF_user = LIST_tables$user
  
  DF_table_clean = DF_user |> 
    dplyr::group_by(id_protocol) |> 
    dplyr::count(status) |> 
    tidyr::pivot_wider(names_from = status, values_from = n)
  
}

DF_table_clean

# Total online participants in MySQL DB
DF_table_clean |> 
  dplyr::ungroup() |> 
  dplyr::summarize(TOTAL_completed = sum(completed))
