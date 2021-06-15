
# Install packages ---------------------------------------------------------

# Run the fist time or when you have an error

  # targets::tar_renv()
  # source("setup/setup.R")


# Sync data from server ---------------------------------------------------

  # EDIT ONLY THE id_protocol variable value
  id_protocol = 999
  
  # If you do not have the .credentials file: rstudioapi::navigateToFile("setup/setup_server_credentials.R")
  list_credentials = source(".vault/.credentials")
  if (!dir.exists(paste0(getwd(), '/data/' , id_protocol, '/'))) dir.create(paste0(getwd(), '/data/' , id_protocol, '/'))
  system(paste0('sshpass -p ', list_credentials$value$password, ' rsync -av --rsh=ssh ', list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, id_protocol, '/.data/ ', getwd(), '/data/' , id_protocol, '/'))
  

# Destroy cache (_targets folder) -----------------------------------------

  # Destroys cache to force a clean run
  targets::tar_destroy()
  
  # Invalidates input_files to force data preparation
  targets::tar_invalidate(matches("input_files"))

  
# Run project --------------------------------------------------------------

  targets::tar_make()

  # IF running megatron
  # TODO : create a test with this? Or maybe just the "manual" test will be enough
  targets::tar_load(DF_joined)
  number_items_tasks(DF_joined)

    
# Visualize targets networks -----------------------------------------------

  targets::tar_visnetwork(targets_only = TRUE, label = "time") #label = "time"
  # targets::tar_glimpse()
  
  # Global time and time per process
  sum(targets::tar_meta(fields = seconds)$seconds, na.rm = TRUE)
  targets::tar_meta() %>% select(name, seconds) %>% arrange(desc(seconds))


# See warnings in all functions ------------------------------------------
  targets::tar_meta(fields = warnings) %>% tidyr::drop_na(warnings)# See warnings


# Errors ------------------------------------------------------------------

  # If we get an error, load workspace of errored state
  targets::tar_workspace(TESTS)
  targets::tar_undebug() # Delete all the debugging stuff
  
  # WARNINGS
  targets::tar_meta(fields = warnings) %>% drop_na
  
  # Data frame of targets info
  targets::tar_manifest(fields = "command")
