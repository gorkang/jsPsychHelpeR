
# Install packages ---------------------------------------------------------

# targets::tar_renv()  

# Descomentar y correr la primera vez o cuando hay errores
# source("setup/setup.R")


# Sync data from server ---------------------------------------------------

id_protocol = 1
server_folder = "/"
local_folder = "/"
system(paste0('rsync -av --rsh=ssh user-cscn@138.197.236.86:', server_folder, '', id_protocol, '/.data/ ', local_folder))


# Destroy cache (_targets folder) -----------------------------------------

  # Destroys cache to force a clean run
  # targets::tar_destroy()
  
  # Invalidates input_files to force data preparation
  targets::tar_invalidate(matches("input_files"))

  
# Run project --------------------------------------------------------------

  # Recreates _packages.R with the above packages (so renv founds them)
  # targets::tar_renv() # Need to run renv::init() if anything changes

  targets::tar_make()

  
# Visualize targets networks -----------------------------------------------

  targets::tar_visnetwork(targets_only = TRUE, label = "time") #label = "time"
  # targets::tar_glimpse()
  
  # Global time and time per process
  targets::tar_meta() %>% summarise(sum(seconds, na.rm = TRUE))
  targets::tar_meta() %>% select(name, seconds) %>% arrange(desc(seconds))


# See warnings in all functions ------------------------------------------
  targets::tar_meta(fields = warnings) %>% tidyr::drop_na(warnings)# See warnings


# Errors ------------------------------------------------------------------

  # If we get an error, load workspace of errored state
  targets::tar_workspace(TESTS)
  targets::tar_undebug() # Delete all the debugging stuff
  
  # Data frame of targets info
    targets::tar_manifest(fields = "command")
