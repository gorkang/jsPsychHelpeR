# SEE https://gorkang.github.io/jsPsychR-manual/qmd/06-jsPsychRadmins.html

# CHECK all protocols --------------------------------------------------------

 # - Sync server with ../CSCN-server
 # - CHECK trialid's 
 # - CHECK names of tasks 
 # - CHECK tasks for which we do not have google docs, or prepare_ scripts...

  rstudioapi::navigateToFile("admin/check_ALL_protocols.R")


# Create NEW_TASKS protocol --------------------------------------------------
  
  # Create NEW_TASKS protocol with all the tasks for which we do not have a csv in 999/
  # To make sure the Github `jsPsychMaker/protocols_DEV/NEW_TASKS/` is up to date,  
    # UPLOAD `CSCN-server/.../NEW_TASKS` to the server
    # DOWNLOAD NEW_TASKS to `../jsPsychMaker/protocols_DEV/NEW_TASKS/`  


  rstudioapi::navigateToFile("admin/create_protocol_with_NEW_tasks.R")
  

# CHECK CANONICAL ------------------------------------------------------------

  # check that the canonical protocol in development works and expected.
  # In the script you can:
  #   
  #   -   sync `canonical_protocol_DEV/` folder in jsPsychMaker to `999/` in the server
  # -   launch 5 monkeys
  # -   rename the csv files to a fixed date, etc.
  # -   prepare data
  # -   compare with snaphot (WIP)

  rstudioapi::navigateToFile("admin/000_CHECK_CANONICAL.R")
  
  
