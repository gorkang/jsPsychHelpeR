# Step by step instructions to create a Dockerfile in the project's main folder 
# build the docker image and run the docker container

# SEE https://gorkang.github.io/jsPsychR-manual/qmd/06-jsPsychRadmins.html#docker-containers
# SEE https://gorkang.github.io/jsPsychR-manual/qmd/06-jsPsychRadmins.html#debug-container



PID = 999
system(paste0("docker run --rm -ti -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID, " /bin/bash"))
# Clean environment -------------------------------------------------------

  # DELETE ALL CACHE
  system("docker builder prune --all -f")
  
  # Clean renv cache libraries
  renv::clean()

  # Clean extra versions of libraries
  source("R/helper_functions_extra.R")
  clean_renv_cache()


# Create docker container -----------------------------------------------------  
  
  PID = 999
  jsPsychHelpeR::create_docker_container(PID = PID)


# Run docker image --------------------------------------------------------
  
  # Run project inside a docker container. Output in Downloads/jsPsychHelpeR/pid[PID]/
  file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
  system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID))
  # docker tag gorkang/jspsychhelper:pid999 pid999:latest
  # ***DEBUG*** 
  # docker run --rm -ti -v ~/Downloads/jsPsychHelpeR999/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid999 /bin/bash
  # See last CMD line in Dockerfile_TEMPLATE:
    # source('renv/activate.R'); invisible(lapply(list.files('./R', full.names = TRUE, pattern = '.R$'), source)); setup_folders(pid = 999, folder = '.'); targets::tar_destroy(ask = FALSE); targets::tar_make()
    # du -had1 renv/ # Check size folders
  # *** ***
  
  
  # Windows
  # In cmd:
  paste0("tar -xf pid", PID, ".tar.zip & ", 
         "docker load input - & ",
         r"(docker run --rm -d --name jspsychhelper -v %USERPROFILE%\Downloads\jsPsychHelpeR\outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid)", PID
         )



# SHARE image -------------------------------------------------------------
  
  # List images
  system("docker images")
  
  # Creates a pid[PID].tar.zip file TO SHARE the container
  system(paste0("docker save gorkang/jspsychhelper:pid", PID, " | zip > pid", PID, ".tar.zip"))
  
  
  # UNZIP and load in new computer:
  utils::unzip(zipfile = paste0("pid", PID, ".tar.zip"), files = paste0("-"))
  system(paste0("docker load --input -"))
  

# SHARE with dockerhub ----------------------------------------------------

  # TAG image
  # system(paste0("docker tag gorkang/jspsychhelper:pid", PID, " pid", PID, ":latest"))
  
  # PUSH TO DOCKERHUB
  system(paste0("docker push gorkang/jspsychhelper:pid", PID))
  
  # GET IMAGE
  system(paste0("docker pull gorkang/jspsychhelper:pid", PID))
