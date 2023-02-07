
# Create docker image -----------------------------------------------------

  # This creates a Dockerfile in the project's main folder and builds the docker container
  
  
  # DELETE ALL CACHE
  # docker builder prune
  # Clean renv cache: renv::clean()

  
  
  
  # Clean unused cache libraries
  renv::clean()

  # Clean extra versions of libraries
  source("R/helper_functions_extra.R")
  clean_renv_cache()

  
  
  # MISSING SOME FILES: 
  # Probably also remove some from dockerfile
  
  # renv::install("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR_0.2.0.tar.gz")  
  PID = 999
  jsPsychHelpeR::create_docker_container(PID = PID)
  #60 sec elapsed from CLEAN cache


# Run docker image --------------------------------------------------------
  
  # Run project inside a docker container. Output in Downloads/jsPsychHelpeR/pid[PID]/
  file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
  system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID))
  
  # DEBUG AS WOULD RUN
  # docker run --rm -ti -v ~/Downloads/jsPsychHelpeR999/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid999 /bin/bash
  
  
  # Windows
  # In cmd:
  paste0("tar -xf pid", PID, ".tar.zip & ", 
         "docker load input - & ",
         r"(docker run --rm -d --name jspsychhelper -v %USERPROFILE%\Downloads\jsPsychHelpeR\outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid)", PID
         )



# SHARE image -------------------------------------------------------------
  
  # Creates a pid[PID].tar.zip file TO SHARE the container
  system(paste0("docker save pid", PID, " | zip > pid", PID, ".tar.zip"))
  # regular OLD 1.89 GB #zip 699 MB # 90 sec elapsed
  # regular 1.77 GB #zip 689 MB
  
  # xz, bz2, gz are slower and/or not significantly smaller
  
  
  # UNZIP and load in new computer:
  utils::unzip(zipfile = paste0("pid", PID, ".tar.zip"), files = paste0("pid", PID, ".tar.zip"))
  # docker load --input pid999.tar
  

# SHARE with dockerhub ----------------------------------------------------

  # PUSH TO DOCKERHUB
  # docker tag pid999:latest gorkang/jspsychhelper:pid999
  # docker push gorkang/jspsychhelper:pid999 
  
  # GET IMAGE
  # docker pull gorkang/jspsychhelper:pid999



