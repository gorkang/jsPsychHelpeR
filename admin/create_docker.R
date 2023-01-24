# This creates a Dockerfile in the project's main folder and builds the docker container

# DELETE ALL CACHE
# docker builder prune
# Clean renv cache: renv::clean()

source("admin/helper-scripts-admin.R")
PID = 999

create_docker_container(PID = PID)
#60 sec elapsed from CLEAN cache

# Run project inside a docker container. Output in Downloads/jsPsychHelpeR/pid[PID]/
file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw pid", PID))

# Windows
# In cmd:
paste0("tar -xf pid", PID, ".tar.zip & ", 
       "docker load input - & ",
       r"(docker run --rm -d --name jspsychhelper -v %USERPROFILE%\Downloads\jsPsychHelpeR\outputs:/home/project/jsPsychHelpeR/outputs:rw pid)", PID
       )


# Creates a pid[PID].tar.zip file TO SHARE the container
system(paste0("docker save pid", PID, " | zip > pid", PID, ".tar.zip"))
# regular OLD 1.89 GB #zip 699 MB # 90 sec elapsed
# regular 1.77 GB #zip 689 MB

# xz, bz2, gz are slower and/or not significantly smaller


# UNZIP and load in new computer:
unzip(zipfile = paste0("pid", PID, ".tar.zip"), files = paste0("pid", PID, ".tar.zip"))
# docker load --input pid999.tar