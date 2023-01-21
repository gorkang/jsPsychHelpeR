# This creates a Dockerfile in the project's main folder and builds the docker container

# DELETE ALL CACHE
# docker builder prune

source("admin/helper-scripts-admin.R")
PID = 999


tictoc::tic()
create_docker_container(PID = PID)
tictoc::toc()
#133 sec elapsed from CLEAN cache


# Run project inside a docker container
tictoc::tic()
file.remove(paste0("~/Downloads/jsPsychHelpeR", PID))
system(paste0("docker run --rm -d --name jspsychhelper -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw pid", PID))
# list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE)
tictoc::toc()


# Creates a pid[PID].tar.zip file TO SHARE the container
tictoc::tic()
system(paste0("docker save pid", PID, " | zip > pid", PID, ".tar.zip"))
tictoc::toc()
# regular 1.89 GB #zip 699 MB # 83 sec elapsed

# xz, bz2, gz are slower and/or not significantly smaller


# UNZIP and load in new computer:
unzip(zipfile = paste0("pid", PID, ".tar.zip"), files = paste0("pid", PID, ".tar.zip"))
# docker load --input pid999.tar