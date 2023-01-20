# This creates a Dockerfile in the project's main folder, builds the docker container and runs it

PID = 999

source("admin/helper-scripts-admin.R")
create_dockerfile(PID = PID)
system(paste0("docker build -t pid", PID, " ."))
system(paste0("docker run --rm -d --name jspsychhelper -v ~/Downloads/jsPsychHelpeR/outputs:/home/project/jsPsychHelpeR/outputs:rw ", PID))
list.files("~/Downloads/jsPsychHelpeR/outputs", recursive = TRUE)

# TO SHARE
docker save pid999 | zip > pid999.tar.zip # regular 2.1 GB, zip 944 MB

# UNZIP and:
docker load --input latestversion-1.0.0.tar