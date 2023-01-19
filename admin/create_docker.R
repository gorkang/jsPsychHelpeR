# This creates a Dockerfile in the project's main folder, builds the docker container and runs it

source("admin/helper-scripts-admin.R")
create_dockerfile(PID = 999)
system("docker build -t project .")
system("docker run --rm -d --name jspsychhelper -v ~/Downloads/jsPsychHelpeR/outputs:/home/project/jsPsychHelpeR/outputs:rw project")
list.files("~/Downloads/jsPsychHelpeR/outputs", recursive = TRUE)