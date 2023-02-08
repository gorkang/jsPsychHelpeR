FROM rocker/r-ver:4.2.2

RUN apt-get update && apt-get install -y libssl-dev libglpk-dev libxtst6 libxt6 pandoc pandoc-citeproc libzmq3-dev && apt-get autoclean -y && apt-get autoremove -y

# Create folders
RUN mkdir /home/project \
    && mkdir /home/project/jsPsychHelpeR

RUN addgroup --system project \
    && adduser --system --ingroup project project
    
# Add relevant folders
ADD .vault /home/project/jsPsychHelpeR/.vault
ADD data/999 /home/project/jsPsychHelpeR/data/999
ADD R /home/project/jsPsychHelpeR/R
ADD R_tasks /home/project/jsPsychHelpeR/R_tasks
ADD renv/cache /home/project/jsPsychHelpeR/renv/cache
ADD Rmd /home/project/jsPsychHelpeR/Rmd
ADD tests /home/project/jsPsychHelpeR/tests

# Add relevant files
COPY _targets_options.R /home/project/jsPsychHelpeR/_targets_options.R
COPY _targets.R  /home/project/jsPsychHelpeR/_targets.R 
COPY DESCRIPTION /home/project/jsPsychHelpeR/DESCRIPTION
COPY NEWS.md /home/project/jsPsychHelpeR/NEWS.md
COPY README.md /home/project/jsPsychHelpeR/README.md
COPY renv.lock /home/project/jsPsychHelpeR/renv.lock
COPY renv/activate.R /home/project/jsPsychHelpeR/renv/activate.R
COPY .Rprofile /home/project/jsPsychHelpeR/.Rprofile


# Install all dependencies with renv AND run initial setup for the specific pid
WORKDIR /home/project/jsPsychHelpeR
RUN R -e "source('.Rprofile'); source('renv/activate.R'); renv::restore(prompt = FALSE)"

# CHANGE USER to avoid running as root
# SEE 4: https://hosting.analythium.io/best-practices-for-r-with-docker/
#RUN chown project:project -R /home/project

# Switch to project as user
#USER project

# run_setup in local folder AND tar_make() # setup_folders() recreates the output/ folder structure in the local folder
WORKDIR /home/project/jsPsychHelpeR
CMD R -e "source('renv/activate.R'); invisible(lapply(list.files('./R', full.names = TRUE, pattern = '.R$'), source)); setup_folders(pid = 999, folder = '.'); targets::tar_make()"
