# jsPsychHelpeR

Standardize and automatize data preparation and analysis of [jsPsych](https://www.jspsych.org/) experiments created with [jsPsychMaker](https://github.com/gorkang/jsPsychMaker). See the [jsPsychHelpeR manual](https://gorkang.github.io/jsPsychR-manual/qmd/05-jsPsychHelpeR.html) for more detailed information.  

Please, address any correspondence to [gorkang\@gmail.com](mailto:gorkang@gmail.com)


## Who can use this

If you ran or simulated participants on a protocol created with [jsPsychMaker](https://github.com/gorkang/jsPsychMaker) you can use jsPsychHelpeR.


## How to use

First, install the jsPsychHelpeR package:

```
if (!require('remotes')) utils::install.packages('remotes'); remotes::install_github('gorkang/jsPsychHelpeR')
```

Then, to create and configure an RStudio project with the data preparation, run one of the following options.  

---  


**OPTION 1) If you have the raw data in your computer**

Replace 'pid' below with your project ID  
Use the `folder` parameter to select a specific folder for the RStudio project  
Use the `data_location` parameter to indicate the location of the raw data  

```
jsPsychHelpeR::run_initial_setup(pid = '999', 
                                 data_location = '~/Downloads/JSPSYCH/999/', 
                                 folder = '~/Downloads/jsPsychHelpeR_999/')
```
  
  
**OPTION 2) If you have the FTP credentials in .vault/.credentials**

```
jsPsychHelpeR::run_initial_setup(pid = '999', 
                                 download_files = TRUE)
```
 
---  

If you are using RStudio, the new project will open. In there, open `run.R` and follow the instructions.  


### First run

Visualize pipeline: `targets::tar_visnetwork()`

Start data preparation: `targets::tar_make()`  


### Outputs

All the Rmd reports, tables, figures, etc will appear in the `outputs` folder.  

To list the available objects (dataframes, etc.): `targets::tar_objects()`.  

To load an object `targets::tar_load()`. For example: `targets::tar_load(DF_analysis)`.  

  


## Data preparation and analysis

We use the [targets](https://github.com/wlandau/targets) package.

**The whole process can be reproduced running `targets::tar_make()`**

A nice visualization of all the pre-processing steps can be seen with `targets::tar_visnetwork(targets_only = TRUE)`

The file `_targets.R` contains the important parameters and calls to all the functions used when running `targets::tar_make()`

To see more detail about any specific step, you can:

1.  Go to the relevant function
2.  Load the input parameters of the function with `targets::tar_load()`
3.  Run the code step by step as you would normally do

## Output tables and plots

-   **Plots**, **tables** and **reports** are in `outputs/plots`, `outputs/tables`and `outputs/reports` respectively.

-   **Dataframes** for different stages of data processing can be seen in `outputs/data`
