# jsPsychHelpeR

Project template and set of helper scripts for data preparation and analysis of [jsPsych](https://www.jspsych.org/) experiments created with [jsPsych-maker](https://github.com/gorkang/jsPsych-maker).

Please, address any correspondence to [gorkang\@gmail.com](mailto:gorkang@gmail.com){.email}


![](img/jsPsych-trinity.png)



## Setup

1.  Run `setup.R` :

-   Makes sure all the necessary packages are present
-   Makes sure all the necessary folders are present

2. Copy the output .csv files to data/NUMBER_PROJECT (e.g. data/0)

3. Run `create_targets_file()` to create a _targets.R file for your data. For example:

```
invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
create_targets_file(pid_protocol = 0, folder_data = "data/0/")
```   

## First run

1. To make sure there are no old objects: `targets::tar_destroy()`

2. Run `targets::tar_make()` to start!


## Outputs

All the Rmd reports, tables, figures, etc will appear in the `outputs` folder.  

To list the available objects (dataframes, etc.): `targets::tar_objects()`.  

To load an object `targets::tar_load()`. For example: `targets::tar_load(DF_analysis)`.  

  


## Data preparation and analysis

We use the {targets} (<https://github.com/wlandau/targets>) package.

**The whole process can be reproduced running `targets::tar_make()`**

A nice visualization of all the pre-processing steps can be seen with `targets::tar_visnetwork(targets_only = TRUE)`

The file `_targets.R` contains the important parameters and calls to all the functions used when running `targets::tar_make()`

To see more detail about any specific step, you can:

1.  Go to the relevant function
2.  Load the input parameters of the function with `targets::tar_load()`
3.  Run the code step by step as you would normally do

## Output tables and plots

-   **Raw data** (anonymized) are in `data/raw_data_anonymized.csv`

-   **Plots**, **tables** and **reports** are in `outputs/plots`, `outputs/tables`and `outputs/reports` respectively.

-   **Dataframes** for different stages of data processing can be seen in `outputs/data`
