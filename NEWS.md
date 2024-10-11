# jsPsychHelpeR 0.3.5.905

Major updates

* All prepare_TASK() are using the same format now (closes #4)
* All prepare_TASK() are using the new pipe (|>) and are a bit faster now

Minor updates

* Sync version number with admin, maker, monkeys and manual
* clean_names_analysis() to go from `TEST_DimensionName_DIRd` to `TEST Dimension Name`. Should be useful for tables, plots, etc.
* sync_server_local() and run_initial_setup() should work now for development protocols.
* Add digits parameter to create_number_series() so create_new_task(get_info_googledoc = TRUE) will output 3 digit trialid's  
* All credentials parameters are now credentials_file instead of list_credentials
* Replace all readr instances for data.table to reduce dependencies and speed up things
* Simplify save_files
* Get rid of some unused files/functions
* Started replacing %>% by |> elsewhere
* Add output_formats parameter (closes #33)
* csv files with 'repeated' in the name will be ignored
* Improve run.R


# jsPsychHelpeR 0.2.7

Major updates
* Adapted to work with targets 1.3.0

Minor updates

* Improved some messages
* Delete snaps if not in protocol 999
* Improved input_files test
* Some small fixes to renv cache
* Updated DT version to avoid a false positive in Google malware detector
* Fixed a couple bugs in report_PROGRESS
* Add tasks


# jsPsychHelpeR 0.2.6

Minor updates

* Fixed issues with translated tasks (BNTen)
* Fixed group report
* Other minor fixes and clean ups



# jsPsychHelpeR 0.2.5

Major updates  

* jsPsychHelpeR is now an R package
* We can check the canonical_protocol outputs with testthat now, making sure results don't change
* standardized_names now uses an output list instead of global variables.
* NEW get_dimensions_googledoc() function to help create new tasks
* input files can now be in a single ZIP file
* prepare_TEMPLATE() changed to use a named list for dimensions and items
* With admin/create_docker.R we can create a fully reproducible docker container for a specific pid
* Create DF_analysis_blinded to help with blinded analyses. See MacCoun, R., & Perlmutter, S. (2015). Blind analysis: Hide results to seek the truth. Nature, 526(7572), 187-189 (https://doi.org/10.1038/526187a), or Sarafoglou, A., Hoogeveen, S., & Wagenmakers, E. J. (2023). Comparing analysis blinding with preregistration in the many-analysts religion project. Advances in Methods and Practices in Psychological Science, 6(1), 25152459221128319. (https://doi.org/10.1177/25152459221128319)

Minor updates

* improvements in prepare_TEMPLATE: 
  + get dependencies to _targets_options.R
  + add output_formats parameter to make easier to use csv2 (easier to open for Spanish speaking users)
* Use more informative messages
* Squashed lots of small usability bugs
* Create wide versions of DF_raw and DF_clean to be able to check DF before having all tasks
* NEW functions for some of the recurrent admin tasks
* Can use the canonical prepare_FUN for translated tasks where the logic is the same (e.g. BNTen.js can use prepare_BNT()). See `create_targets_file()`
* When running initial setup, data can be downloaded, or a local folder with the original data can be passed to the function
* sensitive data is moved out of the data folder regardless of the origin (downloaded or local)


# jsPsychHelpeR 0.2.0

Major updates  

* Works with jsPsychMaker 0.2.0
* Working with ~60 tasks
* Automatic setup with run_initial_setup()
* Should be fine when we have multiple responses per screen and multi-select
* First take to automatically create a codebook
* First take to create Snapshots of each task
