# jsPsychHelpeR 0.2.7

Major updates
* Adapted to work with targets 1.3.0

Minor updates

* Improved some messages
* Delete snaps if not in protocol 999
* Improved input_files test
* Some small fixes to renv cache
* Updated DT version to avoid a false positive in Google malware detector


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
