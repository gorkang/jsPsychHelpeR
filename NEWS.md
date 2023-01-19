# jsPsychHelpeR 0.2.9999

Major updates  

* standardized_names now uses an output list instead of global variables.
* NEW get_dimensions_googledoc() function to help create new tasks
* input files can now be in a single ZIP file
* prepare_TEMPLATE() changed to use a named list for dimensions and items
* With admin/create_docker.R we can create a fully reproducible docker container for a specific pid
* Create DF_analysis_blinded 

Minor updates

* improvements in prepare_TEMPLATE: 
  + get dependencies to _targets_options.R
  + add output_formats parameter to make easier to use csv2 (easier to open for Spanish speaking users)
* Use more informative messages
* Squashed lots of small usability bugs
* Create wide versions of DF_raw and DF_clean to be able to check DF before having all tasks
* NEW functions for some of the recurrent admin tasks


# jsPsychHelpeR 0.2.0

Major updates  

* Works with jsPsychMaker 0.2.0
* Working with ~60 tasks
* Automatic setup with run_initial_setup()
* Should be fine when we have multiple responses per screen and multi-select
* First take to automatically create a codebook
* First take to create Snapshots of each task

