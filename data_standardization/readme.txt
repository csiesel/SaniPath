README: ~~SaniPath Database~~
==============================
Scripts:
==============================
Data Standardization (ds):
==============================
-main.R                         
starts all ds files and generates output for each. outputs are saved in data folder with the date of file generation (e.g. col_merged_2020-01-15.csv)

  *ds_col_v*.R
  *ds_community_v*.R
  *ds_household_v*.R
  *ds_lab_v*.R
  *ds_school_v*.R

each merged file includes all of the available data for this specific survey/ sample collection/ lab procedure and follows the same naming conventions.
Below are more details about each of the outputs as well as the data folder structure.

after database files have been run, the ec_calculator.R script can be executed to generate the ec_data file (in data folder)

==============================
Output files:
==============================
-c_merged*.csv		community survey
-s_merged*.csv		school survey
-h_merged*.csv		household survey
-col_merged*.csv	sample collection
-lab_merged*.csv	laboratory results
-ec_data*.csv		calculated ecoli values for all samples


Other R script files in the main folder are helper functions for generating graphs in the markdown or other programs, multi city analysis scripts, tree maps etc. each file does have a short description on the top of the file - describing what is going on.

======================================================
FILES:
======================================================
│   ds_col_v*.R
│   ds_community_v*.R
│   ds_household_v*.R
│   ds_lab_v*.R
│   ds_school_v*.R
│   ec_calculator.R
│   main.R
│   readme.txt
├───analysis
│       part of the tool is saved here (ecoli calculation part - for generating the ec-
│	data files)
├───archive
│       old files for backup
├───data
│   │  all merged database files
│   │
│   ├───deployments
│   │      contains all related data for each of the deployments (odk, raw data, data 
│   │      cleaning scripts, notes, etc.) each deployment is clearly marked.
│   │
│   ├───extra_data
│   │       contains all variables that are not in the standard database for each of the 
│   │       surveys and deployments.
│   └───meta_data
│  	    files about neighborhoods, sample IDs, produce, street food etc.
│
├───images
│       files for the multi city paper and various other graphics (treemap output)
│
│
└───markdown
        various files where markdown documents were made for dissemination etc.