# 0.2.0.

- Add column `polygon` to the boundaries data.frame to uniquely identify the polygons inside a given watershed that conform the boundaries.
- Fix the curation procedures of the vegetation name for the boundaries files. Non-perennial crops are now consistenly labeled as "Row crop", whereas previously they would be named either "Row crop" or "Row Crop".
- New auditory scripts: data-raw/run_folder_analysis.R (Scan for duplicated shapefiles) and data-raw_run_compare_shapefiles.R (Compare the content of two shapefiles and print the main differences).
