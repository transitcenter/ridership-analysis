# Compile NTD Data

This repository contains the necessary code and datasets to compile NTD and census datasets used in Azavea's visualization of transportation ridership for TransitCenter

### Getting started

The R scripts in this repository create the datasets used in the app. You can work through the scripts in the following order:

1. `TransitCenter_match_agencies_to_msas.R`
2. `TransitCenter_data_wrangling_TS21.R`
3. `TransitCenter_create_shapefile_of_change_variables.R`

All scripts rely on input files in the `data/input/` directory and write final outputs to the `data/output/` directory. Intermediate outputs (e.g. csvs that one script outputs but another uses as input) are written to `data/input/`.
