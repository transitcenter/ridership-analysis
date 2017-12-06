###########################################################################
##
## Create MSA point shapefile
##
## Turn msa change transit csv into a shapefile for carto. Create an sf
## spatial object by ceocoding the primary cities of each msa then 
## join change variables to that object before writing it to a zipped
## shapefile in the output directory
##
## This script is heavily commented in an effort to be exceedingly 
## transparent about our methods for calculating each msa-level transit
## metric.
## 
## Note: run 'TransitCenter_match_agencies_to_msas.R' and 
## 'TransitCenter_data_wrangling_TS21.R' before this script
###########################################################################

library(ggmap)
library(tidyverse)
library(sf)

# Change dataset
change_df <- read.csv("data/input/msa_change_transit_vars.csv") 

# this function takes an msa name and geocodes the primary city in that
# msa. It returns a named vector of msa_name and coordinates
get_coordinates <- function(msa_name) {
  # find the name and state of the primary city in the MSA
  main_city <- gsub("-.*,|$", "", msa_name) %>% 
    gsub("-.*$", "", .) %>% 
    gsub(",", "", .)
  # geocode that primary city
  coords <- geocode(main_city)
  # combine msa name and coordinates into a named vector
  cols <- c('name_msa' = msa_name, coords)
}

# Apply get_coordinates over all 55 msas
msas_spatial <- map_df(change_df$name_msa, get_coordinates) %>%
  # convert the output into a spatial object
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  # join the change_df dataset to this sf object
  left_join(change_df)

# find all components of shapefile in output directory
find_shp_files <- function(directory, shapefile_name) {
  d <- dir(directory, full.names = TRUE)
  files <- d[grepl(shapefile_name, d)]
}

# removes previous version of shapefile components
remove_shapefiles <- function(directory, shapefile_name, keep_zip) {
  files <- find_shp_files(directory, shapefile_name)
  
  if (keep_zip) {
    files <- files[!grepl('.zip', files)]
  }
  
  for (f in files) {
    file.remove(f)
  }
}

# zip up shapefile and remove the original components
zip_shapefile <- function(directory, shapefile_name) {
  files <- find_shp_files(directory, shapefile_name)
  outfile <- paste0(shapefile_name, '.zip') %>%
    file.path(directory, .)
  print(paste0("zipping to: '", outfile, "'"))
  zip::zip(outfile, files)
  remove_shapefiles(directory, shapefile_name, TRUE)
}

# clear directory and output a zip files
output_zipshapefile <- function(sp_object, directory, shapefile_name) {
  remove_shapefiles(directory, shapefile_name, FALSE)
  st_write(sp_object, file.path(directory, paste0(shapefile_name, '.shp')))
  zip_shapefile(directory, shapefile_name)
}

output_zipshapefile(msas_spatial, 'data/output/', 'msa_change_transit_vars')


