###########################################################################
##
## Match agencies to MSAs
##
## The objective of this script is to match each agency in the ntd dataset
## to the MSA that it is in and then to extract just the agencies that are 
## in the top 55 largest MSAs in the country. It outputs a csv with a row
## for each agency that falls in one of the selected msas. Each transit 
## agency will have it's four digit unique identifier as well as the unique
## id for the msa that it falls in. This dataset will be joined to ntd 
## datasets in order to filter out agencies that are not relevant to the
## analysis.
## 
## Steps:
##  1. Generate unique set of city state combinations from ntd dataste
##  2. Geocode all city-state pairs and create spatial points object
##  3. Load MSA spatial dataset and filter for just the top 55 msas
##  4. Spatially join geocoded cities msas
##  5. Join cities-with-msas back to agency dataset creating lookup table
##
###########################################################################

library(readxl)
library(tidyverse)
library(ggmap)
library(sf)

options(stringsAsFactors = FALSE)

# Step 1 ------------------------------------------------------------------
agencies_to_geocode <- read_excel("data/input/TS2.1TimeSeriesOpExpSvcModeTOS_6.xls", 
                                  "OpExp Total") %>%
  select(ntdid = `4 Digit NTDID`, city = City, state = State) %>%
  mutate(ntdid = str_pad(ntdid, 4, 'left', '0'),
         geocode = paste0(city, ', ', state, ' USA')) %>%
  filter(state != 'PR') %>%
  na.omit

searches <- unique(agencies_to_geocode$geocode)


# Step 2 ------------------------------------------------------------------
geocode_one <- function(search) {
  v = c(search, geocode(search))
  names(v)[1] <- 'geocode' 
  data.frame(v)
}

# all_cities_geocoded <- map_df(searches, geocode_one)
# write.csv(all_cities_geocoded, "data/input/all_cities_geocoded.csv", row.names = FALSE)
# The previous step will take ~15 minutes, do avoid running it, download
# the already-geocoded cities
all_cities_geocoded <- read.csv("data/input/all_cities_geocoded.csv")

agencies <- st_as_sf(na.omit(all_cities_geocoded), 
                     coords = c('lon', 'lat'), 
                     crs = 4326) 

# Step 3 ------------------------------------------------------------------
selected_msas <- read.csv("data/input/selected_msas.csv") %>%
  mutate_all(as.character)

csas <- st_read("data/input/msas/tl_2015_us_cbsa.shp") %>%
  select(GEOID.msa = GEOID) %>%
  left_join(selected_msas) %>%
  na.omit %>%
  st_transform(4326)

# Step 4 ------------------------------------------------------------------
agencies_in_msas_sp <- st_join(agencies, csas) %>%
  na.omit

agencies_in_msas <- agencies_in_msas_sp %>%
  as.data.frame %>%
  select(-geometry)

# Step 5 ------------------------------------------------------------------
ag <- agencies_to_geocode %>%
  left_join(agencies_in_msas) %>%
  select(-city, -state, -geocode) %>%
  na.omit %>%
  distinct

write.csv(ag, "data/input/agency_msa_lookup_table.csv", row.names = FALSE)
save(ag, file = "data/input/agency_msa_lookup_table.Rdata")
