###########################################################################
##
## Compile NTD data from source
##
## This script takes raw data from 'TS2.1TimeSeriesOpExpSvcModeTOS_6.xls'
## (an NTD summary file), extracts the appropriate variables, filters for
## only the necessary agencies, and compiles them into a single dataset.
## It outputs a csv with values of each ntd variable for each year 2006-
## 2016. It also creates calculates (2006-2015) for all ntd variables
## and outputs those change vairables as a separate dataset.
##
## This script is heavily commented in an effort to be exceedingly 
## transparent about our methods for calculating each msa-level transit
## metric.
## 
## Steps:
##  1. Combine sheets of excel file into useful format
##  2. Add census data
##  3. Add gas prices data
##  4. Calculate change matrix
##
## Note: run 'TransitCenter_match_agencies_to_msas.R' before this script
###########################################################################

library(plyr)
library(tidyverse)
library(readxl)

options(stringsAsFactors = FALSE)

# Step 1: Combine NTD data ------------------------------------------------

# input data and lookup tables
excel_file <- 'data/input/TS2.1TimeSeriesOpExpSvcModeTOS_6.xls'
mode_categories <- read.csv('data/input/mode_categories.csv')
msa_agency_lookup <- read.csv('data/input/agency_msa_lookup_table.csv')

# define sheets that we will need from excel file
column_names <- c('VRM'='revenue_miles', 'VRH'='revenue_hours', 'FARES'='fares',
                  'UPT'='upt_total', 'OpExp Total'='total_expenses')
sheets <- c('revenue_miles'='VRM', 'revenue_hours'='VRH', 'fares'='FARES', 
            'upt_total'='UPT', 'total_expenses'='OpExp Total')

# this function takes an excel file and the name of a sheet in that file
# and returns a dataframe with one ntd variable for each year of each of 
# the 55 msas in our study set
read_sheet <- function(sheet, excel_file) {
  
  # read in sheet
  df <- read_excel(excel_file, sheet) %>%
    # select necessary variables
    select(ntdid = `4 Digit NTDID`, 
           agency = `Agency Name`, 
           mode = Mode,
           `2006`:`2016`) %>%
    # replace leading zeros that may have been stripped from ntdid
    mutate(ntdid = as.character(str_pad(ntdid, 4, 'left', '0'))) %>%
    # filter the dataset for only agencies in relevant msas
    filter(ntdid %in% msa_agency_lookup$ntdid) %>%
    # add an MSA field by joining the lookup table from 
    # 'TransitCenter_match_agencies_to_msas.R'
    left_join(msa_agency_lookup) %>%
    # convert to long-form dataset
    gather(year, sheet, -c(ntdid:mode, GEOID.msa, name_msa)) %>%
    # convert year field to numeric
    mutate(year = as.numeric(year)) %>%
    na.omit(sheet) %>%
    # the data come in with a number of different transit modes. We are
    # only interested in three categories so we must associate each mode
    # with a 'mode-category' using another lookup table that we imported
    # at the top
    left_join(mode_categories) 
  
  # first calculate the overall msa-wide sum of ntd variable
  by_msa <- df %>%
    # group by msa-year 
    group_by(GEOID.msa, name_msa, year) %>%
    # and calculate the sum of our only variable
    dplyr::summarise(msa_total = sum(sheet))

    # then rename the summary variable
  names(by_msa)[ncol(by_msa)] <- column_names[[sheet]]
  
  # upt is the only variable for we are interested in mode-level metrics
  if (sheet == 'UPT') {
    by_mode <- df %>%
      # remove non-rail or bus
      filter(category != 'other') %>%
      # group by msa-year-category
      group_by(GEOID.msa, name_msa, year, category) %>%
      # find the sum for each grouping
      dplyr::summarise(msa_total = sum(sheet)) %>%
      # widen the dataset to include columns for each mode category
      spread(category, msa_total) %>%
      # rename those columns
      dplyr::rename(upt_bus = bus, upt_rail = rail)
    
    # join this df to the original by_msa dataframe the 'UPT' output 
    # from this function will have three measurement variables
    by_msa <- by_msa %>%
      left_join(by_mode)
  }
  as.data.frame(by_msa)
}

# apply this dataframe over the list of sheets and join them into one dataframe
df <- llply(sheets, read_sheet, excel_file = excel_file) %>%
  plyr::join_all() 

# calculate derived ntd variables
df <- df %>%
  mutate(
    # Average Speed
    average_speed = revenue_miles/ revenue_hours,
    # Average Fare
    avg_fare = fares /  upt_total,
    # Farebox Recovery Ratio
    farebox_recovery = fares / total_expenses)

# calculate annual pct change since 2010 for ridership variables. It takes
# a dataframe of yearly transit variables and returns the same data frame 
# with change variables joined to it
calculate_pct_change <- function(df) {
  df %>%
    # create data frame of just the relevant years
    filter(year > 2009) %>%
    # and the relevant columns
    select(name_msa, year, upt_bus, upt_rail, upt_total) %>%
    # add 2010 values as a column
    left_join(filter(., year == 2010), by = "name_msa", suffix = c("", ".10")) %>%
    select(-year.10) %>%
    # calculate % change varaibles
    mutate(
      upt_bus_chg = (upt_bus - upt_bus.10) / upt_bus.10,
      upt_rail_chg = (upt_rail - upt_rail.10) / upt_rail.10,
      upt_total_chg = (upt_total - upt_total.10) / upt_total.10
    ) %>%
    # remove non % change variables
    select(name_msa, year, ends_with('chg')) %>%
    # join to the original dataset
    left_join(df, .)
}

df <- calculate_pct_change(df)

# this function takes the yearly ntd dataset and calculates the yearly
# national averages. It returns the original dataset with the averages
# bound to the bottom of it
get_national_averages <- function(df) {
  d <- df %>%
    # creates geeneric GEOID and name
    mutate(GEOID.msa = "NNNNN", name_msa = "National Average") %>%
    # find the yearly avergae
    group_by(GEOID.msa, name_msa, year) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    ungroup %>%
    # bind to original dataset
    rbind(df, .)
}

df <- get_national_averages(df)
#
# Step 2: Add census data -------------------------------------------------

# this function takes a numeric vector and finds the percent change
# from the first value to each number in that vector
find_pct_change <- function(vec) {
  origin <- first(na.omit(vec))
  change <- (vec - origin) / origin
  return(change)
}

# read dataset of yearly MSA-level census data
msa_census <- read.csv("data/input/msa_yearly_census_variables.csv")
msa_census <- msa_census %>%
  # generate additional fields to match existing carto dataset. Some of
  # these are redundant but create them in the interest of consistency
  mutate(tot_labor = (pct_emp / 100) * tot_pop, # total in labort force
         # total foreign population
         tot_pop_foreign = (pct_pop_foreign / 100) * tot_pop, 
         tot_emp = (pct_emp / 100) * tot_pop, # total number employed
         # total number of households without a vehicle
         hh_no_vehicle = (pct_hh_no_vehicle / 100) * tot_pop) %>%
  # group by MSA
  group_by(geoid_msa) %>%
  # calculate yearly change-since-2010 variables 
  mutate(emp_chg = find_pct_change(tot_labor), # total employed
         pop_chg = find_pct_change(tot_pop)) %>% # total population
  # rename GEOID field to match carto dataset
  dplyr::rename(GEOID.msa = geoid_msa) %>% 
  data.frame() 

# add dataset with square mileage of each MSA 
area <- read.csv("data/input/msa_area_lookup.csv") %>%
  mutate(GEOID.msa = as.character(GEOID.msa))

# join area table to census variables
msa_census <- left_join(msa_census, area) %>%
  # calculate population density
  mutate(pop_dens = tot_pop / area_miles) %>%
  select(-name_msa)

# join to master dataset
df <- df %>%
  left_join(msa_census) 
#
# Step 3: Add gas prices --------------------------------------------------

# read in dataset of yearly gas prices by state
gas <- read.csv("data/input/statewide_yearly_gas_prices.csv") %>% select(-X)
# calculate the yearly national average gas price and bind it to the
# bottom of the dataset
gas_prices <- gas %>%
  group_by(year) %>%
  dplyr::summarise(State = 'ge', gas = mean(gas)) %>%
  select(State, year, gas) %>%
  rbind(gas, .)

# This function extracts the two letter abbreviation of the primary 
# state for each msa 
get_state <- function(msa_names) {
  gsub("-.*,|$", "", msa_names) %>% 
    gsub("-.*$", "", .) %>% 
    gsub(",", "", .) %>%
    substr(., nchar(.) - 1, nchar(.))
}

# Join the gas prices to the master dataset by state
df <- df %>%
  mutate(State = get_state(name_msa)) %>%
  left_join(gas_prices) %>%
  select(-State) 

write.csv(df, "data/output/msa_yearly_transit_vars.csv", row.names = FALSE)
#
# Step 4: Calculate change matrix -----------------------------------------

# this function takes the dataframe created in step 1 and a year value and
# generates a matrix of all ntd variables for each msa in that year
create_year_matrix <- function(df, a_year) {
  df %>%
    # filter the dataframe for the year in question
    filter(year == a_year) %>%
    # remove id variables
    select(-GEOID.msa, -name_msa, -year) %>%
    # convert to matrix
    as.matrix
}

# this function takes the yearly transit data frame, a start year, a finish
# year and calculates the change variables over that period for each msa
# TC has requested that some variables be reported as gross change while others
# will be % change the
create_change_df <- function(df, year_1, year_2) {
  # create a dataframe of msa id variables to join change matrix to
  df_id <- select(df, GEOID.msa, name_msa) %>% 
    dplyr::distinct()
  # create matrices for beginning and end years
  y1 <- create_year_matrix(df, year_1)
  y2 <- create_year_matrix(df, year_2)
  # since most variables will be reported as % change, start by calculating 
  # pct change for all of them
  change_matrix <- ((y2 - y1) / y1) * 100
  # convert the resulting matrix to a dataframe and bind it to the id dataframe
  change_df <- cbind(df_id, data.frame(change_matrix))
  
  # then go back and manually write over the variables for which we want to
  # show raw change
  change_df$avg_fare <- y2[,'avg_fare'] - y1[,'avg_fare']
  change_df$average_speed <- y2[,'average_speed'] - y1[,'average_speed']
  change_df$farebox_recovery <- y2[,'farebox_recovery'] - y1[,'farebox_recovery']
  
  return(change_df)
}

# yearly dataframe of just ntd data
ntd_yearly <- df %>%
  select(GEOID.msa:farebox_recovery, gas)

# create this dataset for 2006-2015
change_df <- ntd_yearly %>%
  filter(name_msa != "National Average") %>%
  create_change_df(year_1 = 2006, year_2 = 2015)

# this function appends all ntd fields for a given to the change dataset
# it takes both the change and yearly datasets, as well as a year, and 
# adds a column for each ntd variable of that year
add_year_variables <- function(change_df, yearly_df, year_val) {
  # create a dataframe of just the year we're interested in
  year_df <- yearly_df %>%
    dplyr::filter(year == year_val) %>%
    select(-year, -name_msa)
  # define a suffix to go on the end of the new columns
  # for example for 2016 it would be '_y16'
  suffix <- as.character(year_val) %>% substr(3, 4) %>%
    paste0("_y", .)
  # join the year dataset to the change dataset, effectively
  # appending columns
  change_df_with_year <- change_df %>%
    left_join(year_df, by = 'GEOID.msa', suffix = c('', suffix))
}

change_df <- change_df %>%
  add_year_variables(ntd_yearly, 2006) %>%
  add_year_variables(ntd_yearly, 2015) %>%
  add_year_variables(ntd_yearly, 2016)

write.csv(change_df, "data/output/msa_change_transit_vars.csv", row.names = FALSE)

