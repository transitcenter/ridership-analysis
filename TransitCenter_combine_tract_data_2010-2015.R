###########################################################################
## 
## Script Purpose: combine census tables and select the correct variables
##    
## Note:
## Before you start the script, download the tables DP02, DP03, DP04, and DP05 from American Fact Finder for every census tract in every state.
## Save each table for each year into the appropriate folders, as created in this repository. 
## There should be a folder for each year and a subfolder for each table. This is necessary because there will be multiple files for each 
## table for each year because of download limitations. These will need to be combined (which we do in this script).
## You will also need to make sure that there is a folder called tables_rbind within each year folder.
##
###########################################################################

library(plyr)
library(tidyverse)
library(sf)
library(stringr)

# Combine multiple files for the same year/table --------------------------

  #Create a variable that lists all of the years
  years <- c("2010", "2011", "2012", "2013", "2014", "2015")
  
  #Create a variable that lists all of the table names
  census_tables <- c("DP02", "DP03", "DP04", "DP05")
  
  
  # A for loop that loops through each table folder within each year folder. 
  # The loop reads the files names in each table folder and binds all of the rows of each file within each table folder.
  # This creates a dataset for each table for each year.
  
  for (i in years){
    for(j in census_tables){
      
      #Read in the file names within each table folder within each year folder
      raw_files <- dir(paste0("data/census/input/",i,"/",j,"/"), full.names=TRUE)
      
      #Read the files
      tables <- lapply(raw_files, read.csv, header = TRUE)
      
      #Remove the annotation row
      tables <- lapply(tables, function(x) x[-1,])
     
      #Bind the rows to create one dataset per table per year 
      combined.df <- do.call(rbind , tables)
      
      #Output each dataset into the tables_rbind folder for each year. An example file name: 2010_table_DP02
      write.csv(combined.df,paste0("data/census/input/",i,"/tables_rbind/",j,"_table_",i,".csv"), row.names=FALSE)
    }
  }


# Select and rename variables ---------------------------------------------

#Each table has many variables that will not be used in this analysis. 
#The variable names are also coded and need to be renamed for ease of use.
#A function is created to read the rbind tables, select the appropriate variables from each table and rename them.
#These files with selected and renamed variables are then joined together to make a profile of selected variables for each year

  #file example to select file name pattern
  file <- "data/census/input/2015/tables_rbind/DP02_table_2015.csv"
  
  #function to select the appropriate fields and rename them
  read_cen_csv <- function(file) {
    f <- read.csv(file)
    suffix <- gsub("^.*DP","", file)
    prefix <- paste0("DP", substr(suffix, 1, 2))
    names <- table_keys[prefix] %>%
      unlist %>%
      unname
    f <- f %>% 
      select(one_of(names))
    names(f) <- table_names[prefix] %>%
      unlist %>%
      unname
    return(f)
  }

#Field names vary based on the year and must be selected separetely.
#Run these lines in order to write over the table_names and table_keys variables with the correct field names based on year.

#### data for 2010-2012
  #names of original selected variables for DP02 for 2010-2012
  DP02_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC128", "HC01_VC134", "HC03_VC134")
  #new names for selected variables, in order
  DP02_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "tot_pop", "tot_pop_foreign", "pct_pop_foreign")
  
  #names of original selected variables for DP03 for 2010-2012
  DP03_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC29", "HC03_VC30", "HC03_VC31", "HC01_VC06", "HC01_VC07", "HC01_VC85", "HC03_VC156", "HC03_VC166")
  #new names for selected variables, in order
  DP03_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "pct_drove_alone", "pct_carpooled", "pct_transit", "tot_labor", "tot_emp", "med_hh_inc", "pct_fam_pov", "pct_pers_pov")
  
  #names of original selected variables for DP04 for 2010-2012
  DP04_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC82", "HC03_VC82")
  #new names for selected variables, in order
  DP04_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "hh_no_vehicle", "hh_pct_no_vehicle")
  
  #names of original selected variables for DP05 for 2010-2012
  DP05_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC43", "HC03_VC44", "HC03_VC50", "HC03_VC82")
  #new names for selected variables, in order
  DP05_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "pct_white", "pct_black", "pct_asian", "pct_hisp")
  
  #Create the table_names and table_keys variables, assigning the variables for 2010-2012
  table_names <- list("DP02"=DP02_names, "DP03"=DP03_names, "DP04"=DP04_names, "DP05"=DP05_names)
  table_keys <- list("DP02"=DP02_vars, "DP03"=DP03_vars, "DP04"=DP04_vars, "DP05"=DP05_vars)
  tables <- c()
  
  #Run the read_cen_csv function for 2010, join all files, and output a tract profile for 2010 with all selected and renamed variables from each table
  tables10 <- dir("data/census/input/2010/tables_rbind/", full.names = TRUE)
  files_10 <- llply(tables10, read_cen_csv, .progress = "text")
  tracts_profile_2010 <- join_all(files_10)
  write.csv(tracts_profile_2010, "data/census/output/tracts_profile_2010.csv", row.names=FALSE)
  
  #Run the read_cen_csv function for 2011, join all files, and output a tract profile for 2010 with all selected and renamed variables from each table
  tables11 <- dir("data/census/input/2011/tables_rbind/", full.names = TRUE)
  files_11 <- llply(tables11, read_cen_csv, .progress = "text")
  tracts_profile_2011 <- join_all(files_11)
  write.csv(tracts_profile_2011, "data/census/output/tracts_profile_2011.csv", row.names=FALSE)
  
  #Run the read_cen_csv function for 2012, join all files, and output a tract profile for 2010 with all selected and renamed variables from each table
  tables12 <- dir("data/census/input/2012/tables_rbind/", full.names = TRUE)
  files_12 <- llply(tables12, read_cen_csv, .progress = "text")
  tracts_profile_2012 <- join_all(files_12)
  write.csv(tracts_profile_2012, "data/census/output/tracts_profile_2012.csv", row.names=FALSE)


  
#### data for 2013-2014
  #names of original selected variables for DP02 for 2013-2014
  DP02_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC130", "HC01_VC136", "HC03_VC136")
  #new names for selected variables, in order
  DP02_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "tot_pop", "tot_pop_foreign", "pct_pop_foreign")
  
  #names of original selected variables for DP03 for 2013-2014
  DP03_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC28", "HC03_VC29", "HC03_VC30", "HC01_VC05", "HC01_VC06", "HC01_VC85", "HC03_VC161", "HC03_VC171")
  #new names for selected variables, in order
  DP03_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "pct_drove_alone", "pct_carpooled", "pct_transit", "tot_labor", "tot_emp", "med_hh_inc", "pct_fam_pov", "pct_pers_pov")
 
  #names of original selected variables for DP04 for 2013-2014
  DP04_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC84", "HC03_VC84")
  #new names for selected variables, in order
  DP04_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "hh_no_vehicle", "hh_pct_no_vehicle")
  
  #names of original selected variables for DP05 for 2013-2014
  DP05_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC49", "HC03_VC50", "HC03_VC56", "HC03_VC88")
  #new names for selected variables, in order
  DP05_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "pct_white", "pct_black", "pct_asian", "pct_hisp")
  
  #Create the table_names and table_keys variables, assigning the variables 2013-2014
  table_names <- list("DP02"=DP02_names, "DP03"=DP03_names, "DP04"=DP04_names, "DP05"=DP05_names)
  table_keys <- list("DP02"=DP02_vars, "DP03"=DP03_vars, "DP04"=DP04_vars, "DP05"=DP05_vars)
  tables <- c()
  
  tables13 <- dir("data/census/input/2013/tables_rbind/", full.names = TRUE)
  files_13 <- llply(tables13, read_cen_csv, .progress = "text")
  tracts_profile_2013 <- join_all(files_13)
  write.csv(tracts_profile_2013, "data/census/output/tracts_profile_2013.csv", row.names=FALSE)
  
  tables14 <- dir("data/census/input/2014/tables_rbind/", full.names = TRUE)
  files_14 <- llply(tables14, read_cen_csv, .progress = "text")
  tracts_profile_2014 <- join_all(files_14)
  write.csv(tracts_profile_2014, "data/census/output/tracts_profile_2014.csv", row.names=FALSE)


  
#### data for 2015
  #names of original selected variables for DP02 for 2015
  DP02_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC130", "HC01_VC136", "HC03_VC136")
  #new names for selected variables, in order
  DP02_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "tot_pop", "tot_pop_foreign", "pct_pop_foreign")
  
  #names of original selected variables for DP03 for 2015
  DP03_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC28", "HC03_VC29", "HC03_VC30", "HC01_VC05", "HC01_VC06", "HC01_VC85", "HC03_VC161", "HC03_VC171")
  #new names for selected variables, in order
  DP03_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "pct_drove_alone", "pct_carpooled", "pct_transit", "tot_labor", "tot_emp", "med_hh_inc", "pct_fam_pov", "pct_pers_pov")
  
  #names of original selected variables for DP04 for 2015
  DP04_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC85", "HC03_VC85")
  #new names for selected variables, in order
  DP04_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "hh_no_vehicle", "hh_pct_no_vehicle")
  
  #names of original selected variables for DP05 for 2015
  DP05_vars <- c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC49", "HC03_VC50", "HC03_VC56", "HC03_VC88")
  #new names for selected variables, in order
  DP05_names <- c("GEO.id", "GEO.id2", "GEO.display.label", "pct_white", "pct_black", "pct_asian", "pct_hisp")
  
  #Create the table_names and table_keys variables, assigning the 2015 variables
  table_names <- list("DP02"=DP02_names, "DP03"=DP03_names, "DP04"=DP04_names, "DP05"=DP05_names)
  table_keys <- list("DP02"=DP02_vars, "DP03"=DP03_vars, "DP04"=DP04_vars, "DP05"=DP05_vars)
  tables <- c()
  
  #Run the read_cen_csv function for 2015, join all files, and output a tract profile for 2010 with all selected and renamed variables from each table
  tables15 <- dir("data/census/input/2015/tables_rbind/", full.names = TRUE)
  files_15 <- llply(tables15, read_cen_csv, .progress = "text")
  tracts_profile_2015 <- join_all(files_15)
  write.csv(tracts_profile_2015, "data/census/output/tracts_profile_2015.csv", row.names=FALSE)

