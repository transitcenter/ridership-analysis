###########################################################################
## 
## Script purpose: calculate census chnage variable and join to census tract shapes, create long and wide csv's.
##    
## Note: 
## Before beginning this script, download all of the census tract shapefiles for the selected MSAs.
## The script "combine_tract_data_2010-2015 should be run first as the outputs are needed for this script.
##
###########################################################################

library(tidyverse)
library(plyr)
library(sf)
library(sp)
library(stringr)


# Read in Tracts and Create process_csv function --------------------------

  #Read in the shapefile of census tracts for selected MSAs
  tracts <- st_read("spatial/input/tracts_in_selected_msas.shp")
  
  #Function to process each tract proflie csv
  process_csv <- function(f, change_names = FALSE) {
    
    #create a year variable
    year <- substr(f, nchar(f) - 7, nchar(f) - 4) 
   
    #convert geo.id fields to characters and assign year as a column
    df <- read.csv(f, stringsAsFactors = FALSE) %>%
      mutate(GEO.id = as.character(GEO.id),
             GEO.id2 = as.character(GEO.id2),
             year = year)
    
          #df$pct_emp <- ((df$tot_emp / df$tot_labor) * 100)
          #df$pct_unemp <- (((df$tot_labor - df$tot_emp) / df$tot_labor) * 100)
    
    #add leading zeros back to geo.id where they got dropped in character conversion
    df$GEO.id2 <- str_pad(df$GEO.id2, 11, 'left', "0")
    
    #filter census data based on tracts that are in the selected tracts shapefile
    df <- df %>%
      filter(GEO.id2 %in% tracts$GEOID_t)
    df1 <- df %>%
      select(GEO.id, GEO.id2, GEO.display.label)
    df2 <- df %>%
      select(-GEO.id, -GEO.id2, -GEO.display.label) %>%
      mutate_all(as.numeric)
    if (change_names) {
      names(df2) <- paste0(names(df2), ".", year)
    }
    df <- cbind(df1, df2)
    return(df)
  }

# Read in an Process Tract Profile files ----------------------------------

  #create list of file names for all of the tract profiles CSVs
  files <- dir("census/output", full.names = TRUE)
  
  #Run the process_csv function and create a long file with each census variable and a field for year
  dfs <- ldply(files, process_csv, .progress = 'text')
  
  #Run the process_csv function and create a list with each year represented in wide form
  df_list <- llply(files, process_csv, change_names = TRUE, .progress = 'text')
  
  
  #find the commmon geo.id2's
  d10 <- df_list[[1]]
  d15 <- df_list[[length(df_list)]]
  common <- intersect(d10$GEO.id2, d15$GEO.id2)
  
  #make every data frame the same length by filtering for only common geo.id2's
  df_list_common <- lapply(df_list, function(x) x %>% filter(GEO.id2 %in% common))
  
  #create variable for each csv
  df_2010 <- df_list_common[[1]]
  df_2011 <- df_list_common[[2]]
  df_2012 <- df_list_common[[3]]
  df_2013 <- df_list_common[[4]]
  df_2014 <- df_list_common[[5]]
  df_2015 <- df_list_common[[6]]
  
  #merge all datasets together to create wide data
  df_merge <- merge(df_2010, df_2011)
  df_merge <- merge(df_merge, df_2012)
  df_merge <- merge(df_merge, df_2013)
  df_merge <- merge(df_merge, df_2014)
  df_merge <- merge(df_merge, df_2015)

# Calculate Change Variables (except pop density) -------------------------

  #create new variables for each change variable except for pop_density
  df_merge$forgn_c <- (df_merge$pct_pop_foreign.2015 - df_merge$pct_pop_foreign.2010)
  df_merge$drove_c <- (df_merge$pct_drove_alone.2015 - df_merge$pct_drove_alone.2010)
  df_merge$carpool_c <- (df_merge$pct_carpooled.2015 - df_merge$pct_carpooled.2010)
  df_merge$transit_c <- (df_merge$pct_transit.2015 - df_merge$pct_transit.2010)
  df_merge$emp_c <- (df_merge$tot_emp.2015 - df_merge$tot_emp.2010)
  df_merge$inc_c <- (df_merge$med_hh_inc.2015 - df_merge$med_hh_inc.2010)
  df_merge$fpov_c <- (df_merge$pct_fam_pov.2015 - df_merge$pct_fam_pov.2010)
  df_merge$ppov_c <- (df_merge$pct_pers_pov.2015 - df_merge$pct_pers_pov.2010)
  df_merge$veh_c <- (df_merge$hh_pct_no_vehicle.2015 - df_merge$hh_pct_no_vehicle.2010)
  df_merge$white_c <- (df_merge$pct_white.2015 - df_merge$pct_white.2010)
  df_merge$black_c <- (df_merge$pct_black.2015 - df_merge$pct_black.2010)
  df_merge$asian_c <- (df_merge$pct_asian.2015 - df_merge$pct_asian.2010)
  df_merge$hisp_c <-  (df_merge$pct_hisp.2015 - df_merge$pct_hisp.2010) 


# Calculate Area of Tracts ------------------------------------------------

  tracts <- tracts %>%
    mutate(GEO.id2 = GEOID_t)

  tracts <- tracts %>%
    mutate(GEO.id3 = GEO.id2)
  
  #reproject data
  tracts_reproj <- tracts %>% st_transform(102003)
  
  #caluclate area, default is meteres
  tracts_reproj$area_m <- st_area(tracts_reproj)
  
  #convert meters to square miles
  tracts_reproj$area_miles <- (tracts_reproj$area_m / 2590000)
  
  tracts_reproj <- tracts_reproj %>% mutate(area_miles = as.numeric(area_miles))
  

# Remove tracts with no population ----------------------------------------

  tracts_reproj_census <- tracts_reproj %>%
    right_join(df_merge)
  
  
  #remove tracts with zero pop in 2010 and 2015
  tracts_reproj_census <- tracts_reproj_census %>% filter(!(tot_pop.2015 == 0 & tot_pop.2010 == 0))
  

# Create Long Data for Charts w/ Pop Density ------------------------------

  #pull out the data for area_miles and the geo.id2 to join
  t <- tracts_reproj %>% select(area_miles, GEO.id2) %>% as.data.frame()

  
  #remove tracts with zero population by filtering on tracts_reproj_census
  t <- t %>% filter(GEO.id2 %in% tracts_reproj_census$GEO.id2)
  
  #remove the geometry  
  t <- t %>% select(-geometry)
  
  #join long table of all variables, dfs, with area_miles
  dfs_join <- left_join(dfs, t)
  
  #calculate population density
  dfs_join <- dfs_join %>% mutate(pop_density = tot_pop/area_miles)
  
  #write long variables to csv
  write.csv(dfs_join, "census/output/census_vars_long.csv", row.names=FALSE)


# Create wide file, select variables for shapefile export, save files --------
  
  #calculate population density
  tracts_reproj_census <- tracts_reproj_census %>% 
                          mutate(pop_density.2010 = tot_pop.2010/area_miles) %>%
                          mutate(pop_density.2015 = tot_pop.2015/area_miles) %>%
                          mutate(pp_dn_c = pop_density.2015 - pop_density.2010)
  
  
  #create variable of all census vars and change vars with tract info to save as csv
  census_change_vars_wide <- as.data.frame(tracts_reproj_census) %>% select(-geometry)
 
  #Final selection of variables to output with the tracts - selecitng all of the raw 2010 and 2015 variables and the change variables.
  tracts_w_census_change <- tracts_reproj_census %>% select(STATEFP, COUNTYF, TRACTCE, GEOID_t, NAME_tr, GEOID_m, NAME_ms, GEO.id2, area_miles, GEO.id, 
                                                 GEO.display.label, pct_pop_foreign.2010, pct_drove_alone.2010, pct_carpooled.2010, pct_transit.2010,
                                                 tot_emp.2010, med_hh_inc.2010, pct_fam_pov.2010, pct_pers_pov.2010, hh_pct_no_vehicle.2010, pct_white.2010,
                                                 pct_black.2010, pct_asian.2010, pct_hisp.2010, pop_density.2010, pct_pop_foreign.2015, pct_drove_alone.2015, pct_carpooled.2015, pct_transit.2015,
                                                 tot_emp.2015, med_hh_inc.2015, pct_fam_pov.2015, pct_pers_pov.2015, hh_pct_no_vehicle.2015, pct_white.2015,
                                                 pct_black.2015, pct_asian.2015, pct_hisp.2015, pop_density.2015, forgn_c, drove_c, carpool_c, transit_c, emp_c, inc_c, fpov_c, ppov_c, veh_c, 
                                                 white_c, black_c, asian_c, hisp_c, pp_dn_c, geometry)
  
  #Renaming the variables before saving
  tracts_w_census_change <- tracts_w_census_change %>% 
                  dplyr::rename(frgn_y10 = pct_pop_foreign.2010) %>%
                  dplyr::rename(drove_y10 = pct_drove_alone.2010) %>%
                  dplyr::rename(carpool_y10 = pct_carpooled.2010) %>%
                  dplyr::rename(transit_y10 = pct_transit.2010) %>%
                  dplyr::rename(emp_y10 = tot_emp.2010) %>%
                  dplyr::rename(inc_y10 = med_hh_inc.2010) %>%
                  dplyr::rename(fpov_y10 = pct_fam_pov.2010) %>%
                  dplyr::rename(ppov_y10 = pct_pers_pov.2010) %>%
                  dplyr::rename(veh_y10 = hh_pct_no_vehicle.2010) %>%
                  dplyr::rename(white_y10 = pct_white.2010) %>%
                  dplyr::rename(black_y10 = pct_black.2010) %>%
                  dplyr::rename(asian_y10 = pct_asian.2010) %>%
                  dplyr::rename(hisp_y10 = pct_hisp.2010) %>%
                  dplyr::rename(pp_dns_y10 = pop_density.2010) %>%
                  dplyr::rename(frgn_y15 = pct_pop_foreign.2015) %>%
                  dplyr::rename(drove_y15 = pct_drove_alone.2015) %>%
                  dplyr::rename(carpool_y15 = pct_carpooled.2015) %>%
                  dplyr::rename(transit_y15 = pct_transit.2015) %>%
                  dplyr::rename(emp_y15 = tot_emp.2015) %>%
                  dplyr::rename(inc_y15 = med_hh_inc.2015) %>%
                  dplyr::rename(fpov_y15 = pct_fam_pov.2015) %>%
                  dplyr::rename(ppov_y15 = pct_pers_pov.2015) %>%
                  dplyr::rename(veh_y15 = hh_pct_no_vehicle.2015) %>%
                  dplyr::rename(white_y15 = pct_white.2015) %>%
                  dplyr::rename(black_y15 = pct_black.2015) %>%
                  dplyr::rename(asian_y15 = pct_asian.2015) %>%
                  dplyr::rename(hisp_y15 = pct_hisp.2015) %>%
                  dplyr::rename(pp_dns_y15 = pop_density.2015) 
  
  #write out the shapefile with all of the required census variables
  #note that this will rename some of the variables because of the requirements of shapefiles.
  #renaming the field names to be shorter, in the previous step, should help to alleviate some of this but there may be manual renaming required.
  st_write(tracts_w_census_change, "spatial/output/tracts_w_census_change.shp")

  #retain all variables as their own csv in case this is needed later (this will retain original field names)
  write.csv(census_change_vars_wide,"census/output/census_change_vars_wide.csv", row.names=FALSE)
  
