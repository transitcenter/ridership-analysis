# Compile Census Data

This repository contains the necessary scripts to process and join the census data to a shapefile of tracts that correlate with desired Metropolitan Statistical Areas (MSAs).

### Getting Started

Before you can start working through the scripts you will need to download the DP02, DP03, DP04, and DP05 tables from American Fact Finder for every census tract in the United States for the 2010-2015 ACS 5-Year estimates.

You will likely have 5 or more files for each table for each year because of download limitations. This will result in many files that look something like this `ACS_10_5YR_DP02_with_ann`, `ACS_10_5YR_DP02_with_ann_1`, `ACS_10_5YR_DP02_with_ann_2`, `ACS_10_5YR_DP02_with_ann_3`, etc.

These files should be saved in the `data\census\input` folder in a file structure that is by year and then by table, such as `data\census\input\2010\DP02\`. A folder called `tables_rbind` also must be created inside of every year folder, such as `data\census\input\2010\tables_rbind\`.

You will also need to download the census tracts for all of the MSAs in your list. This script uses a shapefile with the all of the census tracts for the top 55 MSA's by population as of 2017. The shapefile you download should be titled `tracts_in_selected_msas` and saved in an input folder in this location `data\spatial\input\`. Also create an output folder inside of the spatial folder, such as `data\spatial\output\`.

Work through the census data scripts in the following order:

1. `TransitCenter_combine_tract_data_2010-2015`
2. `TransitCenter_calculate_census_variables`

# Compile NTD Data

This repository contains the necessary code and datasets to compile NTD and census datasets used in Azavea's visualization of transportation ridership for TransitCenter

### Getting started

The R scripts in this repository create the datasets used in the app. You can work through the scripts in the following order:

1. `TransitCenter_match_agencies_to_msas.R`
2. `TransitCenter_data_wrangling_TS21.R`
3. `TransitCenter_create_shapefile_of_change_variables.R`

All scripts rely on input files in the `data/input/` directory and write final outputs to the `data/output/` directory. Intermediate outputs (e.g. csvs that one script outputs but another uses as input) are written to `data/input/`.


# List of 55 Metropolitan Statistical Areas (MSAs)

1. Atlanta-Sandy Springs-Roswell, GA
2. Austin-Round Rock, TX
3. Baltimore-Columbia-Towson, MD
4. Birmingham-Hoover, AL
5. Boston-Cambridge-Newton, MA-NH
6. Buffalo-Cheektowaga-Niagara Falls, NY
7. Charlotte-Concord-Gastonia, NC-SC
8. Chicago-Naperville-Elgin, IL-IN-WI
9. Cincinnati, OH-KY-IN
10. Cleveland-Elyria, OH
11. Columbus, OH
12. Dallas-Fort Worth-Arlington, TX
13. Denver-Aurora-Lakewood, CO
14. Detroit-Warren-Dearborn, MI
15. Grand Rapids-Wyoming, MI
16. Hartford-West Hartford-East Hartford, CT
17. Houston-The Woodlands-Sugar Land, TX
18. Indianapolis-Carmel-Anderson, IN
19. Jacksonville, FL
20. Kansas City, MO-KS
21. Las Vegas-Henderson-Paradise, NV
22. Los Angeles-Long Beach-Anaheim, CA
23. Louisville/Jefferson County, KY-IN
24. Memphis, TN-MS-AR
25. Miami-Fort Lauderdale-West Palm Beach, FL
26. Milwaukee-Waukesha-West Allis, WI
27. Minneapolis-St. Paul-Bloomington, MN-WI
28. Nashville-Davidson–Murfreesboro–Franklin, TN
29. New Orleans-Metairie, LA
30. New York-Newark-Bridgeport, NY-NJ-PA
31. Oklahoma City, OK
32. Orlando-Kissimmee-Sanford, FL
33. Philadelphia-Camden-Wilmington, PA-NJ-DE-MD
34. Phoenix-Mesa-Scottsdale, AZ
35. Pittsburgh, PA
36. Portland-Vancouver-Hillsboro, OR-WA
37. Providence-Warwick, RI-MA
38. Raleigh, NC
39. Richmond, VA
40. Riverside-San Bernardino-Ontario, CA
41. Rochester, NY
42. Sacramento–Roseville–Arden-Arcade, CA
43. Salt Lake City, UT
44. San Antonio-New Braunfels, TX
45. San Diego-Carlsbad, CA
46. San Francisco-Oakland-Hayward, CA
47. San Jose-Sunnyvale-Santa Clara, CA
48. Seattle-Tacoma-Bellevue, WA
49. St. Louis, MO-IL
50. Tampa-St. Petersburg-Clearwater, FL
51. Tucson, AZ
52. Tulsa, OK
53. Urban Honolulu, HI
54. Virginia Beach-Norfolk-Newport News, VA-NC
55. Washington-Arlington-Alexandria, DC-VA-MD-WV
