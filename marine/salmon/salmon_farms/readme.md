To recreate the maps the following scripts are used in this order:

1. STEP1_fao_salmon_production.Rmd : Clean FAO production data

2. STEP2_salmon_farm_locations.Rmd : cleans lat/long information for salmon farms (in some cases we have farms, in other cases we have no information on individual farms and we use generic points to save the data)

3. STEP3_gapfill_salmon_production.Rmd : this produces estimates for total farm numbers for each country that has incomplete farm information. It also produces estimates for the average tonnage per known farm, which is used to estiamte the number of farms needed to be placed. After you finish this script, move on to other species groups. 


Folder descriptions:

 - /data
    - This folder contains data that is created from steps 1 and 2. In particular, it houses prepped salmonid production data, known salmonid farm locations, and identifiers for data-limited salmonid producing countries. 

 - /output
    - salmon_comp_tonnes_per_farm.csv; This file is created in step 2 and describes the tonnes per farm estimates for comprehensively reporting countries. 
    - salmon_points_to_add_final.csv; This file is created in step 2 and describes the number of farm locations to estimate for each data-limited salmonid producing country. 
    - all_known_salmon_farms_final.csv; Pretty self explanatory... a dataset of all of the known salmonid farm locations. 