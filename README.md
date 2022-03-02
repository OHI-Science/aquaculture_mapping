# Mapping the spatial distribution of global mariculture production

Written by: Gage Clawson

## Code

Repository for code and generated data for "Mapping the spatial distribution of global mariculture production", Clawson *et al* 2022. https://doi.org/10.1016/j.aquaculture.2022.738066

External data are freely available from a number of different sources, all listed in the code. 

To replicate the analysis, download necessary data and set up an external directory for these datasets and large files generated during the analysis process.

### Prepping production data

To start, you should complete the scripts in the **marine/STEP1_species_groups** folder. 
 - This folder prepares the FAO aquaculture data to be split into our 6 different species groups; general marine finfish, bivalves, salmonids, tuna, other crustaceans, and shrimp/prawns. 
 - There are 2 scripts in this folder:
   - STEP1_species_groups.Rmd: This script classifies the FAO data into our species groups
   - STEP2_ASC_data_prep.Rmd: This script preps the aquaculture stewardship csv data and saves it as a .shp to use for the different species groups.

### Compiling aquaculture locations and information

Now head back to the **aquaculture_mapping/marine/** folder. Within each species group folder, there is a species_farms folder. The farms folder has a series of scripts which places aquaculture farms for each species group. **STEP1_** preps the FAO mariculture production data to the country/species level. **STEP2_** downloads and saves all mariculture locations as x,y coordinates, and saves other relevant information for modeling. **STEP3_** estimates the remaining number of farms that need to be placed within each country and species grouping combination.

I completed the species folders in the following order: 

 - marine/
    - china_data.Rmd: You need to complete this script first, because each of the species folders will use a subset of this data. This script takes Chinese remote sensed aquaculture data, and splits the data into our species categories. The data doesn't specify between species types, so we had to allocate number of farms based production percentages within China.

 - marine/salmon/
   - salmon_farms/
      - STEP1_fao_salmon_production.Rmd
      - STEP2_salmon_farm_locations.Rmd
      - STEP3_gapfill_salmon.Rmd
 
 - marine/shrimp/ 
   - shrimp_farms/
      - STEP1_fao_shrimp_production.Rmd
      - STEP2_shrimp_farm_locations.Rmd
      - STEP3_gapfill_shrimp.Rmd

 - marine/bivalve/
   - bivalve_farms/
      - STEP1_fao_bivalve_production.Rmd
      - STEP2_bivalve_farm_locations.Rmd
      - STEP3_gapfill_bivalve.Rmd

 - marine/marine_fish_general/
   - finfish_farms/
      - STEP1_fao_finfish_production.Rmd
      - STEP2_finfish_farm_locations.Rmd
      - STEP3_gapfill_finfish.Rmd

 - marine/tuna/
   - tuna_farms/
      - STEP1_fao_tuna_production.Rmd
      - STEP2_tuna_farm_locations.Rmd
      - STEP3_gapfill_tuna.Rmd

 - marine/crustaceans/
   - crustaceans_farms/
      - STEP1_fao_crustacean_production.Rmd
      - STEP2_crustacean_farm_locations.Rmd
      - STEP3_gapfill_crustacean.Rmd
   
In each of these folders there are 3 scripts. Once the three scripts in each folder are complete, move back a folder, into **aquaculture_mapping/marine** and complete **compile_data_for_placement.Rmd**. 

 - **marine/compile_data_for_placement.Rmd**

 - This script compiles all of the known farm locations (taken from steps 1-3 in each farm folder) into a file, and compiles all of the total farms needed to be placed (and gapfilled) into a file. These files will be plugged into our suitability layer and place farms on an overall map.
 
### Calculate average distance to port
 - **workflow/calc_dist_to_port.Rmd**
 - This script calculates the average distance to port which will be used to place farms.


### Place farms with the suitability layer

 - **_spatial/create_world_maps.R** 
  - Creates some general spatial masks that are used in the placement of farms. 

 - **workflow/place_unknown_farms.Rmd**

 - Places farms onto our suitability layer.
  - This scripts place the gapfilled farm estimate numbers onto suitable locations within each country and species group. 
  
 - **workflow/save_suitability_layer.R**
  - Saves the suitability layer in total to use for our validation analysis. 
  
 - **analysis/match_data_sources.Rmd**
     - This script matches our data sources and modeled farms to a final farms dataset with modeled farms, known farms, all data types and all data sources. 
  
### Allocate and rasterize FAO reported production

Once that is completed, you may move into the **marine/last_step_rasterize** folder, and run each of the species groups files. 

  - These files rasterize the production of each species group, which will could be used for any other analysis that might need FAO production distributed across the mariculture map. 
  
**Analysis and data check**

Once all of those are completed, you can find the **analysis** folder, which data checks, conducts some summarization numbers for the final paper, creates figures, and completes the accuracy assessment of our suitability layer. 

Good luck! 


#### General Roadmap: 

1. Jump into **aquaculture_mapping/marine** folder and complete the files in **STEP1_species_group**
2. Within each species folder, complete steps 1 - 3 in the farms folder. 
3. Jump back a folder and complete **compile_data_for_placement.Rmd**
4. Complete files in **aquaculture_mapping/workflow** to place farms that need placement. 
5. Complete the files in **aquaculture_mapping/marine/last_step_rasterize/**
6. Complete summary stats and figures in **analysis** folder. 


