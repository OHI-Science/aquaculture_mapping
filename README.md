# Mapping marine aquaculture


Written by: Gage Clawson

## Code

Here is an overview of the organization of files and data:

This series of folders/scripts preps mariculture mapping layers. 

### Prepping production data

To start, you should complete the scripts in the **marine/STEP1_species_groups** folder. 
 - This folder prepares the FAO aquaculture data to be split into our 6 different species groups; general marine finfish, bivalves, salmon, tuna, crustaceans, and shrimp. 
 - There are 2 scripts in this folder:
   - STEP1_species_groups.Rmd: This script classifies the FAO data into our species groups
   - STEP2_ASC_data_prep.Rmd: This script preps the aquaculture stewardship csv data and saves it as a .shp to use for the different species groups.

### Compiling aquaculture locations and information

Within each species group folder, there is a species_farms folder. The farms folder has a series of scripts which places aquaculture farms for each species group.

I completed the species folders in the following order: 

 - marine/salmon/
   - salmon_farm/
      - STEP1_fao_salmon_production.Rmd
      - STEP2_salmon_farm_locations.Rmd
      - STEP3_gapfill_salmon.Rmd
 
 - marine/shrimp/ 
   - shrimp_farm/
      - STEP1_fao_shrimp_production.Rmd
      - STEP2_shrimp_farm_locations.Rmd
      - STEP3_gapfill_shrimp.Rmd

 - marine/bivalve/
   - bivalve_farm/
      - STEP1_fao_bivalve_production.Rmd
      - STEP2_bivalve_farm_locations.Rmd
      - STEP3_gapfill_bivalve.Rmd

 - marine/marine_fish_general/
   - finfish_farm/
      - STEP1_fao_finfish_production.Rmd
      - STEP2_finfish_farm_locations.Rmd
      - STEP3_gapfill_finfish.Rmd

 - marine/tuna/
   - tuna_farm/
      - STEP1_fao_tuna_production.Rmd
      - STEP2_tuna_farm_locations.Rmd
      - STEP3_gapfill_tuna.Rmd

 - marine/crustaceans/
   - crustaceans_farm/
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

 - **workflow/place_unknown_farms.Rmd**

 - Places farms onto our suitability layer.
  - This scripts place the gapfilled farm estimate numbers onto suitable locations within each country and species group. 
  
  
### Create a validation dataset by placing known farms onto the suitability layer

 - **workflow/coast_buffer.R**
     - creates a buffered coast to conduct validation analysis on. 
 - **workflow/place_validation_farms.R**

 - Places known locations of farms onto our suitability layer to create a validation dataset.
 
  
### Allocate and rasterize FAO reported production

Once that is completed, you may move into the **marine/last_step_rasterize** folder, and run each of the species groups files. 

  - These files rasterize the production of each species group, which will later be plugged into the food systems analysis (or any other analysis that might need FAO production distributed across the mariculture map). 
  
**Analysis and data check**

Once all of those are completed, you can find the **analysis** folder, which data checks, conducts some summarization numbers for the final paper, and creates figures. 

Good luck! 


#### General Roadmap: 

1. Jump into **aquaculture_mapping/marine** folder and complete the files in **STEP1_species_group**
2. Within each species folder, complete steps 1 - 3 in the farms folder. 
3. Jump back a folder and complete **compile_data_for_placement.Rmd**
4. Complete files in **aquaculture_mapping/workflow** to place farms that need placement. 
5. Complete the files in **aquaculture_mapping/marine/last_step_rasterize/**
6. Complete summary stats and figures in **analysis** folder. 


