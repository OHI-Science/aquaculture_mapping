# Mapping marine aquaculture


Written by: Gage Clawson

### Code

Here is an overview of the organization of files and data:

This series of folders/scripts preps the aquaculture layers. 

**Prepping production data**

To start, you should complete "STEP1_species_groups" first, in the **aquaculture/marine** folder. 
 - This folder prepares the FAO aquaculture data to be split into our 6 different species groups; general marine finfish, bivalves, salmon, tuna, crustaceans, and shrimp. 
 - There are 2 scripts in this folder:
   - STEP1_species_groups.Rmd: This script classifies the FAO data into our species groups
   - STEP2_ASC_data_prep.Rmd: This script preps the aquaculture stewardship data and saves it as a .shp to use for the different species groups.

**Compiling aquaculture locations and information**

Within each folder, there is a species_farms folder. The farms folder has a series of scripts which places aquaculture farms for each species group.

I completed the species folders in the following order: 

 - salmon
   - salmon_farm
 
 - shrimp 
   - shrimp_farm

 - bivalve
   - bivalve_farm

 - marine_fish_general
   - finfish_farm

 - tuna
   - tuna_farm

 - crustaceans
   - crustacean_farm
   
In each of these folders there are 3 scripts. Once the three scripts in each folder are complete, move back a folder, into **aquaculture_mapping/marine** and complete **compile_data_for_placement.Rmd**. 

 - This script compiles all of the known farm locations (taken from steps 1-3 in each farm folder) into a file, and compiles all of the total farms needed to be placed (and gapfilled) into a file. These files will be plugged into our suitability layer and place farms on an overall map.

**Place farms with the suitability layer**

To place farms onto our suitability layer, the scripts within the folder **aquaculture_mapping/workflow** folder need to be completed
  - These scripts place the gapfilled farm estimate numbers onto suitable locations within each country and species group. 
  
**Allocate and rasterize FAO reported production**

Once that is completed, you may move into the **aquaculture_mapping/marine/last_step_rasterize** folder, and run each of the species groups files. 

  - These files rasterize the production of each species group, which will later be plugged into the food systems analysis (or any other analysis that might need FAO production distributed across the mariculture map). 
  
**Analysis and data check**

Once all of those are completed, you can find the **analysis** folder, which data check and conduct some summarization numbers for the final paper. 

Good luck! 


#### General Roadmap: 

1. Jump into **aquaculture_mapping/marine** folder and complete the files in **STEP1_species_group**
2. Within each species folder, complete steps 1 - 3 in the farms folder. 
3. Jump back a folder and complete **compile_data_for_placement.Rmd**
4. Complete files in **aquaculture_mapping/workflow** to place farms that need placement. 
5. Complete the files in **aquaculture_mapping/marine/last_step_rasterize/**


