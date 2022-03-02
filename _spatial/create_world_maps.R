#------------------------------------------------------------------------------#
## Project :  Aquaculture Mapping
## Purpose :  Generate coastlines and ocean rasters
## Date    :  02/10/2021
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#


#### Create Coastline ####
#------------------------------------------------------------------------------#
library(tidyverse)
library(sf)
library(raster)
library(fasterize)

# read in dissolved gadm and cast to a coastline
gadm_diss <- st_read("/home/shares/food-systems/Global_datasets/World_map/World_ISO_dissolve.shp")
gadm_full <- st_read("/home/shares/food-systems/Global_datasets/World_map/gadm36.shp")


outline <- gadm_diss %>%
  st_union() %>% # union all touching polygons
  sf::st_as_sf() %>%
  st_cast("MULTILINESTRING") # Cast to a multilinestring

# save coastline as an .rds & .shp files 
write_rds(outline, "/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/outline.rds") 
st_write(outline,  "/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/outline.shp")


#### Create Land and Ocean Mask ####
#------------------------------------------------------------------------------#

# read in template raster
source("./_spatial/template_raster.R")

land_mask <- gadm_diss %>%
  fasterize::fasterize(food_raster) 
  
writeRaster(land_mask, "/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/land_mask.tiff")


#### Create EEZ mask ####
#------------------------------------------------------------------------------#

eez_sf <- st_read("/home/shares/food-systems/Global_datasets/EEZ/eez_v10.shp")

eez_mask <- eez_sf %>% 
  fasterize::fasterize(food_raster)


writeRaster(eez_mask, "/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/eez_mask.tiff",
            overwrite=TRUE)






  

