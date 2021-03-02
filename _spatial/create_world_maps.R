#------------------------------------------------------------------------------#
## Project :  Aquaculture Mapping
## Purpose :  Generate coastlines and ocean rasters
## Date    :  02/10/2021
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#

# Outputs:
#
#


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


#### distance to port raster ####
#------------------------------------------------------------------------------#
ports_points <- st_read("/home/shares/clean-seafood/raw_data/ports_data/Commercial_ports.shp") %>% 
  as("Spatial")

ports_dist_raster <- distanceFromPoints(fake_raster, ports_points)
writeRaster(ports_dist_raster, "/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/dist_to_ports_raster.tiff",
            overwrite=TRUE)



grid = sf::st_as_sf(fake_raster, as_points = TRUE)
grid$NDVI = NULL
grid


pol_sp = as(fake_raster, "SpatialPolygonsDataFrame")
pol = st_as_sf(pol_sp)
dist <- st_distance(pol, coastline)
saveRDS(dist, "distance_matrix.rds")
#### dissolve eez ####
#------------------------------------------------------------------------------#
gadm_full <- st_read("/home/shares/food-systems/Global_datasets/World_map/gadm36.shp")

all <- gadm_full %>% 
  filter(GID_0 == "CHN") %>% 
  group_by(GID_0) %>% 
  st_union() %>% # union all touching polygons
  sf::st_as_sf() #%>%
  #select(GID_0) %>% 
  

chin <- eez_sf %>% 
  filter(ISO_Ter1 == "CHN") %>% 
  group_by(ISO_Ter1)%>% 
  st_union() %>% 
  sf::st_as_sf() %>% 
  ungroup() %>% 
  bind_rows(all) 



chin_mask <- chin %>% 
  fasterize::fasterize(food_raster)

bathy_prep <- bathy_raw %>% 
  projectRaster(to = food_raster)

x <- chin_mask*bathy_prep
plot(x)

test <- chin %>% 
  ungroup() %>% 
  st_union() %>% 
  sf::st_as_sf()



Otherway <- chin_mask %>% 
  projectRaster(to = bathy_raw)

y <- Otherway * bathy_raw
plot(y)
