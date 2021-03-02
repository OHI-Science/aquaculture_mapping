#------------------------------------------------------------------------------#
## Project :  Aquaculture Mapping
## Purpose :  Generate
## Date    :  02/11/2021
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#


#### load libraries and data ####
#------------------------------------------------------------------------------#
library(sf)
library(tidyverse)
library(raster)
library(rgeos)

# load in crs, coastlines, eez, masks, country shapes, ports, depth, and rasters
source("./_spatial/template_raster.R")
gadm_diss <- st_read("/home/shares/food-systems/Global_datasets/World_map/World_ISO_dissolve.shp")
gadm_full <- st_read("/home/shares/food-systems/Global_datasets/World_map/gadm36.shp")
land_mask <- raster("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/land_mask.tif")
eez_mask <- raster("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/eez_mask.tif")
eez_sf <- st_read("/home/shares/food-systems/Global_datasets/EEZ/eez_v10.shp")
coastline <- readRDS("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/outline.rds")

the_crs <- crs(fake_raster, asText = TRUE)
# depth and ports
ports_raw <- st_read("/home/shares/clean-seafood/raw_data/ports_data/Commercial_ports.shp") %>% 
  st_transform(the_crs)
bathy_raw <- raster("/home/shares/clean-seafood/raw_data/bathymetry_data/full_bathy_layer.tif") %>%
  resample(fake_raster, method = 'bilinear')
  #projectRaster(to = fake_raster,
                #res = res(fake_raster))

bathy_raw2 <- bathy_raw2 %>% 
  projectRaster(to = fake_raster)

bathy_raw2 <- bathy_raw
bathy_raw <- bathy_raw2
bathy_raw[bathy_raw >    1] <- NA # replace all values over 1m asl as NA
bathy_raw[bathy_raw < -250] <- NA # replace all values under 250 bsl as NA
bathy_raw[!is.na(bathy_raw)] <- 1 # replace all suitable depths with 1


# load in each of the aquaculture maps and convert to sf objects
all_farms_raw   <-
  bind_rows(read_csv("marine/bivalve/bivalve_farms/data/global_bivalve_farm_lat_lon_data_quality.csv") %>% 
              mutate(type = "bivalve") %>% 
              select(-X1),
            read_csv("marine/crustaceans/crustacean_farms/data/global_crustacean_farm_lat_lon_data_quality.csv") %>% 
              mutate(type = "crustacean") %>% 
              rename(subregion = sub_rgn),
            read_csv("marine/salmon/salmon_farm/data/global_salmon_farm_lat_lon_quality.csv") %>% 
              mutate(type = "salmon"),
            read_csv("marine/shrimp/shrimp_farms/data/global_shrimp_farm_lat_lon_data_quality.csv") %>% 
              mutate(type = "shrimp"),
            read_csv("marine/tuna/tuna_farms/data/global_tuna_farm_lat_lon_data_quality.csv") %>% 
              mutate(type = "tuna"),
            read_csv("marine/marine_fish_general/finfish_farms/data/global_finfish_farm_lat_lon_data_quality.csv") %>% 
              mutate(type = "marine_fish_general")) %>%
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = crs(food_raster), 
           agr = "constant") 


####  Allocate spatially ####
#------------------------------------------------------------------------------#
# First we will allocate the farms for countries where we have the number of 
# farms, but not the location. For some areas we have the number of farms per
# subregion so we do those separately as well

## do it for national 

need_national_allocation <- all_farms_raw %>% 
  filter(action_needed == "Allocate spatially", 
         is.na(subregion))

# generate prediction points for each national allocation area
for (i in 1:unique(need_national_allocation$iso3c)) {
  
  this_iso3 <- unique(need_national_allocation$iso3c)[3]
  
  this_country <- gadm_full %>% 
    filter(GID_0 == this_iso3) %>% 
    group_by(GID_0) %>% 
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%
    ungroup()
  #select(GID_0) %>% 
  
  
  this_eez <- eez_sf %>% 
    filter(ISO_Ter1 == this_iso3) %>% 
    group_by(ISO_Ter1)%>% 
    st_union() %>% 
    sf::st_as_sf() %>% 
    ungroup() 
  
  this_area <- this_country %>% 
    bind_rows(this_eez)
  
  this_bbox <- st_bbox(this_eez)
  this_coast <- st_crop(coastline, this_bbox)
  these_ports <- st_crop(ports_raw, this_bbox)
  
  this_raster_area <- crop(fake_raster, this_eez)
  this_eez_mask <- mask(fake_raster, this_eez) %>% 
    crop(this_eez)
  
  
  this_raster_points <- as(this_eez_mask, "SpatialPoints")
  this_coast_sp <- as(this_coast, "Spatial")
  this_port_sp <- as(these_ports, "Spatial")
  
  # calculate distance to the coast!
  dis_to_coast_rast <- this_eez_mask
  this_dist <- gDistance(this_raster_points, this_coast_sp, byid = TRUE)
  this_dmin = apply(this_dist,2,min)
  dis_to_coast_rast[!is.na(dis_to_coast_rast[])]=this_dmin
  
  ps_1 <- dis_to_coast_rast
  ps_1[ps_1 < .1] <- NA
  ps_1[ps_1 > 3] <- NA
  plot(ps_1)
  
  xbox <- dis_to_port_rast
  xbox[xbox > 2] <- NA
  plot(xbox)
  
  nice <- xbox + ps_1 + this_bathy_mask
  mapview::mapview(this_bathy_mask)
  plot(nice, col = c("red", "green"))
  
  # calculate distance to ports!
  dis_to_port_rast <- this_eez_mask
  this_dist_port <- gDistance(this_raster_points, this_port_sp, byid = TRUE)
  this_dmin_port = apply(this_dist_port,2,min)
  dis_to_port_rast[!is.na(dis_to_port_rast[])]=this_dmin_port
  
  # get depth data!
  this_bathy_mask <- crop(bathy_raw2, this_bbox) #%>% 
    mask(this_eez_mask)
  
}

# generate prediction points for each subnational allocation area
# 


tibble(str_col = c("apple", "banana", "pear", "pinapple")) %>% 
  filter(str_col <= "apple")

