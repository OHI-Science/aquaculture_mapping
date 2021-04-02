#### load libraries and data ####
#------------------------------------------------------------------------------#
library(sf)
library(spatialEco)
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

ports_raw <- st_read("/home/shares/clean-seafood/raw_data/ports_data/Commercial_ports.shp") %>% 
  st_transform(the_crs)



data <- read_csv("marine/output/all_marine_farms.csv") %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = crs(food_raster), 
           agr = "constant") 

need_national_allocation <- read_csv("marine/output/all_points_to_add.csv") %>% 
  group_by(iso3c, type) %>% 
  summarize(num_farms = sum(num_additional_farms))

all_the_farm_points <- data %>% 
  dplyr::select(iso3c, type) %>%
  .[0, ]


#4326
# use     #st_transform(crs = "+proj=moll") %>%  for buffering in meters

for (i in 1:length(unique(need_national_allocation$iso3c))) {

  this_iso3 <- unique(need_national_allocation$iso3c)[i]
  
  this_country <- gadm_full %>% 
    filter(GID_0 == this_iso3) %>% 
    group_by(GID_0) %>% 
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%
    ungroup()
  
  ## 1.b get eez
  this_eez <- eez_sf %>% 
    filter(ISO_Ter1 == this_iso3) %>% 
    group_by(ISO_Ter1)%>% 
    st_union() %>% 
    sf::st_as_sf() %>% 
    ungroup() 
  
  ## 1.c both country and eez
  this_area <- this_country %>% 
    bind_rows(this_eez)
  
  ## 1.d get bounding box
  this_bbox <- st_bbox(this_area)
  
  ## 1.e filter for ports and coast line of this country
  this_coast <- this_country %>%
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%
    st_cast("MULTILINESTRING") # Cast to a multilinestring
  
  #this_coast <- st_crop(coastline, this_bbox)
  these_ports <- st_crop(ports_raw, this_bbox)
  
  ## 1.f crop down the rasters
  this_raster_area <- crop(fake_raster, this_eez)
  this_eez_mask <- crop(fake_raster, this_eez) #%>% 
  
  this_raster_points <- as(this_eez_mask, "SpatialPoints")
  
  #mapview(this_raster_points)
  this_area_sp <- as(this_area, "Spatial")
  this_coast_sp <- as(this_coast, "Spatial")
  this_port_sp <- as(these_ports, "Spatial")
  
  ## 2.b calculate distance to the coast!
  dis_to_coast_rast <- this_eez_mask
  this_dist <- gDistance(this_raster_points, this_coast_sp, byid = TRUE)
  this_dmin = apply(this_dist,2,min)
  dis_to_coast_rast[!is.na(dis_to_coast_rast[])]=this_dmin
  
  coast_test_rast <-dis_to_coast_rast
  coast_test_rast[coast_test_rast > 1] <- NA # replace all values over 1m asl as NA
  
  
  ## 2.c calculate distance to ports!
  dis_to_port_rast <- this_eez_mask
  this_dist_port <- gDistance(this_raster_points, this_port_sp, byid = TRUE)
  this_dmin_port = apply(this_dist_port,2,min)
  dis_to_port_rast[!is.na(dis_to_port_rast[])]=this_dmin_port
  
  port_test_rast <- dis_to_port_rast
  port_test_rast[port_test_rast >.2 ] <- NA
  
  
  suitability_rast <- port_test_rast+coast_test_rast#+this_bathy_mask
  #suitability_rast[is.na(suitability_rast[])]=0
  # 5 Create a buffered coastline
  this_coast_meters <- st_transform(this_coast, crs = "+proj=moll")  %>% ## transform to a crs that can buffer in meters 
    st_buffer(dist = 200) %>% # 200 meters from shore. might be too close. 
    st_cast("MULTILINESTRING") %>% 
    st_transform(crs = crs(the_crs)) %>%  # 
    st_difference(this_country) %>% 
    mutate(info = "names")
  
  this_cell_polygons <- rasterToPolygons(suitability_rast, na.rm = TRUE)%>% 
    st_as_sf() %>%
    st_union()%>% 
    sf::st_as_sf()
  

  write_rds(this_coast_meters, paste0("data/temp_data/temp_spatial/", this_iso3, "_buffered_coast.rds"))
  write_rds(this_cell_polygons, paste0("data/temp_data/temp_spatial/", this_iso3, "_cell_polygons.rds"))
  
}
