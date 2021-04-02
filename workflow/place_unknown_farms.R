#### load libraries and data ####
#------------------------------------------------------------------------------#
library(sf)
library(spatialEco)
library(tidyverse)
library(raster)
library(rgeos)
library(rmapshaper)
library(mapview)

# load in crs, coastlines, eez, masks, country shapes, ports, depth, and rasters
source("./_spatial/template_raster.R")
gadm_diss <- st_read("/home/shares/food-systems/Global_datasets/World_map/World_ISO_dissolve.shp")
gadm_full <- st_read("/home/shares/food-systems/Global_datasets/World_map/gadm36.shp")
land_mask <- raster("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/land_mask.tif")
eez_mask <- raster("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/eez_mask.tif")
eez_sf <- st_read("/home/shares/food-systems/Global_datasets/EEZ/eez_v10.shp")
coastline <- readRDS("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/outline.rds")


blank_raster <- food_raster
blank_raster[] <- 1

the_crs <- crs(fake_raster, asText = TRUE)

ports_raw <- st_read("/home/shares/clean-seafood/raw_data/ports_data/Commercial_ports.shp") %>% 
  st_transform(the_crs)


data <- read_csv("marine/output/all_marine_farms.csv") %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = crs(food_raster), 
           agr = "constant") 

tonnage_per_farms <- read_csv("marine/output/all_points_to_add.csv") %>% 
  distinct(type, avg_tonnes_per_farm)

need_national_allocation <- read_csv("marine/output/all_points_to_add.csv") %>% 
  group_by(iso3c, type) %>% 
  summarize(num_farms = sum(num_additional_farms))

all_the_farm_points <- data %>% 
  dplyr::select(iso3c, type) %>%
  .[0, ]


for (i in 3:length(unique(need_national_allocation$iso3c))) {
  
  ## 1.a get country shape file
  this_iso3 <- unique(need_national_allocation$iso3c)[i]
  
  this_farm_info <- need_national_allocation %>% 
    filter(iso3c == this_iso3) 
  
  set.seed(001)
  
  this_type_vector <- this_farm_info %>% 
    group_by(type) %>% 
    expand(types = seq(1:num_farms)) %>% 
    pull(type) %>% 
    sample()
  
  the_farm_number <- sum(this_farm_info$num_farms)
  
  
  this_country <- gadm_full %>% 
    filter(GID_0 == this_iso3) %>% 
    group_by(GID_0) %>% 
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%
    ungroup()
  #select(GID_0) %>% 
  
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
  
  
  this_simplified_coast <- this_coast %>% 
    ms_simplify()
  
  
  #this_coast <- st_crop(coastline, this_bbox)
  these_ports <- st_crop(ports_raw, this_bbox)
  
  ## 1.f crop down the rasters
  this_raster_area <- crop(fake_raster, this_eez)
  this_eez_mask <- crop(fake_raster, this_eez) 
  
  # 2 Calculate the dist to shore, port, and pull depth
  
  ## 2.a prep the coasts, ports, and eez to spatial points 
  this_raster_points <- as(this_eez_mask, "SpatialPoints")
  
  dis_to_port_rast <- distanceFromPoints(this_raster_area, these_ports)
  port_test_rast <- dis_to_port_rast
  port_test_rast[port_test_rast >35000 ] <- NA
  suitability_rast <- port_test_rast
  

  this_coast_meters <- st_transform(this_simplified_coast, crs = "+proj=moll") %>% ## transform to a crs that can buffer in meters 
    st_buffer(dist = 200) %>% # buffer in meters km 
    st_cast("MULTILINESTRING") %>% 
    st_transform(crs = crs(the_crs)) %>%  # 
    st_difference(this_country) %>% 
    mutate(info = "names")
  
  this_cell_polygons <- rasterToPolygons(suitability_rast)%>% 
    st_as_sf() %>% 
    st_union()%>% 
    sf::st_as_sf()
  
  this_suitable_coast <- st_intersection(this_coast_meters, this_cell_polygons) %>% 
    as_Spatial()
  
  # regularly sample points along line
  this_farm_points <- sp::spsample(this_suitable_coast, n=the_farm_number*100, type = "random") %>% 
    sf::st_as_sf() %>% 
    sample_n(the_farm_number) %>% 
    mutate(
      iso3 = this_iso3,
      type = this_type_vector
    )
  
  write_rds(this_farm_points, paste0("data/temp_data/temp_farms_", i, ".rds"))
  
  
}


all_farms <- list.files("data/temp_data/", pattern = "rds$", full.names = TRUE) %>% 
  map_df(., readRDS) %>% 
  rename(iso3c = iso3) %>% 
  mutate(source = "modeled") %>% 
  #left_join(tonnage_per_farms)
  rbind(data %>% mutate(source = "real"))# %>% 
  #left_join()






plotted <- all_farms %>% 
  filter(iso3c == "USA")

mapview(plotted, 
        zcol = "type")










#### Rasterize ####
#------------------------------------------------------------------------------#




test <- rasterize(all_farms, 
                  blank_raster, 
                  field = "avg_tonnes_per_farm", 
                  fun = )