#------------------------------------------------------------------------------#
## Project :  Aquaculture Mapping
## Purpose :  Generate
## Date    :  02/11/2021
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#


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

#x <- raster("/home/shares/food-systems/Aquaculture_mapping/Data/ports/distance-from-port-v20201104.tiff")

#plot(x)
# ports_gfw <- read_csv("/home/shares/food-systems/Aquaculture_mapping/Data/ports/named_anchorages_v2_20201104.csv") %>% 
#   st_as_sf(coords = c("lon", "lat"),
#            crs = 4326) %>% 
#   head(400)

#mapview::mapview(ports_gfw, zcol = "at_dock")

the_crs <- crs(fake_raster, asText = TRUE)
# depth and ports
ports_raw <- st_read("/home/shares/clean-seafood/raw_data/ports_data/Commercial_ports.shp") %>% 
  st_transform(the_crs)

#all_ports_sp <- as(ports_raw, "Spatial")
#dist_ports_raster <- distanceFromPoints(food_raster, all_ports_sp)
#write_rds(dist_ports_raster, "dist_ports_raster.rds")



# bathy_raw <- raster("/home/shares/clean-seafood/raw_data/bathymetry_data/full_bathy_layer.tif") %>%
#   resample(fake_raster, method = 'bilinear')
#   #projectRaster(to = fake_raster,
#                 #res = res(fake_raster))
# 
# bathy_raw <- bathy_raw %>% 
#   projectRaster(to = fake_raster)
# 
# #bathy_raw2 <- bathy_raw
# #bathy_raw <- bathy_raw2
# bathy_raw[bathy_raw >    10] <- NA # replace all values over 1m asl as NA
# bathy_raw[bathy_raw < -250] <- NA # replace all values under 250 bsl as NA
# bathy_raw[!is.na(bathy_raw)] <- 1 # replace all suitable depths with 1
# 


data <- read_csv("marine/output/all_marine_farms.csv") %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = crs(food_raster), 
           agr = "constant") 



#### load in number of farms ####
#------------------------------------------------------------------------------#










# THIS SECTION CAN BE DELETED
# 
# 
# 
# 
# 
# # load in each of the aquaculture maps and convert to sf objects
# all_farms_raw   <-
#   bind_rows(read_csv("marine/bivalve/bivalve_farms/data/global_bivalve_farm_lat_lon_data_quality.csv") %>% 
#               mutate(type = "bivalve") %>% 
#               dplyr::select(-X1),
#             read_csv("marine/crustaceans/crustacean_farms/data/global_crustacean_farm_lat_lon_data_quality.csv") %>% 
#               mutate(type = "crustacean") %>% 
#               dplyr::select(-prod),
#             read_csv("marine/salmon/salmon_farm/data/global_salmon_farm_lat_lon_quality.csv") %>% 
#               mutate(type = "salmon"),
#             read_csv("marine/shrimp/shrimp_farms/data/global_shrimp_farm_lat_lon_data_quality.csv") %>% 
#               mutate(type = "shrimp"),
#             read_csv("marine/tuna/tuna_farms/data/global_tuna_farm_lat_lon_data_quality.csv") %>% 
#               mutate(type = "tuna"),
#             read_csv("marine/marine_fish_general/finfish_farms/data/global_finfish_farm_lat_lon_data_quality.csv") %>% 
#               mutate(type = "marine_fish_general")) %>%
#   st_as_sf(., 
#            coords = c("lon", "lat"),
#            crs = crs(food_raster), 
#            agr = "constant") 
# 
# all_the_farm_points <- data %>% 
#   dplyr::select(iso3c, type) %>%
#   .[0, ]






####  Allocate spatially ####
#------------------------------------------------------------------------------#
# First we will allocate the farms for countries where we have the number of 
# farms, but not the location. For some areas we have the number of farms per
# subregion so we do those separately as well

## do it for national 
need_national_allocation <- read_csv("marine/output/all_points_to_add.csv") %>% 
  group_by(iso3c, type) %>% 
  summarize(num_farms = sum(num_additional_farms))

all_the_farm_points <- data %>% 
  dplyr::select(iso3c, type) %>%
  .[0, ]

# stucture of the loop for each country 
# 1. Get the country ranges land-eez-bbox
# 2. Calculate the dist to shore, port, and pull depth
# 3. Create final acceptable range rasters
# 4. Create buffered coast line

# layers of data needed gadm_full, eez_sf, coastline, 

# generate prediction points for each national allocation area
for (i in 43:length(unique(need_national_allocation$iso3c))) {
  
  #1. Get the country ranges land-eez-bbox
  # need to do china!!! i = 5 an cuba 7?
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
  
  #this_coast <- st_crop(coastline, this_bbox)
  these_ports <- st_crop(ports_raw, this_bbox)
  
  ## 1.f crop down the rasters
  this_raster_area <- crop(fake_raster, this_eez)
  this_eez_mask <- crop(fake_raster, this_eez) #%>% 
    #crop(this_eez)
  #this_eez_mask <- rasterize(this_eez)
  
  # 2 Calculate the dist to shore, port, and pull depth
  
  ## 2.a prep the coasts, ports, and eez to spatial points 
  this_raster_points <- as(this_eez_mask, "SpatialPoints")
  
  #mapview(this_raster_points)
  this_area_sp <- as(this_area, "Spatial")
  this_coast_sp <- as(this_coast, "Spatial")
  this_port_sp <- as(these_ports, "Spatial")
  
  ## 2.b calculate distance to the coast!
  #dis_to_coast_rast <- this_eez_mask
  #this_dist <- gDistance(this_raster_points, this_coast_sp, byid = TRUE)
  #this_dmin = apply(this_dist,2,min)
  #dis_to_coast_rast[!is.na(dis_to_coast_rast[])]=this_dmin
  
  dis_to_port_rast <- distanceFromPoints(this_raster_area, these_ports, meters = FALSE)
  
  #coast_test_rast <-dis_to_coast_rast
  #coast_test_rast[coast_test_rast > 1] <- NA # replace all values over 1m asl as NA
  
  #plot(dis_to_coast_rast)
  ## 2.c calculate distance to ports!
  #dis_to_port_rast <- this_eez_mask
  #this_dist_port <- gDistance(this_raster_points, this_port_sp, byid = TRUE)
  #this_dmin_port = apply(this_dist_port,2,min)
  #dis_to_port_rast[!is.na(dis_to_port_rast[])]=this_dmin_port
  
  port_test_rast <- dis_to_port_rast
  port_test_rast[port_test_rast >100000 ] <- NA
  
  ## 3.d get depth data!
  # this_bathy_mask <- raster::crop(bathy_raw, this_area_sp) 
  # this_bathy_mask2 <- raster::mask(this_bathy_mask, this_area_sp)
    
  # 4 make full suitability map!
  suitability_rast <- port_test_rast#+coast_test_rast#+this_bathy_mask
  #suitability_rast[suitability_rast >= 0] <- 1
  
  #suitability_rast[!is.na(suitability_rast)] = 1  
  #suitability_rast[!is.na(suitability_rast[])] <- 1
  #values(suitability_rast)[!is.na(values(suitability_rast))] = 1
  # 5 Create a buffered coastline
  this_coast_meters <- st_transform(this_coast, crs = 7801) %>% ## transform to a crs that can buffer in meters 
    st_buffer(dist = 200) %>% # buffer in meters km 
    st_cast("MULTILINESTRING") %>% 
    st_transform(crs = crs(the_crs)) %>%  # 
    st_difference(this_country) %>% 
    mutate(info = "names")
  
  ## create polygons of suitable rasters
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
    
  all_the_farm_points <- all_the_farm_points %>% 
    rbind(this_farm_points)
  
  write_rds(all_the_farm_points, paste0("data/temp_data/temp_farms_", i, ".rds"))
  
}


library(mapview)
# suitability layers
mapview(dis_to_coast_rast) # might not be needed! 
mapview(port_test_rast)
mapview(suitability_rast) 
mapview(this_coast_meters)
mapview(this_suitable_coast)


# Making the suitable coastline
mapview(all_the_farm_points)

# south korea does not have enough ports I think. 

c(1,2,3) %>% 
  sample()


test <- read_rds("data/temp_data/temp_farms_4.rds")
