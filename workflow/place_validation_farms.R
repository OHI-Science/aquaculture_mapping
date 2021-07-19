## This script will place specific number of farms with known locations using our location modelling approach. The resulting dataset will serve as a validation method for our modelling. 


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


inverted_land <- land_mask
inverted_land[is.na(inverted_land[])] <- .5
inverted_land[inverted_land[]==1] <- NA
inverted_land[inverted_land[]==.5] <- 1

blank_raster <- food_raster
blank_raster[] <- 1

the_crs <- crs(fake_raster, asText = TRUE)


ports_raw <- st_read("/home/shares/food-systems/Food_footprint/_raw_data/world_port_index/Commercial_ports.shp") %>%
  st_transform(the_crs) %>%
  filter(HARB_SIZE_ != "L") # remove L ports


data <- read_csv("marine/output/all_marine_farms.csv") %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = crs(food_raster), 
           agr = "constant") 

tonnage_per_farms <- read_csv("marine/output/all_points_to_add.csv") %>% 
  distinct(type, avg_tonnes_per_farm)


need_validation_allocation <- data %>%
  st_drop_geometry() %>%
  group_by(iso3c, type) %>%
  summarise(num_farms = n()) %>%
  ungroup()


# create arctic raster 
arctic_raster <- raster("/home/shares/food-systems/Aquaculture_mapping/latitude_abs.tif")
crs(arctic_raster) <- crs(fake_raster)
m <- c(-1000, -66, 1, 1000, 66, 1,  -66, 66, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
arctic_raster <- reclassify(arctic_raster, c(66,Inf, NA, 0,66,1))

arctic_fixed <- raster::resample(arctic_raster, blank_raster, method = "ngb")

arctic_fixed <- inverted_land + arctic_fixed
arctic_fixed[arctic_fixed[]==2] <- 1 ## add land mask


for (i in 1:length(unique(need_validation_allocation$iso3c))) {
  
  # i = 57
  ## 1.a get country shape file
  this_iso3 <- unique(need_validation_allocation$iso3c)[i]
  
  this_farm_info <- need_validation_allocation %>% 
    filter(iso3c == this_iso3) 
  
  set.seed(001)
  
  this_type_vector <- this_farm_info %>% 
    group_by(type) %>% 
    tidyr::expand(types = seq(1:num_farms)) %>% 
    pull(type) %>% 
    sample()
  
  the_farm_number <- sum(this_farm_info$num_farms)
  
  
  this_country <- gadm_full %>% 
    filter(GID_0 == this_iso3) %>% 
    group_by(GID_0) %>% 
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%
    ungroup()

  
  # plot(this_country)
  # class(this_country)
  
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
  
  
  ## This ifelse statement is just for USA and RUS, because if we don't restrain the bbox the bbox will span the entire globe, making the distance to ports calculation take like 5 hours... now it only takes 20 mins for USA!
  if(this_iso3 == "USA"){
    
    this_bbox <- st_bbox(c(xmin = -180, xmax = -65.69972, ymax = 66, ymin = 15.56312), crs = st_crs(this_area)) ## cut out arctic since it gets cut out later anyways.
    
    this_eez <- st_crop(this_eez, this_bbox)
    
  }else if(this_iso3 == "RUS"){
    this_bbox <- st_bbox(c(xmin = 28, xmax = 180, ymax = 66, ymin = 39.72669), crs = st_crs(this_area)) ## make it easier since the arctic will be classed out later anyways
    
    this_eez <- st_crop(this_eez, this_bbox)
    
  }else{
    
    ## 1.d get bounding box and limit to ymax = 66
    this_bbox_coords <- st_bbox(this_area)
    
    this_bbox_xmin = as.numeric(this_bbox_coords$xmin)
    this_bbox_ymin = as.numeric(this_bbox_coords$ymin)
    this_bbox_xmax = as.numeric(this_bbox_coords$xmax)
    
    this_bbox <- st_bbox(c(xmin = this_bbox_xmin, xmax = this_bbox_xmax, ymax = 66, ymin =this_bbox_ymin), crs = st_crs(this_area))
    
    this_eez <- st_crop(this_eez, this_bbox)
  }
  
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
  
  # 2 Calculate the dist to shore, port
  
  dis_to_port_rast <- distanceFromPoints(this_raster_area, these_ports)
  
  # writeRaster(dis_to_port_rast, file.path("/home/shares/food-systems/Food_footprint/all_food_systems/dataprep/aquaculture/marine/global_dis_to_port_raster.tif"))
  port_test_rast <- dis_to_port_rast
  
  # 31.30463*1000 # 31304.63 meters 
  # 18.85009*1000 # 18850.09 meters
  
  port_test_rast[port_test_rast >31305] <- NA # acceptable distance to port # we are doing 2; the mean (31.305 kilometers) and median (18.850 kilometers) of distance from known locations to ports 
  port_test_rast[port_test_rast<1000] <- NA # need to be 1km away from ports/shipping lanes
  suitability_rast <- port_test_rast + arctic_fixed # add in arctic mask
  #plot(suitability_rast)
  # mapview(suitability_rast)
  # mapview(port_test_rast)
  # plot(arctic_fixed)
  
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
    st_collection_extract("LINESTRING") %>%  # comment this out for <= 200m
    as_Spatial() 
  
  
  
  # regularly sample points along line
  this_farm_points <- sp::spsample(this_suitable_coast, n=the_farm_number*100, type = "random") %>% 
    sf::st_as_sf() %>%
    sample_n(the_farm_number) %>% 
    mutate(
      iso3 = this_iso3,
      type = this_type_vector
    ) 
  
  if (nrow(this_farm_points) != the_farm_number) break
  
  #UNCOMMENT WHEN SAVING! 
  write_rds(this_farm_points, paste0("data/temp_data/temp_validation/temp_farms_", i, ".rds"))
  print(paste0(i, " - ",this_iso3, " has been saved"))
  
  
  ## testing only albania... the model does exceptionally well...
  # data_alb <- data %>%
  #   filter(iso3c == "ALB") %>%
  #   mutate(data_type = "real")
  # mapview(data_alb)
  # 
  # mapview(this_farm_points)
  # 
  # st_distance(this_farm_points, data_alb)
  # testing <- this_farm_points %>%
  #   rename("iso3c" = "iso3") %>%
  #   mutate(data_type = "model") %>%
  #   rbind(data_alb)
  # 
  # mapview(testing, zcol = "data_type")
  
}

# save a file for 18km, 31km
all_farms <- list.files("data/temp_data/temp_validation", pattern = "rds$", full.names = TRUE) %>% 
  map_df(., readRDS) %>% 
  rename(iso3c = iso3) %>% 
  dplyr::mutate(source = "modeled validation") %>% 
  rbind(data %>% dplyr::mutate(source = "real")) 

n_placed_known <- all_farms %>% 
  st_set_geometry(NULL) %>% 
  group_by(iso3c, type, source) %>%
  summarize(count = n()) %>% 
  pivot_wider(names_from = source, 
              values_from = count)


## check for all farms and countries

testing <- all_farms %>% 
  filter(source == "modeled") %>% 
  st_set_geometry(NULL)

testing %>% 
  group_by(iso3c, type) %>%
  summarise(number_places = n()) %>% 
  left_join(need_validation_allocation) %>% 
  filter(number_places != num_farms)

testing %>% 
  group_by(iso3c, type) %>%
  summarise(number_places = n()) %>% 
  right_join(need_validation_allocation)


write_rds(all_farms, "data/all_validation_farms.rds")



plotted <- all_farms %>% 
  filter(iso3c == "USA")

mapview(plotted, 
        zcol = "source")

