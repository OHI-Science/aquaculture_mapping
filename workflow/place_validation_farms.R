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


## New port data for 2019
ports_raw <- st_read("/home/shares/food-systems/Food_footprint/_raw_data/world_port_index/d2019/WPI.shp") %>%
  st_transform(the_crs) %>%
  filter(HARBORSIZE != "L", # filter out large ports 
         HARBORTYPE != "LC") # filter out lake or canals



data <- read_csv("marine/output/all_marine_farms.csv") %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = crs(food_raster), 
           agr = "constant") 

tonnage_per_farms <- read_csv("marine/output/all_points_to_add.csv") %>% 
  distinct(type, avg_tonnes_per_farm)


# data_final <- read_csv("data/all_marine_aquaculture_farms_sources_final.csv") %>%
#   mutate(type = case_when(species_group == "Salmonidae fish" ~ "salmon",
#                                    species_group == "Unfed or algae fed bivalve molluscs" ~ "bivalve",
#                                    species_group == "Shrimps and prawns" ~ "shrimp", 
#                                  species_group == "Bluefin tuna" ~ "tuna", 
#                                    species_group ==  "General marine fish" ~ "marine_fish_general",
#                           species_group == "Non-shrimp crustaceans" ~ "crustaceans")) %>%
#   filter(data_type_2 == "A") %>%
#   group_by(iso3c, type) %>%
#   summarise(num_farms = n())
# 
# sum(data_final$num_farms)


need_validation_allocation <- data %>%
  st_drop_geometry() %>%
  group_by(iso3c, type) %>%
  summarise(num_farms = n()) %>%
  ungroup() 
# 
# %>%
#   group_by(iso3c, type) %>%
#   filter(num_farms %in% c(data_final$num_farms)) %>%
#   filter(iso3c %in% c(data_final$iso3c))

sum(need_validation_allocation$num_farms) # 16138

# create arctic raster 
arctic_raster <- raster("/home/shares/food-systems/Aquaculture_mapping/latitude_abs.tif")
crs(arctic_raster) <- crs(fake_raster)
m <- c(-1000, -66, 1, 1000, 66, 1,  -66, 66, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
arctic_raster <- reclassify(arctic_raster, c(66,Inf, NA, 0,66,1))

arctic_fixed <- raster::resample(arctic_raster, blank_raster, method = "ngb")

arctic_fixed <- inverted_land + arctic_fixed
arctic_fixed[arctic_fixed[]==2] <- 1 ## add land mask

sf::sf_use_s2(FALSE)
rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")


for(i in 1:length(unique(need_validation_allocation$iso3c))) {
  
  # i = 1
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
  
  
  ## This ifelse statement is just for USA, RUS, and NZL, because if we don't restrain the bbox the bbox will span the entire globe, making the distance to ports calculation take like 5 hours... now it only takes 20 mins for USA!
  if(this_iso3 == "USA"){
    
    this_bbox <- st_bbox(c(xmin = -180, xmax = -65.69972, ymax = 66, ymin = 15.56312), crs = st_crs(this_area)) ## cut out arctic since it gets cut out later anyways.
    
    this_eez <- st_crop(this_eez, this_bbox)
    
  }else if(this_iso3 == "RUS"){
    this_bbox <- st_bbox(c(xmin = 28, xmax = 180, ymax = 66, ymin = 39.72669), crs = st_crs(this_area)) ## make it easier since the arctic will be classed out later anyways
    
    this_eez <- st_crop(this_eez, this_bbox)
    
  }else if(this_iso3 == "NZL"){
    this_bbox <- st_bbox(c(xmin = 160.6098, xmax = 180, ymax = -25.88826, ymin = -55.94930), crs = st_crs(this_area)) ## make it easier since the arctic will be classed out later anyways
    
    this_eez <- st_crop(this_eez, this_bbox)
    
    
    
  } else{
    
    ## 1.d get bounding box and limit to ymax = 66
    this_bbox_coords <- st_bbox(this_area)
    
    this_bbox_xmin = as.numeric(this_bbox_coords$xmin)
    this_bbox_ymin = as.numeric(this_bbox_coords$ymin)
    this_bbox_xmax = as.numeric(this_bbox_coords$xmax)
    
    this_bbox <- st_bbox(c(xmin = this_bbox_xmin, xmax = this_bbox_xmax, ymax = 66, ymin =this_bbox_ymin), crs = st_crs(this_area))
    
    this_eez <- st_crop(this_eez, this_bbox)
  }
  
  
  shrimp_info = ifelse(any(this_farm_info$type == "shrimp"), "yes", "no")
  
  
  ## 1.e filter for ports and coast line of this country
  this_coast <- this_country %>%
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%
    st_cast("MULTILINESTRING") # Cast to a multilinestring
  
  this_simplified_coast <- this_coast %>% 
    ms_simplify()
  
  
  #this_coast <- st_crop(coastline, this_bbox)
  these_ports <- st_crop(ports_raw, this_bbox)
  
  if(this_farm_info$iso3c == "HKG"){
    
    ## buffer coast 1 mile inland
    this_eez <- st_transform(this_simplified_coast, crs = "+proj=moll") %>% ## transform to a crs that can buffer in meters
      st_buffer(dist = 4000) %>% # buffer in meters km
      st_transform(crs = crs(the_crs))
  }
  
  ## 1.f crop down the rasters
  this_raster_area <- crop(fake_raster, this_eez)
  this_eez_mask <- crop(fake_raster, this_eez) 
  
  # 2 Calculate the dist to shore, port
  
  dis_to_port_rast <- distanceFromPoints(this_raster_area, these_ports)
  
  # writeRaster(dis_to_port_rast, file.path("/home/shares/food-systems/Food_footprint/all_food_systems/dataprep/aquaculture/marine/global_dis_to_port_raster.tif"))
  port_test_rast <- dis_to_port_rast
  
  # 31.30463*1000 # 31304.63 meters v1
  # 39.824*1000 = 39824 meters v2
  
  port_test_rast[port_test_rast >39651] <- NA # acceptable distance to port # we are doing 2; the mean (39.824 kilometers)
  port_test_rast[port_test_rast<1000] <- NA # need to be 1km away from ports/shipping lanes
  suitability_rast <- port_test_rast + arctic_fixed # add in arctic mask
  #plot(suitability_rast)
  # mapview(suitability_rast)
  # mapview(port_test_rast)
  # plot(arctic_fixed)
  # plot(port_test_rast)
  
  suitability_rast_shrimp <- port_test_rast ## save a suitability raster for shrimp placement
  
  # writeRaster(suitability_rast, paste0("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean/suitability_rast_subset_", this_iso3, ".tif"))
  # writeRaster(suitability_rast_shrimp, paste0("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean_and_land/suitability_rast_subset_", this_iso3, ".tif"))

  
  if(shrimp_info == "no"){
    
    ## place points normally, without shrimp 
    
    this_coast_meters <- st_transform(this_simplified_coast, crs = "+proj=moll") %>% ## transform to a crs that can buffer in meters 
      st_buffer(dist = 200) %>% # buffer in meters 
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
    
    
  }else{  
    
    #### Place shrimp farms if needed ####
    this_type_vector <- this_farm_info %>% 
      group_by(type) %>% 
      tidyr::expand(types = seq(1:num_farms)) %>% 
      filter(type != "shrimp") %>%
      pull(type) %>% 
      sample()
    
    this_type_vector_shrimp <- this_farm_info %>% 
      group_by(type) %>% 
      tidyr::expand(types = seq(1:num_farms)) %>% 
      filter(type == "shrimp") %>%
      pull(type) %>% 
      sample()
    
    
    this_coast_shrimp <- st_transform(this_simplified_coast, crs = "+proj=moll") %>%     
      st_buffer(dist = 100) %>% # buffer in meters 
      st_cast("MULTILINESTRING") %>%
      st_transform(crs = crs(the_crs)) %>% 
      st_difference(this_eez) %>%
      mutate(info = "names")
    
    this_cell_polygons_shrimp <- rasterToPolygons(suitability_rast_shrimp)%>% 
      st_as_sf() %>% 
      st_union()%>% 
      sf::st_as_sf()
    
    this_suitable_coast_shrimp <- st_intersection(this_coast_shrimp, this_cell_polygons_shrimp) %>%
      st_collection_extract("LINESTRING") %>%  
      as_Spatial()
    # mapview(this_suitable_coast_shrimp)
    
    
    the_farm_number_shrimp_df <- this_farm_info %>%
      filter(type == "shrimp")
    
    the_farm_number_shrimp = the_farm_number_shrimp_df$num_farms
    
    set.seed(001)
    # regularly sample points along line
    this_farm_points_shrimp <- sp::spsample(this_suitable_coast_shrimp, n=the_farm_number_shrimp*100, type = "random") %>% 
      sf::st_as_sf() %>%
      #mutate(row_number = row_number()) %>%
      #filter(!(row_number %in% c(103:110, 115:121, 128:138, 141:148))) %>%
      sample_n(the_farm_number_shrimp) %>% 
      mutate(
        iso3 = this_iso3,
        type = this_type_vector_shrimp
      ) #%>%
      #dplyr::select(-row_number)
    
    
    #### Place all other farms than shrimp farms ####
    this_coast_meters <- st_transform(this_simplified_coast, crs = "+proj=moll") %>% ## transform to a crs that can buffer in meters 
      st_buffer(dist = 200) %>% # buffer in meters 
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
    
    
    the_farm_number_df <- this_farm_info %>%
      filter(type != "shrimp")
    
    the_farm_number = sum(the_farm_number_df$num_farms)
    
    if(the_farm_number != 0){
      
      # regularly sample points along line
      this_farm_points_no_shrimp <- sp::spsample(this_suitable_coast, n=the_farm_number*100, type = "random") %>% 
        sf::st_as_sf() %>%
        sample_n(the_farm_number) %>% 
        mutate(
          iso3 = this_iso3,
          type = this_type_vector
        ) 
      
      this_farm_points <- rbind(this_farm_points_no_shrimp, this_farm_points_shrimp)
      
    } else{
      
      
      this_farm_points <- this_farm_points_shrimp
    }
    
    # mapview(this_farm_points, zcol = "type")
    
  }
  
  #UNCOMMENT WHEN SAVING! 
  write_rds(this_farm_points, paste0("data/temp_data/temp_validation_resub_scotland/temp_farms_", i, ".rds"))
  print(paste0(i, " - ",this_iso3, " has been saved"))
  
}

# save a file for 31km
all_farms <- list.files("data/temp_data/temp_validation_resub_scotland", pattern = "rds$", full.names = TRUE) %>% 
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
  filter(source == "modeled validation") %>% 
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


write_rds(all_farms, "data/all_validation_farms_resubmission.rds")



plotted <- all_farms %>% 
  filter(iso3c == "GBR")

mapview(plotted, 
        zcol = "source")


# Mosaic all suitability rasters together, and reproject to other resolutions for validity testing: 
all_suit <- list.files("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean_and_land", pattern = "suitability", full.names = TRUE)


# all_suit_stack <- raster::stack(all_suit_ocean)
# plot(all_suit_stack[[18]], col = "red")


allrasters <- lapply(all_suit, raster)

allrasters$fun <- sum

plot(allrasters[[57]])

# s <- raster::select(allrasters[[12]])
# plot(s)

## mosaic works 

mos <- do.call(mosaic, allrasters)
plot(mos)

unique(mos)


mos[mos == 0] <- NA
mos[mos >0] <- 1

plot(mos, col = "black")


writeRaster(mos, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean_land_suitability.tif"), overwrite = TRUE)



new_crs <- "+proj=cea +lat_ts=45 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
new_raster <- raster(nrows=3000, ncols=4736, xmn=-14207430, xmx=14208570, ymn=-9000182, ymx=8999818, crs = new_crs)

mos_cea <- projectRaster(mos, new_raster, crs = new_crs)

unique(mos_cea)
plot(mos_cea)

s <- raster::select(mos_cea)
plot(s)

mos_cea[mos_cea == 0] <- NA
mos_cea[!is.na(mos_cea)] <- 1

plot(area(mos_cea))
plot(area(mos))

plot(mos_cea, col = "black")
plot(mos, col = "black")

writeRaster(mos_cea, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean_land_suit_36km.tif"), overwrite = TRUE)



## need this for 25km2 cells 
res(mos_cea) <- c(5000, 5000)

mos_cea

plot(mos_cea)

mos_cea_25 <- projectRaster(mos, mos_cea, crs = crs(mos_cea))

plot(mos_cea_25)
plot(area(mos_cea_25))

mos_cea_25[mos_cea_25 == 0] <- NA
mos_cea_25[!is.na(mos_cea_25)] <- 1

plot(mos_cea_25)

writeRaster(mos_cea_25, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean_land_suit_25km.tif"), overwrite = TRUE)


## now reproject to 0.25 degree cell sizes 

mos_quart <- mos
res(mos_quart) <- c(0.25,0.25)
plot(area(mos_quart))

mos_quart_reproj <- projectRaster(mos, mos_quart, crs = crs(mos_quart))

plot(mos_quart_reproj)

unique(mos_quart_reproj)
plot(area(mos_quart_reproj))

writeRaster(mos_quart_reproj, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean_land_suit_quart.tif"), overwrite = TRUE)

