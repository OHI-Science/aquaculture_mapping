## This script will save our suitability maps for each country with known farms. The resulting file will serve as validation methods for our modelling. 


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



need_validation_allocation <- data %>%
  st_drop_geometry() %>%
  group_by(iso3c, type) %>%
  summarise(num_farms = n()) %>%
  ungroup() 

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
  
  ## comment these out if you want to resave the suitability layers.. only need to do if the distance from port variables change.
  # writeRaster(suitability_rast, paste0("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean/suitability_rast_subset_", this_iso3, ".tif"), overwrite = TRUE)
  # writeRaster(suitability_rast_shrimp, paste0("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean_and_land/suitability_rast_subset_", this_iso3, ".tif"), overwrite = TRUE)
  
  
}




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

plot(mos)


writeRaster(mos, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/suitability_rasts/ocean_land_suitability.tif"), overwrite = TRUE)



new_crs <- "+proj=cea +lat_ts=45 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
new_raster <- raster(nrows=3000, ncols=4736, xmn=-14207430, xmx=14208570, ymn=-9000182, ymx=8999818, crs = new_crs)

mos_cea <- projectRaster(mos, new_raster, crs = new_crs)

unique(mos_cea)
plot(mos_cea)


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

