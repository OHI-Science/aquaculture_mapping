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



# test <- ports_raw %>%
#   filter(HARB_SIZE_ == "L")
# mapview(test)
# 
# test2 <- ports_raw %>%
#   filter(HARB_SIZE_ != "L")
# mapview(test2)

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

# create arctic raster 
arctic_raster <- raster("/home/shares/food-systems/Aquaculture_mapping/latitude_abs.tif")
crs(arctic_raster) <- crs(fake_raster)
m <- c(-1000, -66, 1, 1000, 66, 1,  -66, 66, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
arctic_raster <- reclassify(arctic_raster, c(66,Inf, NA, 0,66,1))

arctic_fixed <- resample(arctic_raster, blank_raster, method = "ngb")

arctic_fixed <- inverted_land + arctic_fixed
arctic_fixed[arctic_fixed[]==2] <- 1 ## add land mask


for (i in 42) {
  # 52:length(unique(need_national_allocation$iso3c))
  # i = 42
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
  port_test_rast[port_test_rast >45000] <- NA # acceptable distance to port # we are doing 3; 10km, 35km, and 45km
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
    st_collection_extract("LINESTRING") %>% 
    as_Spatial()
  
# mapview(this_suitable_coast)  
  
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
  write_rds(this_farm_points, paste0("data/temp_data/temp_45km/temp_farms_", i, ".rds"))
  print(paste0(i, " - ",this_iso3, " has been saved"))
  
}

# save a file for 10km, 35km, and 45km
all_farms <- list.files("data/temp_data/temp_45km", pattern = "rds$", full.names = TRUE) %>% 
  map_df(., readRDS) %>% 
  rename(iso3c = iso3) %>% 
  dplyr::mutate(source = "modeled") %>% 
  rbind(data %>% dplyr::mutate(source = "real")) %>% 
  left_join(tonnage_per_farms)

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
  left_join(need_national_allocation) %>% 
  filter(number_places != num_farms)

testing %>% 
  group_by(iso3c, type) %>%
  summarise(number_places = n()) %>% 
  right_join(need_national_allocation)


write_rds(all_farms, "data/all_modeled_marine_farms_45km.rds")



plotted <- all_farms %>% 
  filter(iso3c == "USA")

mapview(plotted, 
        zcol = "source")




