To recreate the maps the following codes are used in this order:

To recreate the maps the following codes are used in this order:

1. fao_finfish_production.Rmd : Clean FAO production data

2. finfish_farm_locations.Rmd : cleans lat/long information for finfish farms (in some cases we have farms, in other cases we have no information on individual farms and we use generic points to save the data)

3. STEP3_gapfill_finfish_production.Rmd : this produces estimates for total farm numbers for each country that has incomplete farm information. After you finish this script, move on to other species groups.

See README.md in the **marine** folder for comprehensive instructions. 