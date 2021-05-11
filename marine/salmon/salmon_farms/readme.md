To recreate the maps the following codes are used in this order:

1. fao_salmon_production.Rmd : Clean FAO production data

2. salmon_farm_locations.Rmd : cleans lat/long information for salmon farms (in some cases we have farms, in other cases we have no information on individual farms and we use generic points to save the data)

3. STEP3_gapfill_salmon_production.Rmd : this produces estimates for total farm numbers for each country that has incomplete farm information. After you finish this script, move on to other species groups. 

