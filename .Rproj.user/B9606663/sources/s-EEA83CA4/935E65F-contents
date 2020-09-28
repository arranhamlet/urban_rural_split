
pop_density_rural_urban_split <- function(pop_data_raster, urban_rural_raster, shapefile){
  
  all_adm_locations <- do.call(rbind, sapply(1:nrow(shapefile), function(x){
    
    adm_location <- shapefile[x, ]
    adm_location_pop <- rasterize(adm_location, pop_data_raster, mask = TRUE)
    adm_location_urban <- rasterize(adm_location, urban_rural_raster, mask = TRUE)
    
    adm_area <- area(adm_location)/1000^2
    
    
    urban_area_pop <- sum((adm_location_pop * adm_location_urban)[], na.rm = TRUE)
    urban_area_prop <- length(which(adm_location_urban[] == 1))/length(which(!is.na(adm_location_urban[])))
    
    population <- sum(adm_location_pop[], na.rm = TRUE)
    
    
    data.frame(state = adm_location$NAME_1,
               DIDE_CODE = adm_location$DIDE_CODE,
               area = adm_area,
               urban_area = adm_area*urban_area_prop,
               population = population,
               urban_population = urban_area_pop,
               urban_area_prop = urban_area_prop,
               urban_pop_prop = urban_area_pop/population,
               pop_density = population/adm_area,
               pop_density_urban = urban_area_pop/(adm_area*urban_area_prop),
               pop_density_rural = (population - urban_area_pop)/(adm_area - adm_area*urban_area_prop),
               stringsAsFactors = FALSE)
    
  }, simplify = FALSE))
  
  
  all_adm_locations

  
}