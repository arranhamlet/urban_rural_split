calculate_country_level_urban_threshold <- function(pop_data_raster, overall_level_urban){
  
  #Going to start with the definition of urban as a location with at last 300 people per km2
  #https://ourworldindata.org/urbanization
  for(i in 1:100){
    
    density_try <- if(i == 1) 300 else new_density
    
    urban_rural_try <- pop_data_raster
    urban_rural_try[urban_rural_try >= density_try] <- 1
    urban_rural_try[urban_rural_try != 1] <- 0
    
    pop_urban <- urban_rural_try*pop_data_raster
    
    total_pop_urban <- sum(pop_urban[], na.rm = TRUE)
    total_prop_of_pop_urban <- total_pop_urban/sum(pop_data_raster[], na.rm = TRUE)
    
    if(total_prop_of_pop_urban <= overall_level_urban*1.01 & total_prop_of_pop_urban >= overall_level_urban*0.99){
      break()
    } else {
      new_density <- density_try*total_prop_of_pop_urban/overall_level_urban
    }
  }
  
  data.frame(density_correct = density_try)
  
}