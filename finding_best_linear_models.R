reduced_data <- read.csv('C:\\Users\\ybard\\Downloads\\project_data.csv') #This is the data sent by eric Change this according to your file location

#correlations for powerpoint - Jade
in_var <- c("in.bathroom_spot_vent_hour","in.bedrooms",                                         
             "in.building_america_climate_zone","in.ceiling_fan"  ,                                     
             "in.city","in.clothes_dryer",                                     
             "in.clothes_washer","in.cooking_range",                                     
             "in.cooling_setpoint","in.cooling_setpoint_has_offset",                      
             "in.cooling_setpoint_offset_magnitude","in.cooling_setpoint_offset_period",                  
             "in.county","in.county_and_puma",                                   
             "in.dishwasher","in.ducts",                                             
             "in.federal_poverty_level","in.geometry_attic_type",                               
             "in.geometry_floor_area","in.geometry_floor_area_bin",                           
             "in.geometry_foundation_type","in.geometry_garage",                                   
             "in.geometry_stories","in.geometry_wall_exterior_finish",                     
             "in.geometry_wall_type","in.has_pv",                                            
             "in.heating_fuel","in.heating_setpoint",                                  
             "in.heating_setpoint_offset_magnitude","in.heating_setpoint_offset_period",                    
             "in.hot_water_fixtures","in.hvac_cooling_efficiency",                           
             "in.hvac_cooling_partial_space_conditioning","in.hvac_cooling_type",                                 
             "in.hvac_has_ducts","in.hvac_has_zonal_electric_heating",                   
             "in.hvac_heating_efficiency","in.hvac_heating_type",                                 
             "in.hvac_heating_type_and_fuel","in.income",                                            
             "in.income_recs_2015","in.income_recs_2020",                                  
             "in.infiltration","in.insulation_ceiling",                                
             "in.insulation_floor","in.insulation_foundation_wall",                        
             "in.insulation_rim_joist","in.insulation_roof",                                   
             "in.insulation_slab","in.insulation_wall",                                   
             "in.lighting" ,"in.misc_extra_refrigerator",                           
             "in.misc_freezer","in.misc_gas_fireplace" ,                               
             "in.misc_gas_grill","in.misc_gas_lighting",                                 
             "in.misc_hot_tub_spa","in.misc_pool",                                         
             "in.misc_pool_heater","in.misc_pool_pump",                                    
             "in.misc_well_pump","in.occupants",                                         
             "in.orientation","in.plug_load_diversity"  ,                             
             "in.puma","in.puma_metro_status"  ,                               
             "in.pv_orientation","in.pv_system_size",                                    
             "in.range_spot_vent_hour","in.reeds_balancing_area" ,                             
             "in.refrigerator","in.roof_material"      ,                               
             "in.tenure","in.usage_level"  ,                                     
             "in.vacancy_status","in.vintage" ,                                          
             "in.vintage_acs","in.water_heater_efficiency",                           
             "in.water_heater_fuel","in.weather_file_city",                                 
             "in.weather_file_latitude","in.weather_file_longitude" ,                           
             "in.window_areas","in.windows" ,                                          
             "upgrade.insulation_roof","upgrade.water_heater_efficiency",                      
             "upgrade.infiltration_reduction","upgrade.geometry_foundation_type",                     
             "upgrade.clothes_dryer","upgrade.insulation_ceiling",                           
             "upgrade.ducts","upgrade.hvac_heating_type" ,                          
             "upgrade.insulation_wall","upgrade.insulation_foundation_wall",                   
             "upgrade.hvac_heating_efficiency","upgrade.cooking_range")

out_var <- c("out.electricity.ceiling_fan.energy_consumption" ,      
              "out.electricity.clothes_dryer.energy_consumption","out.electricity.clothes_washer.energy_consumption"    
             , "out.electricity.cooling_fans_pumps.energy_consumption", "out.electricity.cooling.energy_consumption"           
              ,"out.electricity.dishwasher.energy_consumption","out.electricity.freezer.energy_consumption"           
              ,"out.electricity.heating_fans_pumps.energy_consumption", "out.electricity.heating.energy_consumption"           
              ,"out.electricity.hot_tub_heater.energy_consumption","out.electricity.hot_tub_pump.energy_consumption"      
              ,"out.electricity.hot_water.energy_consumption","out.electricity.lighting_exterior.energy_consumption" 
              ,"out.electricity.lighting_garage.energy_consumption","out.electricity.lighting_interior.energy_consumption" 
              ,"out.electricity.mech_vent.energy_consumption","out.electricity.plug_loads.energy_consumption"        
              ,"out.electricity.pool_heater.energy_consumption","out.electricity.pool_pump.energy_consumption"         
              ,"out.electricity.pv.energy_consumption","out.electricity.range_oven.energy_consumption"        
              ,"out.electricity.refrigerator.energy_consumption","out.electricity.well_pump.energy_consumption"         
              ,"out.natural_gas.fireplace.energy_consumption","out.natural_gas.grill.energy_consumption"             
             , "out.natural_gas.hot_tub_heater.energy_consumption","out.natural_gas.lighting.energy_consumption"          
              ,"out.natural_gas.pool_heater.energy_consumption")

corr <- matrix(nrow=28, ncol=96)

for(i in 1:length(out_var)){
  for(j in 1:length(in_var)){
    corr[i,j] = abs(cor(reduced_data[,out_var[i]], reduced_data[,in_var[j]]))    
  }
}

sort(corr[1,], decreasing = TRUE)
in_var[order(corr[1,], decreasing = TRUE)]


#Finding the optimal model - Jeet and Erik


#finding the best predictors for each output in a linear model. 
significant_terms <- summary(lm(out.electricity.lighting_exterior.energy_consumption~in.bathroom_spot_vent_hour+in.bedrooms+in.building_america_climate_zone+in.ceiling_fan+in.city+in.clothes_dryer+in.clothes_washer+in.cooking_range+in.cooling_setpoint+in.cooling_setpoint_has_offset+in.cooling_setpoint_offset_magnitude+in.cooling_setpoint_offset_period+in.county+in.county_and_puma+in.dishwasher+in.ducts+in.federal_poverty_level+in.geometry_attic_type+in.geometry_floor_area+in.geometry_floor_area_bin+in.geometry_foundation_type+in.geometry_garage+in.geometry_stories+in.geometry_wall_exterior_finish+in.geometry_wall_type+in.has_pv+in.heating_fuel+in.heating_setpoint+in.heating_setpoint_offset_magnitude+in.heating_setpoint_offset_period+in.hot_water_fixtures+in.hvac_cooling_efficiency+in.hvac_cooling_partial_space_conditioning+in.hvac_cooling_type+in.hvac_has_ducts+in.hvac_has_zonal_electric_heating+in.hvac_heating_efficiency+in.hvac_heating_type+in.hvac_heating_type_and_fuel+in.income+in.income_recs_2015+in.income_recs_2020+in.infiltration+in.insulation_ceiling+in.insulation_floor+in.insulation_foundation_wall+in.insulation_rim_joist+in.insulation_roof+in.insulation_slab+in.insulation_wall+in.lighting+in.misc_extra_refrigerator+in.misc_freezer+in.misc_gas_fireplace+in.misc_gas_grill+in.misc_gas_lighting+in.misc_hot_tub_spa+in.misc_pool+in.misc_pool_heater+in.misc_pool_pump+in.misc_well_pump+in.occupants+in.orientation+in.plug_load_diversity+in.puma+in.puma_metro_status+in.pv_orientation+in.pv_system_size+in.range_spot_vent_hour+in.reeds_balancing_area+in.refrigerator+in.roof_material+in.tenure+in.usage_level+in.vacancy_status+in.vintage+in.vintage_acs+in.water_heater_efficiency+in.water_heater_fuel+in.weather_file_city+in.weather_file_latitude+in.weather_file_longitude+in.window_areas+in.windows+upgrade.insulation_roof+upgrade.water_heater_efficiency+upgrade.infiltration_reduction+upgrade.geometry_foundation_type+upgrade.clothes_dryer+upgrade.insulation_ceiling+upgrade.ducts+upgrade.hvac_heating_type+upgrade.insulation_wall+upgrade.insulation_foundation_wall+upgrade.hvac_heating_efficiency+upgrade.cooking_range, data=reduced_data))$coefficients[,"Pr(>|t|)"]

significant_terms <- summary(lm(out.electricity.ceiling_fan.energy_consumption~date+time, data=reduced_data))$coefficients[,"Pr(>|t|)"]

paste(names(significant_terms)[significant_terms<0.001][2:length(names(significant_terms)[significant_terms<0.001])], collapse='+')



#Output dimensions for Sandeep to build Shiny app
#4248240x29, filter by column or row, row wise sum.