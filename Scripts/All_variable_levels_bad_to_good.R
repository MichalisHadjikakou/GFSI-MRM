##############################################################
#### All PREDICTOR LEVELS IN ASCENDING ORDER OF AMBITION ####
############################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Re-arranging predictors from bad to good


source("Functions/Prediction_dataset_relative.R") # See source script for explanation and sources of all scenario levels
All_pred_levels <- Prediction_dataset(Base_year,Scen_year,levels) 
  
  ## Demand-side interventions
  
  Population <- All_pred_levels$Population$Pop_levels %>% sort(decreasing = T) # SSP1 (most optimistic) + Low, Median, High estimate from UN DESA (2019) - source: https://population.un.org/wpp/Download/Standard/Population/
  Diet <- c("RICH DIET","BAU DIET","REDUCED MEAT","LOW ASF") # Allows enough calories for vegan diets (incl.waste) and also high calorie western diets 
  Plant_kcal <- c(2900,2700,2500,2300) # Different levels of plant calories
  Waste <- c("BAU_High","Current","BAU_Low","Half") # Allows enough calories for flexitarian diets and also very high meat western diets (30% animal calories) 
  
  ## Supply-side interventions
  
  Yield_level <- All_pred_levels$Yield$Yield_levels # Yield increase relative to 2010 baseline at 15% increments
  Feed_efficiency <- All_pred_levels$FCR_FCF$Feed_efficiency %>% unique() # Based on dataset and FAO, FAO X2, FAOX4
  Feed_composition <- All_pred_levels$FCR_FCF$Feed_composition %>% unique()# %>% rev() # From no action to $200t price for carbon mitigation (Source: Lucas et al. 2007)
  Carbon_price <- All_pred_levels$GHG$C_price # Zero meaning no action to 3 very high action. 
  WUEinc <- All_pred_levels$Water$WPinc # This is coded as WPinc which stands for water productivity
  N_management <- All_pred_levels$Nitrogen$N_management # Based on levels in the datasets and UNEP GEO-6 and Zhang et al. (2015) recommendations (SDGs etc)
  P_management <- All_pred_levels$Phosphorus$P_management  # Based on levels in dataset and general FUE recommendations

  All_levels <- list(Population,Diet,Plant_kcal,Waste,Yield_level,Feed_efficiency,Feed_composition,Carbon_price,WUEinc,N_management,P_management) # Creating list with all predictor levels
  All_levels_char <- lapply(All_levels, as.character) # Converting factors to strings
  