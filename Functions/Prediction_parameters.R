##################################################
## FUTURE FOOD SCENARIOS - Indicator-specific prediction dataset ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Last updated: 26 April 2022
# Purpose: Generate synthetic dataset for predictions across all indicators

Pred_data_indicator <- function(Pred_data,i){
  
  pred_list <- c("Population","Diet","Yield","FCR_FCF") # Universal list of predictors across all models
  
  if (i==1|i==2){ # Land-system change indicators
    
    pred_list <-  pred_list
     
  } else if (i==3){ # Freshwater use
    
    pred_list <-  c(pred_list,"Water")
    
  } else if (i==4|i==5){ # Climate change 
    
    pred_list <-  c(pred_list,"GHG","Organic")
    
  } else if (i==6|i==7){ # Biogeochemical flows 
    
    pred_list <-  c(pred_list,"Nitrogen")#"Organic")
    
  } else if (i==8|i==9){ # Biogeochemical flows 
    
    pred_list <-  c(pred_list,"Phosphorus")#,"Organic")
    
  }
  
  list_sel <- Pred_data[pred_list] # Selecting only relevant predictors

  pred_colnames <- sapply(seq_along(pred_list),function(x) list_sel[[x]] %>%
                         colnames()) %>%
    unlist()
  
  df_pred <- do.call(tidyr::expand_grid,list_sel) %>% # Building prediction data.frame with all scenario settings
    as.matrix.data.frame() %>%
    magrittr::set_colnames(pred_colnames) %>% # Renaming column names to ensure the right names
    as.data.frame() %>% 
    retype() %>% 
    dplyr::mutate(Rum_meat_kcal_FCR_FCF = ((Rum_meat_kcal+1)*(FCR_FCF_rum_meat+1))-1,
           Non_rum_kcal_FCR_FCF = ((Non_rum_kcal+1)*(FCR_FCF_monogastrics+1))-1,
           Dairy_kcal_FCR_FCF = ((Dairy_kcal+1)*(FCR_FCF_dairy+1))-1) %>% 
    dplyr::mutate(Rum_meat_grass_feed = ((Population+1)*(Rum_meat_kcal+1)*(FCR_grass_rum_meat+1))-1,
           Dairy_grass_feed = ((Population+1)*(Dairy_kcal+1)*(FCR_grass_dairy+1))-1,
           Mon_grass_feed = ((Population+1)*(Non_rum_kcal+1)*(FCR_grass_monogastrics+1))-1,
           Rum_feed_grass = (Rum_meat_grass_feed*1.86/(1.86+0.58)) + (Dairy_grass_feed*0.58/(1.86+0.58))) %>% 
    dplyr::mutate(Rum_meat_crop_feed_water = ((Population+1)*(Rum_meat_kcal+1)*(FCR_FCF_rum_meat+1))-1, # Composite variable with FCR+FCF
           Dairy_crop_feed_water = ((Population+1)*(Dairy_kcal+1)*(FCR_FCF_dairy+1))-1,
           Rum_feed_water = (Rum_meat_crop_feed_water*1.86/(1.86+0.58)) + (Dairy_crop_feed_water*0.58/(1.86+0.58)), # Weighted to reflect higher demands for ruminant meat
           Mon_feed_water = ((Population+1)*(Non_rum_kcal+1)*(FCR_FCF_monogastrics+1))-1,
           All_feed_water = (Rum_meat_crop_feed_water*1.86/(1.86+0.58+2.00)) + (Dairy_crop_feed_water*0.58/(1.86+0.58+2.00)) + (Mon_feed_water*2.00/(1.86+0.58+2.00)),# Weighted to reflect higher demands for ruminant meat
           Plant_food_water =((Population+1)*(Vegetal_kcal+1))-1) %>% 
    dplyr::mutate(Rum_meat_kcal_GHG = ((Population+1)*(Rum_meat_kcal+1)*(FCR_rum_meat+1))-1, # Composite variable with FCR+FCF
           Dairy_kcal_GHG = ((Population+1)*(Dairy_kcal+1)*(FCR_dairy+1))-1,
           Non_rum_kcal_GHG = ((Population+1)*(Non_rum_kcal+1)*(FCR_monogastrics+1))-1,
           Plant_kcal_GHG =((Population+1)*(Vegetal_kcal+1))-1)
  
  return(df_pred)
  
}

