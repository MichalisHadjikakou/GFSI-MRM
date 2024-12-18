##################################################
## FUTURE FOOD SCENARIOS - Indicator-specific prediction dataset ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Generate synthetic dataset for predictions across all indicators

Pred_data_indicator <- function(Pred_data,i){
  
  pred_list <- c("Population","Diet","Yield","FCR_FCF") # Universal list of predictors across all models
  
  if (i==1|i==2){ # Land-system change indicators
    
    pred_list <-  pred_list
     
  } else if (i==3){ # Freshwater use
    
    pred_list <-  c(pred_list,"Water")
    
  } else if (i==4|i==5){ # Climate change 
    
    pred_list <-  c(pred_list,"GHG","Organic")
    
  } else if (i==6){ # Biogeochemical flows 
    
    pred_list <-  c(pred_list,"Nitrogen")#"Organic")
    
  } else if (i==7){ # Biogeochemical flows 
    
    pred_list <-  c(pred_list,"Phosphorus")#,"Organic")
    
  }
  
  list_sel <- Pred_data[pred_list] # Selecting only relevant predictors

  pred_colnames <- sapply(seq_along(pred_list),function(x) list_sel[[x]] %>%
                         colnames()) %>%
    unlist()
  
  
## Starting weights (2010) based on physical quantities (source: FAOSTAT) for different livestock/feed classes - population not multiplier through since it's the same number across all livestock/feed classes
  
  # Constants
  Rum_meat_conv = 0.784* 1532.004197 + (1-0.784) * 2124.338624 # Ruminant meat conversion factor assumes 0.784 share for beef vs. lamb (see FAOSTAT FBS 2010 Old methodology)
  Mon_conv = 0.1352*641.715503 +  0.143442623*1432.174888 + 0.495901639*2829.276105 +  0.225409836*1419.731259 # Accounts for different shares of seafood, eggs, pork and poultry
  
  # Grass
  Rum_meat_grass_w <- 51*1/Rum_meat_conv*35*0.95 # (kcal/cap/d * kcal_to_kg_conv * FCR * (1-FCF)) 
  Dairy_grass_w <- 136*1/559*2*0.85 # (kcal/cap/d * kcal_to_kg_conv * FCR * (1-FCF))
  Rum_feed_grass_tw <- Rum_meat_grass_w + Dairy_grass_w
  Rum_meat_perc <- Rum_meat_grass_w/Rum_feed_grass_tw
  Dairy_perc <- Dairy_grass_w/Rum_feed_grass_tw
    
  # Crops
  Rum_meat_crop_w <- 51*1/Rum_meat_conv*35*0.05 # (kcal/cap/d * kcal_to_kg_conv * FCR * (1-FCF))
  Dairy_crop_w <- 136*1/559*2*0.15 # (kcal/cap/d * kcal_to_kg_conv * FCR * (1-FCF))
  Rum_feed_crop_tw <- Rum_meat_crop_w + Dairy_crop_w
  Mon_crop_w <- 244*1/Mon_conv*4*0.8 # Assumes average contribution from pork/eggs/chicken/seafood to control for any future changes
  All_feed_crop_tw <- Rum_meat_crop_w + Dairy_crop_w + Mon_crop_w
  
  # Combined (crops + grass)
  All_mon_feed_tw  <- Mon_crop_w + 244*1/Mon_conv*4*0.2
  All_dairy_feed_tw <- Dairy_grass_w + Dairy_crop_w
  Non_rum_dairy_feed_tw <- All_mon_feed_tw + All_dairy_feed_tw
  All_rum_feed_tw <- Rum_feed_crop_tw + Rum_feed_grass_tw
  
  # Binding everything together in one single prediction dataframe
  
  df_pred <- do.call(tidyr::expand_grid,list_sel) %>% # Building prediction data.frame with all scenario settings
    as.matrix.data.frame() %>%
    magrittr::set_colnames(pred_colnames) %>% # Renaming column names to ensure the right names
    as.data.frame() %>% 
    retype() %>% 
    dplyr::mutate(Rum_meat_kcal_FCR_FCF = ((Rum_meat_kcal+1)*(FCR_FCF_rum_meat+1))-1, # Final % change is calculated as product multiplier - 1
           Non_rum_kcal_FCR_FCF = ((Non_rum_kcal+1)*(FCR_FCF_monogastrics+1))-1,
           Dairy_kcal_FCR_FCF = ((Dairy_kcal+1)*(FCR_FCF_dairy+1))-1) %>% 
    dplyr::mutate(Rum_meat_grass_feed = ((Population+1)*(Rum_meat_kcal+1)*(FCR_grass_rum_meat+1))-1,
           Dairy_grass_feed = ((Population+1)*(Dairy_kcal+1)*(FCR_grass_dairy+1))-1,
           Mon_grass_feed = ((Population+1)*(Non_rum_kcal+1)*(FCR_grass_monogastrics+1))-1,
           Rum_feed_grass = (Rum_meat_grass_feed*Rum_meat_perc) + (Dairy_grass_feed*Dairy_perc)) %>% 
    dplyr::mutate(Rum_meat_crop_feed = ((Population+1)*(Rum_meat_kcal+1)*(FCR_FCF_rum_meat+1))-1, # Composite variable with FCR+FCF
           Dairy_crop_feed = ((Population+1)*(Dairy_kcal+1)*(FCR_FCF_dairy+1))-1,
           Rum_crop_feed = (Rum_meat_crop_feed*Rum_meat_crop_w/Rum_feed_crop_tw) + (Dairy_crop_feed*Dairy_crop_w/Rum_feed_crop_tw), # Weighted to reflect higher demands for ruminant meat
           Mon_crop_feed = ((Population+1)*(Non_rum_kcal+1)*(FCR_FCF_monogastrics+1))-1,
           All_crop_feed = (Rum_meat_crop_feed*Rum_meat_crop_w/All_feed_crop_tw) + 
             (Dairy_crop_feed*Dairy_crop_w/All_feed_crop_tw) + 
             (Mon_crop_feed*Mon_crop_w/All_feed_crop_tw),# Weighted to reflect higher demands for ruminant meat
           Rum_feed_all = Rum_feed_grass*Rum_feed_grass_tw/All_rum_feed_tw + Rum_crop_feed*Rum_feed_crop_tw/All_rum_feed_tw,
           All_plant_food =((Population+1)*(Vegetal_kcal+1))-1) %>% 
    dplyr::mutate(Rum_meat_kcal_GHG = ((Population+1)*(Rum_meat_kcal+1)*(FCR_rum_meat+1))-1, # Composite variable with FCR+FCF
           Dairy_kcal_GHG = ((Population+1)*(Dairy_kcal+1)*(FCR_dairy+1))-1,
           Non_rum_kcal_GHG = ((Population+1)*(Non_rum_kcal+1)*(FCR_monogastrics+1))-1,
           Non_rum_dairy_kcal_GHG = (Dairy_kcal_GHG*All_dairy_feed_tw/Non_rum_dairy_feed_tw) + 
             (Non_rum_kcal_GHG*All_mon_feed_tw/Non_rum_dairy_feed_tw),
           Plant_kcal_GHG =((Population+1)*(Vegetal_kcal+1))-1)
  
  return(df_pred)
  
}

