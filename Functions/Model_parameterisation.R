model_par <- function(ind){

  list_var <- rep(rep(list(list()),length(ind)))# List lists of selected variables (columns) for each model
  
  # Cropland - Hybrid chosen - remove residuals + Roos study (uncertain base year estimates)
  list_var[[1]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","C_price") # Yield adjustments already encompass organic production
  list_var[[1]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF",
                          "Vegetal_kcal","Yield","C_price")
  list_var[[1]][[3]] <- c("Rum_meat_crop_feed_water",'Dairy_crop_feed_water',"Mon_feed_water","Plant_food_water","Yield")# C_price not significant
  #list_var[[1]][[3]] <- c("All_feed_water","Plant_food_water","Yield")# aggregation reduced RMSE
  
  # Pasture - Process-based chosen
  list_var[[2]][[1]] <- c("Population","Rum_meat_kcal","Dairy_kcal","FCR_FCF_rum_meat","FCR_FCF_dairy","C_price")
  list_var[[2]][[2]] <- c("Rum_meat_kcal_pop","Dairy_kcal_pop","FCR_grass_rum_meat","FCR_grass_dairy","FCR_grass_mon",
                          "C_price")
  #list_var[[2]][[3]] <- c("Rum_meat_grass_feed","Dairy_grass_feed")#,"C_price") # Chosen model - will benefit from more data n.removed = 9 percent.removed = 2.564103 
  list_var[[2]][[3]] <- c("Rum_feed_grass")
  
  # Water - Process-based the best option for Water - with trimmed residuals (n.removed=17, %removed 2.32)
  list_var[[3]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","WUEinc")
  list_var[[3]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF","Vegetal_kcal","Yield","WUEinc")
  #list_var[[3]][[3]] <- c("Rum_feed_water","Mon_feed_water","Plant_food_water","Yield","WPinc") # High VIFs suggest colinearity between rum and mon feed
  list_var[[3]][[3]] <- c("All_feed_water","Plant_food_water","Yield","WPinc")
  #list_var[[3]][[3]] <- c("All_feed_water","LNFV_water","Other_plant_water","Yield","WPinc")
  
  # CH4 emissions
  list_var[[4]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal", # n.removed = 30 percent.removed = 4.200323  - chosen model
                          "Vegetal_kcal","FCR_rum_meat","FCR_dairy","FCR_monogastrics","GHG_eff_CH4")
  list_var[[4]][[2]] <- c("Rum_meat_kcal_pop","Non_rum_kcal_pop","Dairy_kcal_pop", # Also a very good model
                          "Vegetal_kcal","FCR_rum_meat","FCR_dairy","FCR_monogastrics","GHG_eff_CH4")
  list_var[[4]][[3]] <- c("Rum_meat_kcal_GHG","Dairy_kcal_GHG","Non_rum_kcal_GHG", # Assumption that FCR determines enteric fermentation and manure production and that composition impacts FCR
                          #"Rum_meat_grass_feed","Dairy_grass_feed",
                          #"Rum_feed_grass", # All ruminant feed modeled as single predictor for parsimony and reduces colinearity issues
                          #"Rum_feed_water",
                          #"Mon_feed_water",
                          #"All_feed_water", # All feedlot/crop feed from all animals summed up correlates to methane from manure management
                          "Plant_kcal_GHG","Yield","GHG_eff_CH4") 
  # Separate feed for ruminants and monogastrics 
  
  # N2O emissions
  list_var[[5]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","FCR_FCF_rum_meat","FCR_FCF_dairy","FCR_FCF_monogastrics","Yield","GHG_eff_N2O","Organic")
  list_var[[5]][[2]] <- c("Population","Rum_kcal_FCR","Non_rum_kcal_FCR","Dairy_kcal_FCR","Yield",
                          "Vegetal_kcal","GHG_eff_N2O","Organic")
  list_var[[5]][[3]] <- c(#"Rum_meat_kcal_GHG","Dairy_kcal_GHG",#"Non_rum_kcal_GHG",
                          "Rum_feed_grass",
                          "All_feed_water",
                          "Plant_kcal_GHG",
                          "Yield","GHG_eff_N2O","Organic") # Chosen model (1+Yield/Model) with one residual trim and without excluding any studies, n.removed = 17 
  #percent.removed = 2.746365 
  # Also tested the following:
  #"Rum_meat_kcal_pop","Dairy_kcal_pop","Non_rum_kcal_pop",
  #"Rum_meat_grass_feed","Dairy_grass_feed",#"Rum_feed_grass",#"Mon_feed_all",
  
  # Nitrogen fertiliser - PB
  list_var[[6]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","Organic","NUEinc","NrecHousehold") # Yield adjustments already encompass organic production
  list_var[[6]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF",
                          "Vegetal_kcal","Yield","NUEinc","NrecHousehold","Organic")
  #list_var[[6]][[3]] <- c("Rum_feed_water","Mon_feed_water","Plant_food_water", # Collapsed all ruminant feed to one predictor to prevent colinearity
  #                        "Yield","NUEinc","NrecHousehold")# C_price not significant  - chosen model (not trimming of residuals)
  list_var[[6]][[3]] <- c("Rum_feed_water","Mon_feed_water",
                          "Plant_food_water", # Collapsed all ruminant feed to one predictor to prevent colinearity
                          "Yield","NUEinc","NrecHousehold")# N recycling not considered in many models
  
  # Nitrogen surplus - PB 
  list_var[[7]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","NUEinc","NrecHousehold") # Yield adjustments already encompass organic production
  list_var[[7]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF",
                          "Vegetal_kcal","Yield","NUEinc","NrecHousehold")
  list_var[[7]][[3]] <- c("Rum_feed_all","Mon_feed_water","Plant_food_water",#"Rum_feed_grass causes serious co-linearity issues
                          "Yield","NUEinc")# Adding grass feed to capture surplus from manure and fertriliser applied to grasslands
  
  # Phosphorus fertiliser - PB
  list_var[[8]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal", # Significant colinearity between Yield and population (VIF>10)
                           "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","PUEinc","PrecHousehold") # Yield adjustments already encompass organic production
  list_var[[8]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF",
                           "Vegetal_kcal","Yield","PUEinc","PrecHousehold")
  list_var[[8]][[3]] <- c("All_feed_water","Plant_food_water","Yield", # Colinearity remains but does not pose problems - residuals OK - no trimming
                          "PUEinc","PrecHousehold") # Aggregated feed due to low sample size and colinearity
  
  # Phosphorus surplus - PB
  list_var[[9]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                           "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","Organic","PUEinc","PrecHousehold") # Yield adjustments already encompass organic production
  list_var[[9]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF",
                           "Vegetal_kcal","Yield","PUEinc","PrecHousehold","Organic")
  list_var[[9]][[3]] <- c("Rum_feed_grass","All_feed_water","Plant_food_water",
                          "Yield","PUEinc") # Adding grass feed to capture surplus from manure and fertriliser applied to grasslands
  

  return(list_var)

}