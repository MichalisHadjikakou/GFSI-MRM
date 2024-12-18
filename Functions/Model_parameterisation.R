model_par <- function(ind){

  list_var <- rep(rep(list(list()),length(ind)))# List lists of selected variables (columns) for each model
  
  # Cropland
  list_var[[1]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","C_price") # Yield adjustments already encompass organic production
  list_var[[1]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF",
                          "Vegetal_kcal","Yield","C_price")
  list_var[[1]][[3]] <- c("All_crop_feed","All_plant_food","Yield")
  
  # Pasture 
  list_var[[2]][[1]] <- c("Population","Rum_meat_kcal","Dairy_kcal","FCR_FCF_rum_meat","FCR_FCF_dairy","C_price")
  list_var[[2]][[2]] <- c("Rum_meat_kcal_pop","Dairy_kcal_pop","FCR_grass_rum_meat","FCR_grass_dairy","FCR_grass_mon",
                          "C_price")
  list_var[[2]][[3]] <- c("Rum_feed_grass")
  
  # Water 
  list_var[[3]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","WUEinc")
  list_var[[3]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF","Vegetal_kcal","Yield","WUEinc")
  list_var[[3]][[3]] <- c("All_crop_feed","All_plant_food","Yield","WPinc")
  
  # CH4 emissions
  list_var[[4]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal", # n.removed = 30 percent.removed = 4.200323  - chosen model
                          "Vegetal_kcal","FCR_rum_meat","FCR_dairy","FCR_monogastrics","GHG_eff_CH4")
  list_var[[4]][[2]] <- c("Rum_meat_kcal_pop","Non_rum_kcal_pop","Dairy_kcal_pop", # Also a very good model
                          "Vegetal_kcal","FCR_rum_meat","FCR_dairy","FCR_monogastrics","GHG_eff_CH4")
  list_var[[4]][[3]] <- c("Rum_meat_kcal_GHG","Non_rum_dairy_kcal_GHG", # Assumption that FCR determines enteric fermentation and manure production and that composition impacts FCR
                          "All_plant_food","GHG_eff_CH4") 
  
  # N2O emissions
  list_var[[5]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","FCR_FCF_rum_meat","FCR_FCF_dairy","FCR_FCF_monogastrics","Yield","GHG_eff_N2O","Organic")
  list_var[[5]][[2]] <- c("Population","Rum_kcal_FCR","Non_rum_kcal_FCR","Dairy_kcal_FCR","Yield",
                          "Vegetal_kcal","GHG_eff_N2O","Organic")
  list_var[[5]][[3]] <- c("Rum_feed_grass","All_crop_feed","All_plant_food",
                          "Yield","GHG_eff_N2O") 
  
  # Nitrogen fertiliser - PB
  list_var[[6]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal",
                          "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","Organic","NUEinc","NrecHousehold") # Yield adjustments already encompass organic production
  list_var[[6]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF",
                          "Vegetal_kcal","Yield","NUEinc","NrecHousehold","Organic")
  list_var[[6]][[3]] <- c("All_crop_feed","All_plant_food", 
                          "Yield","NUEinc")
  
  # Phosphorus fertiliser - PB
  list_var[[7]][[1]] <- c("Population","Rum_meat_kcal","Non_rum_kcal","Dairy_kcal", # Significant colinearity between Yield and population (VIF>10)
                           "Vegetal_kcal","Yield","FCR_FCF_ruminants","FCR_FCF_monogastrics","PUEinc","PrecHousehold") # Yield adjustments already encompass organic production
  list_var[[7]][[2]] <- c("Population","Rum_meat_kcal_FCR_FCF","Non_rum_kcal_FCR_FCF","Dairy_kcal_FCR_FCF",
                           "Vegetal_kcal","Yield","PUEinc","PrecHousehold")
  list_var[[7]][[3]] <- c("All_crop_feed","All_plant_food",
                          "Yield","PUEinc") 

  return(list_var)

}