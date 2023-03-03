#################### DATA FRAME PRE-PROCESSING ##################################

## Last updated: 210815
## Author: Michalis Hadjikakou, Deakin University
## Purpose: Carrying out necessary transformations/unit conversions to ensure dataset is fully harmonised

DF_preprocess <- function(data_raw, PB_matrix){ # Reading in raw data file
  
  mysum <- function(x){ifelse(all(is.na(x)),NA,sum(x,na.rm = TRUE))}  # Custom sum function to enable multiple NAs being equal to NA - adapted from: https://stat.ethz.ch/pipermail/r-help/2002-April/020797.html
  
  data_recoded <- data_raw %>% 
    dplyr::rename(Study =`Study abbreviation`,Scen_desc = `Scenario description`, ScenYear =`Scenario year (numeric)`,
           Scope = `Study scope`, Included = `Included in meta-model`,       
           CH4 = TotalDirectCH4, N2O = TotalDirectN2O, DirNonCO2 = `TotalNonCO2 (excludes burning)`,DirNonCO2LUC = `NonCO2+LUC`,DirIndGHG = LCA,
           Water = `Blue water`, Green_water = `Green water`, LUC = `LUC - Total`,
           Population = `World population`, Total_kcal = `Total food supply`, Animal_kcal = `Animal products (total)`,
           Vegetal_kcal = `Plant products`, Rum_meat_kcal = `Ruminant meat`, Mon_meat_kcal = `Non-ruminant meat`,
           Aqua_kcal = Aquaculture, Ocean_kcal = `Capture seafood`, Seafood_kcal = `Total seafood`, Poultry_kcal = Poultry,
           Dairy_eggs_kcal = `Dairy & eggs combined`,Dairy_kcal = Dairy, Eggs_kcal = Eggs, Pork_kcal = Pork, 
           Animal_kcal = `Animal products (total)`,Yield_cereal = `Cereal yield - average`,
           Yield_all = `All crops yields`, FCR_rum_meat = `Ruminant meat FCR`, FCR_ruminants = `Ruminant FCR (meat & dairy)`,
           FCR_mon_meat = `Non-ruminant meat FCR`, FCR_monogastrics = `Non-ruminant FCR (meat & eggs)`, FCR_pork = `Pork FCR`,
           FCR_dairy = `Dairy FCR`, FCR_eggs = `Eggs FCR`,FCF_dairy = `FCF dairy`,FCF_eggs = `FCF egg`, 
           FCR_poultry = `Chicken FCR`, FCR_aquaculture = `Aquaculture FCR`, FCF_aquaculture = `FCF aquaculture`,
           FCF_ruminants = `FCF All ruminants`,FCF_monogastrics = `FCF All non-ruminants`, FCF_pork = `FCF pork`, FCF_poultry = `FCF chicken`,
           FCR_dairy_eggs = `Dairy & Eggs FCR`,FCF_rum_meat = `FCF ruminant meat`, FCF_mon_meat = `FCF non-ruminant meat`,FCF_all = `FCF overall`,
           C_price = `Carbon price ($US 2010/tCO2eq)`, GHG_eff_CH4 = `GHGefficiency-CH4`,GHG_eff_N2O = `GHGefficiency-N2O`, GHG_eff_CO2 = `GHGefficiency-CO2`,
           GHG_eff_all = GHGeffinc, Scen_family = `Scenario family`, Scen_name = `Scenario name`, Int_family = `Intervention family`,
           Legumes_nuts = `Legumes & nuts`,Fruit_veg = `Fruit & vegetables`)
  
  # Nitrogen
  
  DF <- mutate(data_recoded,Nfert = if_else(`Nitrogen units`=="kg N cap-1 yr-1",Nfert*Population,Nfert),
               Nsurplus = if_else(`Nitrogen units`=="kg N cap-1 yr-1",Nsurplus*Population,Nsurplus)) # Making necessary conversions
  DF$`Nitrogen units` <- "Tg N/yr" # All units harmonised
  
  # Phosphorus
  
  DF <- mutate(DF,Pfert = if_else(`Phosphorus units`=="Mt P2O5/yr",Pfert*0.4364,Pfert),
               Psurplus = if_else(`Phosphorus units`=="Mt P2O5/yr",Psurplus*0.4364,Psurplus)) # Making necessary conversion
  DF$`Phosphorus units` <- "Tg P/yr" # All units harmonised
  
  # Land use
  DF$Cropland<-as.numeric(DF$Cropland) # Ensuring variable is numeric
  DF$Pasture.<-as.numeric(DF$Pasture) # Ensuring variable is numeric
  DF$TotalArea<-as.numeric(DF$TotalArea) # Ensuring variable is numeric
  DF$Forest <- as.numeric(DF$Forest) # Ensuring variable is numeric
  
  DF <- mutate(DF,Cropland = if_else(`Land units`=="ha cap-1 yr-1",Cropland*Population,Cropland),
               Pasture = if_else(`Land units`=="ha cap-1 yr-1",Pasture*Population,Pasture),
               TotalArea= if_else(`Land units`=="ha cap-1 yr-1",TotalArea*Population,TotalArea)) # Making necessary conversions
  
  DF <- mutate(DF,Cropland = if_else(`Land units`=="billion ha",Cropland*1000,Cropland),
               Pasture = if_else(`Land units`=="billion ha",Pasture*1000,Pasture),
               TotalArea= if_else(`Land units`=="billion ha",TotalArea*1000,TotalArea)) # Making necessary conversions
  
  DF <- mutate(DF,Cropland = if_else(`Land units`=="Mkm2",Cropland*100,Cropland),
               Pasture = if_else(`Land units`=="Mkm2",Pasture.*100,Pasture),
               TotalArea= if_else(`Land units`=="Mkm2",TotalArea*100,TotalArea)) # Making necessary conversions# Setting PB limits - need to read this in as a data.frame
  DF$`Land units` <- "Mha" # All units harmonised
    
  DF$`Water units` <- "km3/yr water consumption" # All units harmonised
  
  # is_all_numeric <- function(x) { # see: https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r
  #   !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
  # }
  # DF %>% 
  #   mutate_if(is_all_numeric,as.numeric) %>%
  #   str()
  # Ensuring that all eligible columns are turned to numeric
  
  DF <- DF %>% retype() # Uses command from hablar package
  
  sel_cols <- c("DirNonCO2LUC","DirNonCO2","DirIndGHG","LUC","Aqua_kcal","Ocean_kcal","Seafood_kcal",
                "FCR_rum_meat","FCR_mon_meat","FCR_dairy","FCR_eggs","FCR_dairy_eggs","FCR_pork","FCR_poultry",
                "FCR_ruminants","FCR_monogastrics","FCF_ruminants","FCF_monogastrics","FCF_pork","FCF_poultry",
                "FCR_aquaculture","FCF_aquaculture",
                "FCF_rum_meat","FCF_mon_meat","FCF_dairy","FCF_eggs","FCF_all") # Selecting columns to convert
  #DF %>% mutate_at(sel_cols,as.numeric) %>% str() # Ensuring GHG columns are numeric
  DF[sel_cols] <- sapply(DF[sel_cols],as.numeric)
  
  # Greenhouse gas emissions - Different scopes now in seperate columns - unit harmonisation required for each column 
  
  # DF <- DF %>% mutate(across(sel_cols[1:4],# Selecting only GHG emissions and ensuring right units
  #                     if_else(`GHG units`=="Mt CO2e/yr",~.*0.001,~.)))
  
  DF <- mutate(DF,DirNonCO2LUC = if_else(`GHG units`=="Mt CO2e/yr",DirNonCO2LUC *0.001,DirNonCO2LUC),
               #DirNonCO2 = if_else(`GHG units`=="Mt CO2e/yr",DirNonCO2 *0.001,DirNonCO2),
               DirIndGHG = if_else(`GHG units`=="Mt CO2e/yr",DirIndGHG *0.001,DirIndGHG),
               LUC = if_else(`GHG units`=="Mt CO2e/yr",LUC *0.001,LUC)) # Making necessary conversions# Setting PB limits - need to read this in as a data.frame
  
  DF$`GHG units` <- "GtCO2eq/yr" # All units harmonised
  

  #DF <- mutate(DF,GHG...total = replace(GHG...total,GHG...total<0,NA)) # Removing any negative values
  
  # Population
  
  DF <- mutate(DF,Population = if_else(`Population (units)`=="million",Population*0.001,Population))# Making necessary conversions# Setting PB limits - need to read this in as a data.frame
  DF$`Population (units)`<- "billion" # All units harmonised
  
  # Caloric intake and yield adjustments
  
  DF <- DF %>% 
    #rowwise() %>% 
    mutate(Non_rum_meat_kcal = rowSums(dplyr::select(., Mon_meat_kcal,Aqua_kcal), na.rm = TRUE)) %>% 
    mutate(Non_rum_kcal = rowSums(dplyr::select(., Non_rum_meat_kcal,Eggs_kcal), na.rm = TRUE)) %>% 
    mutate(C_price = replace_na(C_price,0)) %>% 
    #mutate(Non_rum_meat_kcal = sum(Mon_meat_kcal:Aqua_kcal), na.rm = TRUE) %>% # Adding up aquaculture and non ruminant meats (as one category)
    mutate(Yield = if_else(is.na(Yield_cereal),Yield_all,Yield_cereal)) %>% 
    mutate(FCR_FCF_rum_meat = FCR_rum_meat*FCF_rum_meat,FCR_FCF_mon_meat = FCR_mon_meat*FCF_mon_meat,
           FCR_FCF_dairy = FCR_dairy*FCF_dairy, FCR_FCF_eggs = FCR_eggs*FCF_eggs,
           FCR_FCF_pork = FCR_pork*FCF_pork, FCR_FCF_poultry = FCR_poultry*FCF_poultry,
           FCR_FCF_aqua = FCR_aquaculture*FCF_aquaculture,
           FCR_FCF_ruminants = FCR_ruminants*FCF_ruminants,
           FCR_FCF_monogastrics = FCR_monogastrics*FCF_monogastrics,
           Rum_meat_kcal_FCR_FCF = Rum_meat_kcal*FCR_FCF_rum_meat,
           Non_rum_kcal_FCR_FCF = Non_rum_kcal*FCR_FCF_monogastrics,
           Dairy_kcal_FCR_FCF = Dairy_kcal*FCR_FCF_dairy)%>% # Calculating FCRs for food-competing crops
    mutate(FCR_grass_rum_meat = FCR_rum_meat*(1-FCF_rum_meat),# Grass FCRs for ruminant meat and dairy
           FCR_grass_dairy = FCR_dairy*(1-FCF_dairy),
           FCR_grass_mon = FCR_monogastrics*(1-FCF_monogastrics)) %>% # Grass FCRs for ruminant meat and dairy
    mutate(Rum_meat_grass_feed = Population*Rum_meat_kcal*FCR_grass_rum_meat, # Composite variables (grass)
           Dairy_grass_feed = Population*Dairy_kcal*FCR_grass_dairy,# Composite variables (grass)
           Mon_grass_feed = Population*FCR_monogastrics*(1-FCF_monogastrics),# Pasture/scavenge feed for poultry/pigs
           Rum_meat_kcal_pop = Population*Rum_meat_kcal, # Composite variable (population X consumption)
           Dairy_kcal_pop = Population*Dairy_kcal, # Composite variable (population X consumption)
           Rum_kcal_pop = Rum_meat_kcal_pop + Dairy_kcal_pop,
           Non_rum_kcal_pop = Population*Non_rum_kcal, # Composite variable (population X consumption)
           Animal_kcal_pop = Rum_meat_kcal_pop + Dairy_kcal_pop + Non_rum_kcal_pop,
           Plant_kcal_pop = Population*Vegetal_kcal) %>% # Composite variable (population X consumption)
    #mutate(Rum_meat_crop_feed = Population*(Rum_meat_kcal*FCR_FCF_rum_meat)/Yield, # Composite variable with yields
    #       Dairy_crop_feed = Population*(Dairy_kcal*FCR_FCF_dairy)/Yield, # Composite variable with yields
    #       Mon_meat_feed = Population*(Mon_meat_kcal*FCR_FCF_mon_meat)/Yield, # Composite variable with yields
    #       Eggs_feed = Population*(Eggs_kcal*FCR_FCF_eggs)/Yield, # Composite variable with yields
    #       Mon_feed = (Population*(Non_rum_kcal*FCR_FCF_monogastrics)/Yield),  # Composite variable with yields
    #       Plant_food = Population*Vegetal_kcal/Yield) %>% 
    mutate(Rum_meat_crop_feed_water = Population*(Rum_meat_kcal*FCR_FCF_rum_meat), # Composite variable with FCR+FCF  # Remove water eventually from name
           Dairy_crop_feed_water = Population*(Dairy_kcal*FCR_FCF_dairy), # Composite variable with FCR+FCF
           Mon_meat_feed_water = Population*(Mon_meat_kcal*FCR_FCF_mon_meat), # Composite variable with FCR+FCF
           Eggs_feed_water = Population*(Eggs_kcal*FCR_FCF_eggs)*1.432,# Composite variable with FCR+FCF
           Pork_feed_water = Population*(Pork_kcal*FCR_FCF_pork)*2.83,
           Poultry_feed_water = Population*(Poultry_kcal*FCR_FCF_poultry)*1.42,
           Aqua_feed_water = Population*(Aqua_kcal*FCR_FCF_aqua)*0.64,
           Mon_feed_water = Population*Non_rum_kcal*FCR_FCF_monogastrics,# Composite variable with FCR+FCF
           Mon_feed_all = rowSums(across(c(Eggs_feed_water, Pork_feed_water,Poultry_feed_water,Aqua_feed_water)),na.rm=T),
           Rum_feed_water = case_when(
             is.na(Dairy_crop_feed_water) ~ Rum_meat_crop_feed_water*1.86, # Adding up dairy and ruminant meat crop feed with appropriate conversion to physical units
             is.na(Rum_meat_crop_feed_water) ~ Dairy_crop_feed_water*0.58,
             TRUE ~ Dairy_crop_feed_water*0.58 + Rum_meat_crop_feed_water*1.86),
           Rum_feed_grass = case_when(
             is.na(Dairy_grass_feed) ~ Rum_meat_grass_feed*1.86, # Adding up dairy and ruminant meat crop feed with appropriate conversion to physical units
             is.na(Rum_meat_grass_feed) ~ Dairy_grass_feed*0.58,
             TRUE ~ Dairy_grass_feed*0.58 + Rum_meat_grass_feed*1.86),
           #Rum_feed_water = Population*(Dairy_kcal*0.58*FCR_FCF_dairy) + (Population*(Rum_meat_kcal*1.86*FCR_FCF_rum_meat)), # Weighted in physical units
           All_feed_water = case_when(
             is.na(Rum_feed_water) ~ Mon_feed_all, # Adding up ruminant and monogastric feed already converted to physical units
             is.na(Mon_feed_all) ~ Rum_feed_water,
             TRUE ~ Rum_feed_water + Mon_feed_all),
           # Population*(Mon_meat_kcal*2.00*FCR_FCF_mon_meat))),
           Plant_food_water = Population*Vegetal_kcal,
           Rum_kcal_FCR = Rum_meat_kcal*FCR_rum_meat,
           Dairy_kcal_FCR = Dairy_kcal*FCR_dairy,
           Non_rum_kcal_FCR = Non_rum_kcal*FCR_monogastrics,
           Rum_meat_kcal_GHG = Population*(Rum_meat_kcal*FCR_rum_meat),
           Dairy_kcal_GHG = Population*Dairy_kcal*FCR_dairy,
           Non_rum_kcal_GHG = Population*Non_rum_kcal*FCR_monogastrics,
           Plant_kcal_GHG = Population*Vegetal_kcal) %>% 
    mutate(LNFV = Legumes_nuts + Fruit_veg,
           Other_plants = Vegetal_kcal - LNFV,
           LNFV_water = Population*LNFV,
           Other_plant_water = Population*Other_plants)
      
  return(DF)
}