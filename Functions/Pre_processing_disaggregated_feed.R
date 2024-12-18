#################### DATA FRAME PRE-PROCESSING ##################################

## Author: Michalis Hadjikakou, Deakin University
## Purpose: Carrying out necessary transformations/unit conversions to ensure dataset is fully harmonised

DF_preprocess <- function(data_raw, PB_matrix){ # Reading in raw data file
  
  mysum <- function(x){ifelse(all(is.na(x)),NA,sum(x,na.rm = TRUE))}  # Custom sum function to enable multiple NAs being equal to NA - adapted from: https://stat.ethz.ch/pipermail/r-help/2002-April/020797.html
  
  data_recoded <- data_raw %>% 
    dplyr::rename(Study =`Study abbreviation`,Scen_desc = `Scenario description`, ScenYear =`Scenario year (numeric)`,
           Scope = `Study scope`, Included = `Included in meta-model`,       
           CH4 = TotalDirectCH4, N2O = TotalDirectN2O, DirNonCO2 = `TotalNonCO2 (excludes burning)`,DirNonCO2LUC = `NonCO2+LUC`,DirIndGHG = LCA,
           Water = `Blue water withdrawals`, Green_water = `Green water consumption`, LUC = `LUC - Total`,
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
  
  DF <- DF %>% retype() # Uses command from hablar package
  
  sel_cols <- c("DirNonCO2LUC","DirNonCO2","DirIndGHG","LUC","Aqua_kcal","Ocean_kcal","Seafood_kcal",
                "FCR_rum_meat","FCR_mon_meat","FCR_dairy","FCR_eggs","FCR_dairy_eggs","FCR_pork","FCR_poultry",
                "FCR_ruminants","FCR_monogastrics","FCF_ruminants","FCF_monogastrics","FCF_pork","FCF_poultry",
                "FCR_aquaculture","FCF_aquaculture",
                "FCF_rum_meat","FCF_mon_meat","FCF_dairy","FCF_eggs","FCF_all") # Selecting columns to convert
  #DF %>% mutate_at(sel_cols,as.numeric) %>% str() # Ensuring GHG columns are numeric
  DF[sel_cols] <- sapply(DF[sel_cols],as.numeric)
  
  DF <- mutate(DF,DirNonCO2LUC = if_else(`GHG units`=="Mt CO2e/yr",DirNonCO2LUC *0.001,DirNonCO2LUC),
               #DirNonCO2 = if_else(`GHG units`=="Mt CO2e/yr",DirNonCO2 *0.001,DirNonCO2),
               DirIndGHG = if_else(`GHG units`=="Mt CO2e/yr",DirIndGHG *0.001,DirIndGHG),
               LUC = if_else(`GHG units`=="Mt CO2e/yr",LUC *0.001,LUC)) # Making necessary conversions# Setting PB limits - need to read this in as a data.frame
  
  DF$`GHG units` <- "GtCO2eq/yr" # All units harmonised
  
  # Population
  
  DF <- mutate(DF,Population = if_else(`Population (units)`=="million",Population*0.001,Population))# Making necessary conversions# Setting PB limits - need to read this in as a data.frame
  DF$`Population (units)`<- "billion" # All units harmonised
  
  # Conversion factors (kcal to kg from FAOSTAT FBS assuming 2010 base year)
  Rum_meat_CF = 0.784* 1532.004197 + (1-0.784) * 2124.338624 # Ruminant meat conversion factor assumes 0.784 share for beef vs. lamb (see FAOSTAT FBS 2010 Old methodology)
  Dairy_CF = 559
  Aqua_CF = 641.715503
  Eggs_CF = 1432.174888
  Pork_CF = 2829.276105
  Poultry_CF = 1419.731259
  
  # Tidying up and calculating composite variables
  
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
    mutate(Rum_meat_grass_feed = Population*Rum_meat_kcal*FCR_grass_rum_meat/Rum_meat_CF, # Composite variables (grass)
           Dairy_grass_feed = Population*Dairy_kcal*FCR_grass_dairy/Dairy_CF,# Composite variables (grass)
           Mon_grass_feed = Population*FCR_monogastrics*(1-FCF_monogastrics),# Pasture/scavenge feed for poultry/pigs
           Rum_meat_kcal_pop = Population*Rum_meat_kcal, # Composite variable (population X consumption)
           Dairy_kcal_pop = Population*Dairy_kcal, # Composite variable (population X consumption)
           Rum_kcal_pop = Rum_meat_kcal_pop + Dairy_kcal_pop,
           Non_rum_kcal_pop = Population*Non_rum_kcal, # Composite variable (population X consumption)
           Animal_kcal_pop = Rum_meat_kcal_pop + Dairy_kcal_pop + Non_rum_kcal_pop,
           Plant_kcal_pop = Population*Vegetal_kcal) %>% # Composite variable (population X consumption)
    mutate(Rum_meat_crop_feed = Population*(Rum_meat_kcal*FCR_FCF_rum_meat)/Rum_meat_CF, # Composite variable with FCR+FCF  # Remove water eventually from name
           Dairy_crop_feed = Population*(Dairy_kcal*FCR_FCF_dairy)/Dairy_CF, # Composite variable with FCR+FCF
           Mon_meat_crop_feed = Population*(Mon_meat_kcal*FCR_FCF_mon_meat), # Composite variable with FCR+FCF
           Eggs_crop_feed = Population*(Eggs_kcal*FCR_FCF_eggs)/Eggs_CF,# kcal_to_kg conversion for eggs
           Pork_crop_feed = Population*(Pork_kcal*FCR_FCF_pork)/Pork_CF,# kcal_to_kg conversion for pigmeat
           Poultry_crop_feed = Population*(Poultry_kcal*FCR_FCF_poultry)/Poultry_CF, # kcal_to_kg conversion for poultry
           Aqua_crop_feed = Population*(Aqua_kcal*FCR_FCF_aqua)/Aqua_CF, # kcal_to_kg conversion for seafood
           Mon_crop_feed = Population*Non_rum_kcal*FCR_FCF_monogastrics,# Composite variable with FCR+FCF
           Mon_feed_all = rowSums(across(c(Eggs_crop_feed, Pork_crop_feed,Poultry_crop_feed,Aqua_crop_feed)),na.rm=T),
           Rum_crop_feed = case_when(
             is.na(Dairy_crop_feed) ~ Rum_meat_crop_feed, # Adding up dairy and ruminant meat crop feed with appropriate conversion to physical units
             is.na(Rum_meat_crop_feed) ~ Dairy_crop_feed,
             TRUE ~ Dairy_crop_feed + Rum_meat_crop_feed),
           Rum_feed_grass = case_when(
             is.na(Dairy_grass_feed) ~ Rum_meat_grass_feed, # Adding up dairy and ruminant meat crop feed with appropriate conversion to physical units
             is.na(Rum_meat_grass_feed) ~ Dairy_grass_feed,
             TRUE ~ Dairy_grass_feed + Rum_meat_grass_feed),
           Rum_feed_all = Rum_crop_feed+Rum_feed_grass, # All ruminant feed (including grass)
           All_crop_feed = case_when(
             is.na(Rum_crop_feed) ~ Mon_feed_all, # Adding up ruminant and monogastric feed already converted to physical units
             is.na(Mon_feed_all) ~ Rum_crop_feed,
             TRUE ~ Rum_crop_feed + Mon_feed_all),
           All_feed_all = All_crop_feed + Rum_feed_grass,
           All_plant_food = Population*Vegetal_kcal,
           Rum_kcal_FCR = Rum_meat_kcal*FCR_rum_meat,
           Dairy_kcal_FCR = Dairy_kcal*FCR_dairy,
           Non_rum_kcal_FCR = Non_rum_kcal*FCR_monogastrics,
           Rum_meat_kcal_GHG = Population*(Rum_meat_kcal*FCR_rum_meat),
           Dairy_kcal_GHG = Population*Dairy_kcal*FCR_dairy,
           Non_rum_kcal_GHG = Population*Non_rum_kcal*FCR_monogastrics,
           Non_rum_dairy_kcal_GHG = Dairy_kcal_GHG + Non_rum_kcal_GHG) %>% 
    mutate(Pasture_yield = Rum_kcal_pop/Pasture) %>%  # Calculated the increase in yield relative to pasture
    mutate(All_crops = All_plant_food+(All_crop_feed*3500)) %>% # All crops for food and feed in kcal
    mutate(Crop_kcal_yield = All_crops/Cropland) %>% # Caloric yield for all crops (including feed)
    mutate(LNFV = Legumes_nuts + Fruit_veg,
           Other_plants = Vegetal_kcal - LNFV,
           LNFV_all = Population*LNFV,
           Other_plant_all = Population*Other_plants)
      
  return(DF)
}