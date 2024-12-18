##################################################
## FUTURE FOOD SCENARIOS - Universal prediction data ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Generate synthetic dataset for predictions across all indicators

Prediction_dataset <- function(Base_year,Scen_year,levels){

  ## 1. Initialising function
  Area <- "World" # Only set up for global projections
  Food_element <- "Food supply (kcal/capita/day)" # kcal form the basis - can be changed to kg or other unit of choice
  Yield_element <- "All cereals" # Cereals used as proxy for yield growth
  Yield_target <- "Attainable yield (plateau)" # Assumes damping of trends
  Diet_simplified = "yes" # Change to 'no' to consider every single possible diet combination (computationally very slow)
  
  # 1.2 Loading files used for base year estimates
  FBS <- read.csv("Input_data/Scenario_settings/FAOSTAT_FBS_1960_2013_World.csv") %>% 
    dplyr::filter(Year==Base_year & Element==Food_element)
  # Downloaded from http://www.fao.org/faostat/en/#data/FBSH (Selecting World only to reduce size)
  
  ## 2. Defining scenario parameters (4 levels of ambition Low-V.High across each predictor/driver)
  
  # 2.1 Socioeconomic variables - loading all datasets
  
  # Loading SSP projections (source: Wittgenstein Centre for Demography and Human Capital Data Explorer, https://guyabel.github.io/wcde/index.html)
  pop_SSPs <- get_wcde(indicator = "pop",scenario = 1:5) %>% 
    dplyr::filter(name==Area,year==Scen_year) %>% 
    dplyr::mutate(pop = pop/1e6)
  
  # Reading in sheet with historical and future population from UN WPP 2024 (POP/DB/WPP/Rev.2024/GEN/F01/Rev.1, Source: https://population.un.org/wpp/Download/Standard/MostUsed/)
  pop_wpp22 <- read.csv("Input_data/Population_UNWPP_2024.csv") %>% 
    dplyr::mutate(Value = Value/1e6) 
  
  pop_base_year <- pop_wpp22 %>% 
    dplyr::filter(Year==Base_year) %>% # Global 
    dplyr::pull(Value)  # Pulling out base year value and converting to billions
  
  pop_2050_BAU <- pop_wpp22 %>% 
    dplyr::filter(Scenario=="Medium") %>% # Global 
    dplyr::pull(Value)  # Pulling out base year value and converting to billions
  
  pop_2050_SSP <- seq(pop_SSPs$pop %>% min(),pop_SSPs$pop %>% max(),length.out= levels)
  
  Pop_levels <- c(pop_2050_SSP[1:2],pop_2050_BAU,pop_2050_SSP %>% max())
  
  Population <- Pop_levels/pop_base_year-1 # SSP1 (most optimistic) + Low, Median, High estimate from UN DESA (2019) - source: https://population.un.org/wpp/Download/Standard/Population/
  
  Pop_2050_levels <- data.frame(Pop_levels,Population)
  
  # 2.2 Diet variables - as multipliers compared to base year FAOSTAT food supply - NO WASTE
  
  Items <- list(c("Vegetal Products"),
                c("Animal Products"),
                c("Bovine Meat","Mutton & Goat Meat"),
                c("Pigmeat","Poultry Meat","Fish, Seafood"),
                c("Milk - Excluding Butter"),
                c("Eggs"))
  
  Kcal_values <- sapply(seq_along(Items),function(x) dplyr::filter(FBS,Item %in% Items[[x]]) %>%
                          dplyr::select(-Item.Code) %>% 
                          dplyr::distinct() %>% # Removes duplicates
                          dplyr::pull(Value) %>% # Pulls vector
                          sum()) # Sums up 
  
  Vegetal_kcal <- seq(2300,2900,length.out= levels)/Kcal_values[1]-1 # Allows enough calories for vegan diets (incl.waste) and also high calorie western diets 
  Animal_kcal <- seq(250,700,length.out= levels)/Kcal_values[2]-1 # Allows enough calories for flexitarian diets and also very high meat western diets (30% animal calories) 
  
  Rum_meat_kcal <- seq(25,70,length.out= levels)/Kcal_values[3]-1# Bovine Meat and Mutton & Goat Meat - varied in isolation 
  Non_rum_meat_kcal <- seq(150,300,length.out= levels)/Kcal_values[4]-1 # Poultry Meat, Pigmeat & seafood total
  Dairy_kcal <- seq(120,180,length.out= levels)/Kcal_values[5]-1 # Milk excluding butter
  Eggs_kcal <- seq(15,60,length.out= levels)/Kcal_values[6]-1 # Eggs 
  Non_rum_kcal <- (seq(100,300,length.out= levels) + seq(15,60,length.out= levels))/(Kcal_values[4]+Kcal_values[6])-1 # Total non-ruminant calories
  
  Diet_all <- data.frame(Rum_meat_kcal = c(Kcal_values[3],seq(25,70,length.out= levels)),
                                 Dairy_kcal = c(Kcal_values[5],seq(120,180,length.out= levels)),
                                 Non_rum_kcal = c(Kcal_values[4]+Kcal_values[6],seq(150,300,length.out= levels)+seq(15,60,length.out= levels)),
                                 Vegetal_kcal = c(Kcal_values[1],seq(2300,2900,length.out= levels)))
  
  Diet_2010 <- Diet_all %>% 
    dplyr::filter(dplyr::row_number()==1)
  
  Diet_2050 <- Diet_all %>% 
    dplyr::filter(dplyr::row_number()!=1) 
  
  Diet_2050_comb <- expand.grid(Diet_2050) %>% 
    dplyr::add_row(Diet_2010,.before = 1) # Creating all combinations
  
  Diet_2050 <- Diet_2050_comb %>%  # Calculating relative deviation (%) from base year
    dplyr::mutate_all(dplyr::funs(c(dplyr::first(.), (. / dplyr::first(.))[-1]-1)) )  
  
  # Merging levels and % change figures for all animal and plant calorie combinations
  Diet_2050_levels <- Diet_2050_comb %>% # Merging absolute and % change figures
    magrittr::set_colnames(c("Rum_meat_kcal_levels","Dairy_kcal_levels","Non_rum_kcal_levels","Plant_kcal_levels")) %>% 
    tibble::add_column(Diet_2050) %>% 
    dplyr::slice(-1) %>% 
    dplyr::mutate(Diet = dplyr::case_when(Rum_meat_kcal_levels==25 & Dairy_kcal_levels==120 & Non_rum_kcal_levels==165  ~ "LOW ASF", # Levels chosen based on the reviewed literature to ensure nutritional outcomes
                            Rum_meat_kcal_levels==40 & Dairy_kcal_levels==160 & Non_rum_kcal_levels==230  ~ "REDUCED MEAT",
                            Rum_meat_kcal_levels==55 & Dairy_kcal_levels==160 & Non_rum_kcal_levels==295  ~ "BAU DIET",
                            Rum_meat_kcal_levels==70 & Dairy_kcal_levels==180 & Non_rum_kcal_levels==360  ~ "RICH DIET"))
  
  if(Diet_simplified == "yes"){
    
    Diet_2050_selected <- Diet_2050_levels %>% # Selecting only representative diets - can be modified
      dplyr::filter(!is.na(Diet))
  
  } else {
  
    Diet_2050_selected <- Diet_2050_comb # Select all combinations
  
  }
  
  Waste_levels <- c("Current","BAU_High","BAU_Low","Half") # All waste levels
  Diet_levels <- c("Current","FLEX","LOW MEAT","BAU","RICH") # Pre-canned diet levels - if using that instead of separate animal/veg calorie combinations
  
  # Reading in waste fractions for each commodity category
  Waste_ratios <- read_excel("Input_data/Scenario_settings/Global_waste_fractions_2010.xlsx") %>%  
    dplyr::mutate(Category=dplyr::recode(Category,`All plant calories`="Vegetal_kcal", # Recoding to allow merging with diet dataframe
                                  `Ruminant meat`="Rum_meat_kcal",
                                  `Dairy`="Dairy_kcal",
                                  `Monogastrics + farmed seafood`="Non_rum_kcal"))

  # Calculating food supply caloric estimates at different waste levels
  Diet_waste_all <- Diet_2050_selected %>% 
    dplyr::select(Diet,Rum_meat_kcal_levels:Plant_kcal_levels) %>% # cAN CHANGE to Diet 2050
    magrittr::set_colnames(c("Diet","Rum_meat_kcal","Dairy_kcal","Non_rum_kcal","Vegetal_kcal")) %>% 
    tidyr::pivot_longer(cols = Rum_meat_kcal:Vegetal_kcal,names_to = "Category",values_to = "kcal_pc") %>% 
    merge(Waste_ratios) %>% 
    dplyr::mutate(Current = kcal_pc,
           BAU_High = kcal_pc*(1+(Waste_fraction/4)),
           BAU_Low = kcal_pc*(1-(Waste_fraction/4)),
           Half = kcal_pc*(1-(Waste_fraction/2)),
           Zero = kcal_pc*(1-(Waste_fraction)))
  
  # Harmonising base year diet column names and formatting to calculate percentage change
  Base_year_diet <- Diet_all[1,] %>% # Selecting first row (base year)
    tidyr::pivot_longer(cols = Rum_meat_kcal:Vegetal_kcal,names_to = "Category",values_to = "kcal_pc")%>% 
    magrittr::set_colnames(c("Category",paste0("kcal_",Base_year)))
  
  # Calculating change relative to the base year for all scenario combinations (including zero waste)
  Diet_waste_relative <- Diet_waste_all %>% 
    dplyr::select(-c(kcal_pc,Waste_fraction)) %>% 
    tidyr::pivot_longer(cols = Current:Zero,names_to = "Waste",values_to = "kcal") %>% 
    dplyr::arrange(Diet,Waste) %>% 
    merge(Base_year_diet) %>% 
    dplyr::mutate(Perc_change = (kcal/kcal_2010)-1)
  
# Separating out animal and vegetal calories
  
  Diet_waste_change_animal <- Diet_waste_relative %>% 
    dplyr::select(-kcal_2010) %>% 
    dplyr::filter(Category!="Vegetal_kcal") %>% 
    dplyr::distinct() %>% 
    tidyr::pivot_wider(names_from = Category,values_from = c(kcal,Perc_change))
  
  Diet_waste_change_all <- Diet_waste_relative %>% # Treating vegetal kcal alone and then merging
    dplyr::select(-c(kcal_2010)) %>% 
    dplyr::filter(Category=="Vegetal_kcal") %>% 
    dplyr::distinct() %>% 
    dplyr::left_join(Diet_waste_change_animal,by=c('Diet','Waste')) %>% 
    dplyr::arrange(Diet,Waste,kcal)

#writexl::write_xlsx(Diet_waste_change_all,"Diet_waste_check.xlsx") # For inspecting results

# Creating the final dataframe for diet/waste 
      
nrow_diet_waste <- nrow(Diet_waste_change_all) # Number of unique scenarios
Current_supply <- Diet_waste_change_all %>% dplyr::filter(Waste=="Current")
Food_intake <- Diet_waste_change_all %>% dplyr::filter(Waste=="Zero")

Diet_waste_final <- Diet_waste_change_all %>% # Adding levels as supply (full waste current and intake)
  tibble::add_column(Vegetal_kcal_supply = Current_supply$kcal %>% unique() %>% rep(nrow_diet_waste/4),
             Vegetal_kcal_intake = Food_intake$kcal %>% unique() %>% rep(nrow_diet_waste/4),
             Dairy_kcal_supply = c(160,120,160,180) %>% rep(each=nrow_diet_waste/4), # Dairy needs an exception as levels are maintained for health
             Dairy_kcal_intake = c(150.9145,113.1858,150.9145,169.7788) %>% rep(each=nrow_diet_waste/4),
             Non_rum_kcal_supply = Current_supply$kcal_Non_rum_kcal %>% unique() %>% rep(nrow_diet_waste/4),
             Non_rum_kcal_intake = Food_intake$kcal_Non_rum_kcal %>% unique() %>% rep(nrow_diet_waste/4),
             Rum_meat_kcal_supply = Current_supply$kcal_Rum_meat_kcal %>% unique() %>% rep(nrow_diet_waste/4),
             Rum_meat_kcal_intake = Food_intake$kcal_Rum_meat_kcal %>% unique() %>% rep(nrow_diet_waste/4)) %>% 
  dplyr::select(-c(Category,kcal,kcal_Dairy_kcal:kcal_Rum_meat_kcal)) %>% 
  dplyr::filter(Waste!="Zero") %>%  # Removing zero waste as this was only used for determining intake
  dplyr::rename(Vegetal_kcal=Perc_change,
                Dairy_kcal = Perc_change_Dairy_kcal,
                Non_rum_kcal = Perc_change_Non_rum_kcal,
                Rum_meat_kcal = Perc_change_Rum_meat_kcal)

  # 2.3 Crop/cereal yields - sourced from van Zeist et al. (2020) GEC - https://doi.org/10.1016/j.gloenvcha.2020.102120
  
  Yield_cereals <- read.csv("Input_data/Scenario_settings/1-s2.0-S0959378020307032-mmc3.csv") %>% # Reading in cereal yields from van Zeist et al. (2020)
    dplyr::filter(Region==Area & Item==Yield_element)
  
  Base_year_yield <- Yield_cereals %>% 
    dplyr::filter(Year==Base_year & Scenario=="Historical" & Model=="FAO") %>% 
    dplyr::pull(value)
  
  Future_yield <- Yield_cereals %>% 
    dplyr::filter(Year==Scen_year & Variable=="Attainable yield" & 
             Scenario=="Attainable yield (plateau)" & Model=="AY_av") %>% 
    dplyr::pull(value)
  
  Yield_multiplier <- round(Future_yield/Base_year_yield,1) # Yield multiplier rounded to 1 decimal place
  
  Yield_2050_levels <- data.frame(seq(1.15,Yield_multiplier,length.out=levels),
    seq(1.15,Yield_multiplier,length.out=levels)-1) %>% 
    as.data.frame() %>% # Yield increase relative to 2010 FAOSTAT baseline
    magrittr::set_colnames(c("Yield_levels","Yield")) 
    
  # 2.4 Livestock feed efficiency and feed composition variables (FCR & FCF)
  
  FCR_rum_meat <- seq(35,20,length.out=levels) # Taking FCR levels established through summary statistics of papers in the database
  FCF_rum_meat <- seq(0.05,0.2,length.out=levels) # Taking FCF levels established through summary statistics of papers in the database
  
  FCR_FCF_rum_meat <- tidyr::expand_grid(FCR_rum_meat,FCF_rum_meat) %>% 
    dplyr::mutate(FCR_FCF_rum_meat = FCR_rum_meat*FCF_rum_meat,
           FCR_grass_rum_meat = FCR_rum_meat*(1-FCF_rum_meat)) %>% 
    dplyr::mutate(dplyr::across(c("FCR_rum_meat","FCR_FCF_rum_meat","FCR_grass_rum_meat"), ~(.) / dplyr::first(.)-1))# Calculating percentage change# Calculating percentage change
  
  FCR_dairy <- seq(2,1.25,length.out=levels) # Taking FCR levels established through summary statistics of papers in the database
  FCF_dairy <- seq(0.15,0.3,length.out=levels) # Taking FCF levels established through summary statistics of papers in the database
  
  FCR_FCF_dairy <- tidyr::expand_grid(FCR_dairy,FCF_dairy) %>% 
    dplyr::mutate(FCR_FCF_dairy = FCR_dairy*FCF_dairy,
           FCR_grass_dairy = FCR_dairy*(1-FCF_dairy)) %>% 
    dplyr::mutate(dplyr::across(c("FCR_dairy","FCR_FCF_dairy","FCR_grass_dairy"), ~(.) / dplyr::first(.)-1))# Calculating percentage change
  
  FCR_monogastrics <- seq(4,2.5,length.out=levels) # Taking FCR levels established through summary statistics of papers in the database
  FCF_monogastrics <- seq(0.8,0.95,length.out=levels) # Taking FCF levels established through summary statistics of papers in the database
  
  FCR_FCF_monogastrics <- tidyr::expand_grid(FCR_monogastrics,FCF_monogastrics) %>% 
    dplyr::mutate(FCR_FCF_monogastrics = FCR_monogastrics*FCF_monogastrics,
           FCR_grass_monogastrics = FCR_monogastrics*(1-FCF_monogastrics)) %>% 
    dplyr::mutate(dplyr::across(c("FCR_monogastrics","FCR_FCF_monogastrics","FCR_grass_monogastrics"), ~(.) / dplyr::first(.)-1))# Calculating percentage change
  
  FCR_FCF_2050_levels <- cbind(FCR_FCF_rum_meat,FCR_FCF_dairy,FCR_FCF_monogastrics) %>%  # Binding FCRs/FCFs for all animals - these will all vary together
    dplyr::mutate(Feed_efficiency = dplyr::case_when(FCR_rum_meat==0 & FCR_dairy==0 & FCR_monogastrics==0 ~ "STAGNANT", # Ruminant meat trends also follows the others
                                       FCR_dairy==-0.125 & FCR_monogastrics==-0.125 ~ "TREND",
                                       FCR_dairy==-0.250 & FCR_monogastrics==-0.250 ~ "ACCELERATED",
                                       FCR_dairy==-0.375 & FCR_monogastrics==-0.375 ~ "HIGH")) %>% 
    tibble::add_column(Feed_composition = rep(c("LOW GRAIN/HIGH GRASS","TREND","INTENSIFIED","HIGH GRAIN/LOW GRASS"),4)) 
  
  ## 2.4 Other efficiency parameters
  
  # Nutrients and water
  
  N_management <- c("Current","+10% NUE,+10% REC","+20% NUE,+20% REC","+30% NUE,30% REC")
  P_management <- c("Current","+5% PUE,+15% REC","+10% PUE,+30% REC","+15% PUE,+45% REC")
  
  NUEinc <- seq(0,0.30,length.out=levels) # Based on levels in the database and UNEP GEO-6 and Zhang et al. (2015) recommendations (SDGs etc)
  PUEinc <- seq(0,0.15,length.out=levels)  # Based on levels in database and general FUE recommendations
  
  NrecHousehold <-  seq(0,0.3,length.out=levels)  # Based on levels in database and general FUE recommendations
  NrecManure <- seq(0.75,0.90,length.out=levels) # # Based on levels in database and general FUE recommendations
  PrecHousehold <-  seq(0,0.45,length.out=levels)  # Based on levels in database and general FUE recommendations
  PrecManure <- seq(0.85,1.00,length.out=levels) # # Based on levels in database and general FUE recommendations
  
  N_2050_levels <- data.frame(N_management,NUEinc,data.frame(NrecHousehold,NrecManure)) # Different efficiency and recycling combinations
  P_2050_levels <- data.frame(P_management,PUEinc,data.frame(PrecHousehold,PrecManure)) # Different efficiency and recycling combinations
  
  WPinc <- seq(0,0.15,length.out=levels) # Based on levels in the database and UNEP GEO-6 
  
  GHG_eff_all <- seq(0,0.30,length.out= levels) # From no action to $200t price for carbon mitigation (Source: Lucas et al. 2007)
  GHG_eff_CH4 <- seq(0,0.40,length.out= levels) # Up to mitigation equivalent with scenarios with $200/tC - feasibility according to Roe et al. (2021): https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15873
  GHG_eff_N2O <- seq(0,0.12,length.out= levels) # Up to mitigation equivalent with scenarios with $200/tC - feasibility according to Roe et al. (2021): https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15873
  C_price <- c(0,25,100,200) # Price range based on IPCC AR6 WGIII: https://report.ipcc.ch/ar6wg3/pdf/IPCC_AR6_WGIII_SummaryForPolicymakers.pdf (Figure SPM.7: Overview of mitigation options and their estimated ranges of costs and potentials in 2030.)
  
  ## 3. Generate scenario prediction list with all predictors and their mitigation levels
 
  Pred_dataset <- list(Population=Pop_2050_levels %>% round(3),
                        Diet = Diet_waste_final,
                        FCR_FCF = FCR_FCF_2050_levels,
                        Yield = Yield_2050_levels,
                        Water = data.frame(WPinc),
                        GHG = data.frame(GHG_eff_all,GHG_eff_CH4,GHG_eff_N2O,C_price),
                        Nitrogen = N_2050_levels,
                        Phosphorus = P_2050_levels)
  
  # Writing final dataset

  return(Pred_dataset)
}

#saveRDS(Pred_dataset, file = paste0("Pred_dataset_revised_",Sys.Date(),".rds"))
