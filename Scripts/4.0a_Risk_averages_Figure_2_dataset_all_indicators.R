##################################################
## FUTURE FOOD SCENARIOS - Model Avg_predictions ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Generate Avg_predictions and plot exceedance ratios across all indicators and predictor combinations
# Outputs: Data preparation for Figure 3 in main manuscript

################################################################ 1. SETTING UP SCRIPT #########################################################################

## 1.1  Loading necessary packages, functions and key variables

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,RColorBrewer,Hmisc,extraDistr,formattable,future.apply,svMisc,wppExplorer,wpp2019,readxl,wcde,data.table)

write_results <- 'yes'
n <- 10000 # Number of random draws for distributions/simulations
sim_date <- "2024-11-01" # Date of simulation
today <- Sys.Date() # Today's date
Base_year <- 2010
Scen_year <- 2050
levels <- 4
WUEinc_levels <- c(0.00,0.05,0.10,0.15) # Writing this out separately to ensure conditional filtering works

capFirst <- function(s) { # Capitalise first letter
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

source("Scripts/All_variable_levels_bad_to_good.R") # Need to add to script folder

# Indicator names
ind <- c("Land_system_change","Freshwater_Use","Climate_change","Biogeochemical_Flows_N","Biogeochemical_Flows_P") 
         
ind_names <- c("Land-system change","Freshwater use","Climate change","Biogeochemical flows N","Biogeochemical_Flows P") # Revised indicator names 

# Setting up parallel processing for future.apply
plan(multisession, workers = length(ind))

# Intervention/predictor names

All_preds <- as.character(expression(Pop_levels,Diet,Plant_kcal,Waste,
                                     Yield_levels,Feed_efficiency,Feed_composition,
                                     Carbon_price,WUEinc,N_management,P_management))

## 1.2  Making necessary adjustments to predictors and levels

All_levels[[which(All_preds=="Pop_levels")]] <- All_levels[[which(All_preds=="Pop_levels")]] %>% 
  round(1)

All_levels[[which(All_preds=="Carbon_price")]] <- All_levels[[which(All_preds=="Carbon_price")]] %>% 
  round(1)

All_levels_char <- lapply(All_levels, as.character) # Converting factors to strings

list_all <- list() # List where all results will be saved 
Predictors <- list() # List with predictors for each model

# Setting up look-up tables for scenario filtering

All_BAU <- matrix(2,nrow = levels,ncol = length(All_preds)-1) %>% 
  data.frame()

All_int_combs <- lapply(seq_along(All_preds),function(x) All_BAU %>%
                          add_column(1:4,.before = x) %>% 
                          magrittr::set_colnames(All_preds)) %>% 
  bind_rows()


######################################### 2. CREATING DATASET WITH INDIVIDUAL EFFECTS ####################################################

## 2.1  Looping through the indicators to create a list of level-specific intervention effects

for (k in seq_along(ind)){
  
  print(k)
  
  progress(k, progress.bar = TRUE)
  data <- fread(paste0("Outputs/Risk_estimates/",ind[k],"_",sim_date,".csv")) %>%  # Reading in csv with all generated predictions based on indicator model
    mutate(Pop_levels = Pop_levels %>% round(1))
  
  Col_names <- colnames(data)[which(colnames(data)=="Pop_levels"):length(colnames(data))] # Indicator column names
  
  Predictors[[k]] <- Col_names[1:grep("Pred_avg",Col_names)[1]-1] # Only selecting pure predictor names which varies with indicator
  
  groups <- rlang::syms(Predictors[[k]]) # Instead of quosures, this is a specific predictor selection for each indicator
  
  if(ind[k]=="Biogeochemical_Flows_N"){
  
    All_combs <- lapply(1:dim(All_int_combs)[1],
           function(x) data %>% 
             filter(.data[[All_preds[[1]]]]==All_levels[[1]][All_int_combs$Pop_levels[x]], 
                    .data[[All_preds[[2]]]]==All_levels[[2]][All_int_combs$Diet[x]],
                    .data[[All_preds[[3]]]]==All_levels[[3]][All_int_combs$Plant_kcal[x]],  
                    .data[[All_preds[[4]]]]==All_levels[[4]][All_int_combs$Waste[x]],
                    .data[[All_preds[[5]]]]==All_levels[[5]][All_int_combs$Yield_levels[x]],
                    .data[[All_preds[[6]]]]==All_levels[[6]][All_int_combs$Feed_efficiency[x]],
                    .data[[All_preds[[7]]]]==All_levels[[7]][All_int_combs$Feed_composition[x]],
                    if_any(matches(All_preds[[8]]),~.x %in% All_levels[[8]][All_int_combs$Carbon_price[x]]),# Source: https://stackoverflow.com/questions/45146688/execute-dplyr-operation-only-if-column-exists 
                    if_any(matches(All_preds[[9]]),~.x %in% WUEinc_levels[All_int_combs$WUEinc[x]]),
                    if_any(matches(All_preds[[10]]),~.x %in% All_levels[[10]][All_int_combs$N_management[x]])))
  
  }else{
    All_combs <- lapply(1:dim(All_int_combs)[1],
                        function(x) data %>% 
                          filter(.data[[All_preds[[1]]]]==All_levels[[1]][All_int_combs$Pop_levels[x]], 
                                 .data[[All_preds[[2]]]]==All_levels[[2]][All_int_combs$Diet[x]],
                                 .data[[All_preds[[3]]]]==All_levels[[3]][All_int_combs$Plant_kcal[x]],  
                                 .data[[All_preds[[4]]]]==All_levels[[4]][All_int_combs$Waste[x]],
                                 .data[[All_preds[[5]]]]==All_levels[[5]][All_int_combs$Yield_levels[x]],
                                 .data[[All_preds[[6]]]]==All_levels[[6]][All_int_combs$Feed_efficiency[x]],
                                 .data[[All_preds[[7]]]]==All_levels[[7]][All_int_combs$Feed_composition[x]],
                                 if_any(matches(All_preds[[8]]),~.x %in% All_levels[[8]][All_int_combs$Carbon_price[x]]),# Source: https://stackoverflow.com/questions/45146688/execute-dplyr-operation-only-if-column-exists 
                                 if_any(matches(All_preds[[9]]),~.x %in% WUEinc_levels[All_int_combs$WUEinc[x]]),
                                 if_any(matches(All_preds[[10]]),~.x %in% All_levels[[10]][All_int_combs$N_management[x]]),
                                 if_any(matches(All_preds[[11]]),~.x %in% All_levels[[11]][All_int_combs$P_management[x]])))
  }
      
  list_all[[k]] <-  All_combs %>% 
    bind_rows() %>% 
    add_column(Predictor=rep(All_preds,each=levels),
               Level = All_levels_char %>% unlist()) %>% 
    filter(Predictor %in% Predictors[[k]]) %>% 
    relocate(Predictor,Level,.after = System) %>% 
    dplyr::select(-any_of("Indicator")) %>% 
    rename(Indicator=System) %>% 
    dplyr::select(Indicator,Predictor,Level,Pred_avg,Pred_SD,Risk)
  
}  

## 2.2  Concatenating into one data.frame and saving results 
 
All_results <- list_all %>% 
    bind_rows()

# Writing results for barplot script and also saving in an Excel file

writexl::write_xlsx(All_results,paste0("Outputs/Composite_barplots/Figure_3_data_table_",today,".xlsx"))

