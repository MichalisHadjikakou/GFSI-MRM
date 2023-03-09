##################################################
## FUTURE FOOD SCENARIOS - Model Avg_predictions ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Last updated: 25 May 2022
# Purpose: Generate Avg_predictions and plot exceedance ratios across all indicators and predictor combinations
# Outputs: Data for figure 3 in main manuscript

# Notes: In this script the barplots are plotted using exceedance relative to the mode (Risk_avg) and the uncertainty is over the min and max of
# the boundary (Risk_L & Risk_H) - previously we were using exceedance relative to the median boundary. Both are possible by modifying the script

################################################################ 1. SETTING UP SCRIPT #########################################################################

## 1.1  Working directories
rm(list = ls())
PC <- 'denethor'
write_results <- 'yes'
n <- 10000 # Number of random draws for distributions/simulations
sim_date <- "2023-02-06"# Date of simulation
today <- Sys.Date() # Today's date
Base_year <- 2010
Scen_year <- 2050
levels <- 4

if(PC=='work_laptop') {
  setwd("N:/LES/Burwood/Brett-lab/Michalis/Future_food_systems_review/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("N:/Future_food_systems_review/GFSS-MM/")
} else {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSI-MRM/GFSI-MRM/")  
}

capFirst <- function(s) { # Capitalise first letter
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,RColorBrewer,Hmisc,extraDistr,formattable,future.apply,svMisc,wppExplorer,wpp2019,readxl)

source("Scripts/All_variable_levels_bad_to_good.R") # Need to add to script folder

# Indicator names
ind <- c("Cropland","Pasture","CH4","N2O","CO2_LUC",
         "Water","Nfert","Nsurplus","Pfert","Psurplus")

ind_names <- c("Cropland","Pasture","Methane","Nitrous oxide","Land use change emissions",
               "Blue water", "N fertiliser","N surplus, P fertiliser","P surplus") # Revised indicator names 

# Setting up parallel processing for future.apply
plan(multisession, workers = length(ind))

#ind_names <- c("Cropland","TotalArea","Forest","Water",expression("DirNonCO"[2]),"DirGHG","Dir+IndGHG",expression("DirNonCO"[2]*"+LUC"),
#               "Nfert","Nsurplus","Pfert","Psurplus") # Revised indicator names 

All_preds <- as.character(expression(Pop_levels,Diet,Plant_kcal,Waste,
                                     Yield_levels,Feed_efficiency,Feed_composition,Carbon_price,WUEinc,N_management,P_management))

All_levels[[which(All_preds=="Carbon_price")]] <- All_levels[[which(All_preds=="Carbon_price")]] %>% 
  round(1)

All_levels[[which(All_preds=="Pop_levels")]] <- All_levels[[which(All_preds=="Pop_levels")]] %>% 
  round(1)

All_levels_char <- lapply(All_levels, as.character) # Converting factors to strings

#Pred_names <- c("Population","Vegetal_kcal","Animal_kcal","Diet","YieldGap","FeedEffinc","Neff","WUEinc","GHGeff","Trade","FCF","Nrec") # Re-arranged

list_all <- list() # List where all results will be saved 
Predictors <- list() # List with predictors for each model


######################################### 2. CREATING DATASET WITH AVERAGED INDICATORS AND STANDARD DEVIATIONS ####################################################

## for k in ind (1 out of 12 indicators) - simulation date will depend on which results need to be retrieved
#list_all <- future_lapply(seq_along(ind),function(k){
  

    #Predictors <- list() # List with predictors for each model
for (k in seq_along(ind)){
  
  print(k)
  
  #progress(k, progress.bar = TRUE)
  data <- fread(paste0("Outputs/Meta-regression/Predictions/All_models_",sim_date,"/",ind[k],"_",sim_date,".csv")) %>% # Reading in csv with all generated predictions based on indicator model
    dplyr::rename(any_of(c("Plant_kcal" = "Vegetal_kcal_supply",
                           "Pred_avg" = "Pred_avg_LUC",
                           "Carbon_price" ="C_price",
                           "WUEinc" = "WPinc"))) %>% 
    dplyr::mutate(Pop_levels=round(Pop_levels,1))# Correcting column name
  
  Predictors <- data %>% # Indicator column names # Only selecting pure predictor names which varies with indicator
    dplyr::select(any_of(All_preds)) %>% 
    colnames()
  
  groups <- rlang::syms(Predictors) # Instead of quosures, this is a specific predictor selection for each indicator
  
#  lapply(seq_along(groups),function(i){
  
  # Initiating empty lists to save results and plots
  
  xtabs <- replicate(length(All_preds), list()) # List to save counts and percentages for each variable level across each variable
  var_levels <- list() # List of variable level names
  df_plot <- replicate(length(groups), list()) # List of list of variables level names
  df_graphs <- replicate(length(groups), list()) # List of plots corresponding to each variable level
  colour_order <- replicate(length(groups), list()) # List of colour orders specific to the iteration
  #layout.matrix <- matrix(0, nrow =5, ncol = length(All_preds)) # Matrix with zeros to be filled with positions of each chart in subsequent loop
  
  ind_preds <- match(Predictors, All_preds, nomatch = 0) # Predictor index (determines the horizontal position) # since some predictors may be missing
  
  # Indicator-specific layout to allow summary plot on top 
  
  for (i in seq_along(groups)) { # Looping through each predictor variable included 
    
    if(groups[i]=="Carbon_price"){ # Adding correction to ensure that the right number of levels are being matched properly
      data$Carbon_price <- data$Carbon_price %>% round(1)
    }
    
    xtabs[[ind_preds[i]]] <- data %>% # Saving as dataframe in the right position
      group_by(!!groups[[i]]) %>% # Unquote with !!
      summarise(n = dplyr::n(),
                Pred_mean = mean(Pred_avg,na.rm=TRUE), Pred_SD = sd(Pred_avg,na.rm=TRUE),
                #Risk_L = quantile(lwr,.05,na.rm=TRUE), Risk_H = quantile(upr,.95,na.rm=TRUE), # If the extremes are of interest
                Risk_L = quantile(fit,.05,na.rm=TRUE), Risk_H = quantile(fit,.95,na.rm=TRUE),
                Risk_joint_mean = mean(fit,na.rm=TRUE), Risk_joint_median = median(fit,na.rm=TRUE),
                Risk_Q25 = quantile(fit,.25,na.rm=TRUE), Risk_Q75 = quantile(fit,.75,na.rm=TRUE)) %>% 
      group_by_at(1) %>% 
      data.frame() %>% 
      dplyr::rename(Level = 1) %>%
      add_column(Predictor=Predictors[i],.before = 1) %>% 
      add_column(Indicator=ind[k],.before = 1) 
  }
  list_all[[k]] <- xtabs
}
    #data.frame(xtabs)

#  })
#})

# Concatenating lists by predictor/intervetnion and tidying up long numeric levels to ensure matching
All_pred_lists <- do.call(Map, c(f = rbind, list_all))

All_pred_lists[[which(All_preds=="Pop_levels")]] <- All_pred_lists[[which(All_preds=="Pop_levels")]] %>% 
  mutate(Level = Level %>% round(1))

All_pred_lists[[which(All_preds=="Carbon_price")]] <- All_pred_lists[[which(All_preds=="Carbon_price")]] %>% 
  mutate(Level = Level %>% round(1))

All_results <- do.call(rbind,All_pred_lists)

# Writing results for barplot script and also saving in an Excel file
saveRDS(All_pred_lists,paste0("Outputs/Composite_barplots/Figure_SM_data_list_",today,".rds")) # Saving models for prediction
writexl::write_xlsx(All_results,paste0("Outputs/Composite_barplots/Figure_SM_data_table_",today,".xlsx"))


