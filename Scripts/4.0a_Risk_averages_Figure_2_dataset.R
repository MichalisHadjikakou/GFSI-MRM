##################################################
## FUTURE FOOD SCENARIOS - Model Avg_predictions ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Last updated: 01 March 2021
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
sim_date <- "2023-02-07" # Date of simulation
today <- Sys.Date() # Today's date
Base_year <- 2010
Scen_year <- 2050
levels <- 4

if(PC=='work_laptop') {
  setwd("N:/LES/Burwood/Brett-lab/Michalis/Future_food_systems_review/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("N:/Future_food_systems_review/GFSS-MM/")
} else {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSS-MM/")  
}

capFirst <- function(s) { # Capitalise first letter
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,RColorBrewer,Hmisc,extraDistr,formattable,future.apply,svMisc,wppExplorer,wpp2019,readxl)

# Setting up parallel processing for future.apply
plan(multisession, workers = 8)

source("Scripts/All_variable_levels_bad_to_good.R") # Need to add to script folder

# Indicator names
ind <- c("Land_system_change","Freshwater_Use","Climate_change","Biogeochemical_Flows") 
         #"Nfert","Nsurplus","Pfert","Psurplus")

ind_names <- c("Land-system change","Freshwater use","Climate change","Biogeochemical Flows") # Revised indicator names 

#ind_names <- c("Cropland","TotalArea","Forest","Water",expression("DirNonCO"[2]),"DirGHG","Dir+IndGHG",expression("DirNonCO"[2]*"+LUC"),
#               "Nfert","Nsurplus","Pfert","Psurplus") # Revised indicator names 

# Intervention/predictor names

All_preds <- as.character(expression(Pop_levels,Diet,Plant_kcal,Waste,
                                     Yield_levels,Feed_efficiency,Feed_composition,Carbon_price,WUEinc,N_management))

# Making adjustments to intervention/prediction levels

All_levels <- head(All_levels,-1) # Removing P_management as this intervention has been aggregated here 

All_levels[[which(All_preds=="Pop_levels")]] <- All_levels[[which(All_preds=="Pop_levels")]] %>% 
  round(1)

All_levels[[which(All_preds=="Carbon_price")]] <- All_levels[[which(All_preds=="Carbon_price")]] %>% 
  round(1)

All_levels_char <- lapply(All_levels, as.character) # Converting factors to strings

#Pred_names <- c("Population","Vegetal_kcal","Animal_kcal","Diet","YieldGap","FeedEffinc","Neff","WUEinc","GHGeff","Trade","FCF","Nrec") # Re-arranged

list_all <- list() # List where all results will be saved 
Predictors <- list() # List with predictors for each model


######################################### 2. CREATING DATASET WITH AVERAGED INDICATORS AND STANDARD DEVIATIONS ####################################################

## for k in ind (1 out of 12 indicators) - simulation date will depend on which results need to be retrieved

for (k in seq_along(ind)){
  
  print(k)
  
  progress(k, progress.bar = TRUE)
  data <- fread(paste0("Outputs/Risk_estimates/",ind[k],"_",sim_date,".csv")) %>%  # Reading in csv with all generated predictions based on indicator model
    mutate(Pop_levels = Pop_levels %>% round(1))
  # set.seed(123) # Ensures reproducibility of random generation
  # Median_PB <- qtriang(c(0.25,0.5,0.75), # Running through each row of the dataframe and selecting the appropriate indicator/boundary
  #                      a = data$Min[1],
  #                      b = data$Max[1],
  #                      c = data$Mode[1])
  
  Col_names <- colnames(data)[which(colnames(data)=="Pop_levels"):length(colnames(data))] # Indicator column names
  
  Predictors[[k]] <- Col_names[1:grep("Pred_avg",Col_names)[1]-1] # Only selecting pure predictor names which varies with indicator
  
  groups <- rlang::syms(Predictors[[k]]) # Instead of quosures, this is a specific predictor selection for each indicator
  
  # Initiating empty lists to save results and plots
  
  xtabs <- replicate(length(All_preds), list()) # List to save counts and percentages for each variable level across each variable
  var_levels <- list() # List of variable level names
  df_plot <- replicate(length(groups), list()) # List of list of variables level names
  df_graphs <- replicate(length(groups), list()) # List of plots corresponding to each variable level
  colour_order <- replicate(length(groups), list()) # List of colour orders specific to the iteration
  #layout.matrix <- matrix(0, nrow =5, ncol = length(All_preds)) # Matrix with zeros to be filled with positions of each chart in subsequent loop
  
  ind_preds <- match(Predictors[[k]], All_preds, nomatch = 0) # Predictor index (determines the horizontal position) # since some predictors may be missing
  
  # Indicator-specific layout to allow summary plot on top 
  
  for (i in seq_along(groups)) { # Looping through each predictor variable included 
    
    if(groups[i]=="Carbon_price"){ # Adding correction to ensure that the right number of levels are being matched properly
      data$Carbon_price <- data$Carbon_price %>% round(1)
    }
    
    #BAU <- data %>% 
    #  filter()
    
    xtabs[[ind_preds[i]]] <- data %>% # Saving as dataframe in the right position
      group_by(!!groups[[i]]) %>% # Unquote with !!
      dplyr::summarise(n = dplyr::n(),
                Pred_mean = mean(Pred_avg,na.rm=TRUE), Pred_SD = sd(Pred_avg,na.rm=TRUE),
                Risk_L = quantile(Risk,.05,na.rm=TRUE), Risk_H = quantile(Risk,.95,na.rm=TRUE),
                Risk_joint_mean = mean(Risk,na.rm=TRUE), Risk_SD = sd(Risk,na.rm=TRUE),
                Risk_joint_median = median(Risk,na.rm=TRUE),Risk_Q25 = quantile(Risk,.25,na.rm=TRUE), Risk_Q75 = quantile(Risk,.75,na.rm=TRUE)) %>% 
                #Pred_LB = mean(Pred_lwr), Pred_UP = mean(Pred_upr)) %>% 
                #Exc_mean = mean(Exceedance,na.rm = TRUE),Exc_SD =sd(Exceedance,na.rm = TRUE), 
                #Exc_mode = median(Exceedance_mode), Exc_min = median(Exceedance_mode_SD_L),Exc_max = median(Exceedance_mode_SD_H),# Metrics based on min, mode and max of boundary
                #Exc_median = median(Exceedance,na.rm = TRUE), Exc_Q25 = quantile(Exceedance,.25,na.rm = TRUE),Exc_Q75 = quantile(Exceedance,.75,na.rm = TRUE)) %>% # Counting risk colours
      group_by_at(1) %>% 
      data.frame() %>% 
      dplyr::rename(Level = 1) %>% # Renaming first column to level (ambition level)
      arrange(match(Level,unlist(All_levels_char[ind_preds[[i]]]))) %>% # Arranging with increasing level of mitigation
      add_column(Predictor=Predictors[[k]][i],.before = 1) %>% 
      add_column(Indicator=ind[k],.before = 1) 
  }
  list_all[[k]] <- xtabs
}

# Concatenating lists by predictor/intervetnion and tidying up long numeric levels to ensure matching
All_pred_lists <- do.call(Map, c(f = rbind, list_all))

All_pred_lists[[which(All_preds=="Pop_levels")]] <- All_pred_lists[[which(All_preds=="Pop_levels")]] %>% 
  mutate(Level = Level %>% round(1))

All_pred_lists[[which(All_preds=="Carbon_price")]] <- All_pred_lists[[which(All_preds=="Carbon_price")]] %>% 
  mutate(Level = Level %>% round(1))

All_results <- do.call(rbind,All_pred_lists)

# Writing results for barplot script and also saving in an Excel file
saveRDS(All_pred_lists,paste0("Outputs/Composite_barplots//Figure_2_data_list_",today,".rds")) # Saving models for prediction
writexl::write_xlsx(All_results,paste0("Outputs/Composite_barplots/Figure_2_data_table_",today,".xlsx"))

