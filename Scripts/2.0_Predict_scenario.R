##################################################
## FUTURE FOOD SCENARIOS - Predict_scenario ##
##################################################

# Author: Michalis Hadjikakou, Deakin University m.hadjikakou@deakin.edu.au)
# Last updated: February 2023
# Purpose: Quickly create new predictions with saved models

################################################# 1.0 INITIALISING #####################################################

## 1.1  Working directories
rm(list = ls())
PC <- "denethor"

if(PC=='work_laptop') {
  setwd("C:/Users/Hadj/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("M:/Food-Systems/Meta_analysis/")
} else if (PC =='denethor') {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSI-MRM/GFSI-MRM/")
} else {
  setwd("C:/Users/Michalis/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/GFSS-MM/")  
}

## 1.2 Setting key parameters and loading packages

n <- 10000 # Number of random draws for distributions/simulations
Nsim = 2000 # Number of bootstraps
set.seed(123) # Ensures reproducibility of bootstrap results
simdate <- "2023-03-21"
today <- Sys.Date() # Today's date

# Prediction parameters
Base_year <- 2010
Scenario_year <- 2050
Pred_levels <- 4 # 4 unique levels for each predictor

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(extraDistr,data.table,gridExtra,car,MASS,outliers,propagate,sjPlot,lme4,glmm,hablar,wppExplorer,wpp2019,lmerTest,readxl,
               LMERConvenienceFunctions,MuMIn,fitdistrplus,merTools,eatGet,tidyverse,pbkrtest,future.apply)

# Indicator and broad scenario names
# Indicator and broad scenario names
ind <- c("Cropland","Pasture","Water","CH4","N2O", # Forest and LUC are derived post-hoc (on the basis of pasture and cropland)
         "Nfert","Nsurplus","Pfert","Psurplus") # Using 'Nfert' instead of "Nfix' as per Springmann et al. (2018) 
system <- c(rep("Land-System Change",2),"Freshwater Use",rep("Climate Change",2),
           rep("Biogeochemical Flows",4))
# LUC is produced in another script

effect_size <- rep("LnR",length(ind)) # All indicator use LnR as effect size metric 
effect_size[ind=="Pasture"] <- "delta%" # Except for pasture
  
value_base_year <- c(1519, # Source: FAOSTAT Cropland in base year
                     3277, # Source: FAO - Permanent meadows and pastures in base year
                     1807, # Freshwater use Source: Springmann et al. 2018 - compatible with water boundary definition
                     #5673.004, #AR6: 5623.622 # 2010 Non-CO2 emissions FAO Tier 1 IPCC Agriculture, Source: Tubiello et al. 2021, FAOSTAT: https://www.fao.org/faostat/en/#data/GT
                     3659.082, #AR6: 3659.082 # 2010 CH4 emissions FAO Tier 1 IPCC Agriculture, Source: Tubiello et al. 2021, FAOSTAT: https://www.fao.org/faostat/en/#data/GT
                     1963.54, #AR6: 1963.54 # 2010 N2O emissions FAO Tier 1 IPCC Agriculture, Source: Tubiello et al. 2021, FAOSTAT: https://www.fao.org/faostat/en/#data/GT
                     103.68, # 101.05, # Nfert, Source: 2010 FAOSTAT, https://www.fao.org/faostat/en/#data/RFN
                     134.39, # Nsurplus Source: Willet et al. 2019, The Lancet
                     17.85, # 18.97, # Pfert, Source: 2010 FAOSTAT, https://www.fao.org/faostat/en/#data/RFN
                     11.99) # Source: Beusen et al. (2022), GEC, https://doi.org/10.1016/j.gloenvcha.2021.102426

## 1.3 Loading all static files required for predictions

# Loading spreadsheet with environmental limits
PBs <- read.csv("Outputs/Environmental_limits/All_PB_limits.csv") # Reading in PB distribution parameters

source("Functions/Prediction_dataset_relative.R") # Loading prediction dataset
Pred_data <- Prediction_dataset(Base_year,Scenario_year,Pred_levels)

source("Functions/Prediction_parameters.R")# Loading full prediction dataset

# Loading directory with all models+data
All_models <- list.files(paste0("Outputs/Meta-regression/Fitted_models/Models/",simdate,"/"))
All_datasets <- list.files(paste0("Outputs/Meta-regression/Fitted_models/Datasets/",simdate,"/"))

################################################# 2.0 MODEL PREDICTIONS #####################################################

# New directory for saving results - will only be created if it doesn't already exist
dir.create(path = paste0("Outputs/Meta-regression/Predictions/All_models_",today,"/"))
ptm <- proc.time()

## 2.1 Looping through predictions for each control variable model
plan(multisession, workers = length(ind)) # Can change this accordingly (If 2010, use crop_num)
# 
future_sapply(seq_along(ind), function(i) {

#for (i in seq_along(ind)){
  
  print(i)

  # Creating bespoke prediction dataset for each indicator
  Ind_preds <- Pred_data_indicator(Pred_data,i) %>% 
    dplyr::mutate(Model = "~0",
           Delta_initial = 0) # # Adding column names and specifying random intercept =~0# Loading indicator-specific prediction dataset
  # Loading global model,top models and underlying dataset
  
  glmm_global <- readRDS(paste0("Outputs/Meta-regression/Fitted_models/Models/",simdate,"/",All_models[grep(ind[i],All_models)])) # Loading global model
  stat_df <- readRDS(paste0("Outputs/Meta-regression/Fitted_models/Datasets/",simdate,"/",All_datasets[grep(ind[i],All_datasets)])) # Loading final dataframe
  
  # Carrying out predictions with top models - if carrying out multi-model inference
  
  #ensemble <- readRDS(paste0("GLMMs/Selected_models_",ind[i],simdate,".rds")) # Loading full prediction dataset
  #model.preds <- sapply(get.models(ensemble, subset= TRUE), predict, newdata = Pred_dataset,re.form=~0) # Predicting from mean intercept 
  #Avg_prediction<-model.preds %*% Weights(ensemble) # Zero method
  
  # Bootstrapping model (using predictInterval and only accounting for uncertainty around fixed effects for speed)
  
  Pred_int <- suppressWarnings(predictInterval(merMod = glmm_global,
                                               newdata = Ind_preds,
                                               n.sims = Nsim,
                                               which='full',seed = 101,
                                               level=0.95,
                                               include.resid.var = T,
                                               stat="mean",returnSims = FALSE))
  
  if(effect_size[i]=='LnR') {  # Requires correction to express change as a percentage
    
    Pred_int <- Pred_int %>% 
      dplyr::mutate_all(~exp(.) - 1) # Converting log response ratio to % change
  }
  
  Final_preds <- cbind(Ind_preds,Pred_int) %>%  # Selecting only relevant columns for summarising/presenting prediction results
    dplyr::select(any_of(c("Pop_levels","Diet","Waste","Yield_levels","Feed_efficiency","Feed_composition","Rum_meat_kcal_supply","Rum_meat_kcal_intake",
                           "Dairy_kcal_intake","Dairy_kcal_supply","Non_rum_kcal_intake","Non_rum_kcal_supply",
                           "Vegetal_kcal_supply","Vegetal_kcal_intake",
                           #"FCR_rum_meat","FCF_rum_meat","FCR_dairy","FCF_dairy","FCR_monogastrics","FCF_monogastrics",
                           "GHG_eff_all","GHG_eff_CH4","GHG_eff_N2O","C_price",
                           "WPinc","Organic",
                           "N_management","NUEinc","NrecHousehold",
                           "P_management","PUEinc","PrecHousehold",
                           "fit","upr","lwr"))) %>% 
    dplyr::mutate(Pred_avg = value_base_year[i] + fit*value_base_year[i], # Converting % predictions to physical units
           Pred_upr = value_base_year[i] + upr*value_base_year[i],
           Pred_lwr = value_base_year[i] + lwr*value_base_year[i]) %>% 
    dplyr::mutate(SD_lwr = (Pred_avg-Pred_lwr)/2,
           SD_upr = (Pred_upr-Pred_avg)/2,
           SD_avg = rowMeans(data.frame(SD_lwr,SD_upr))) # Assumption that distribution is gaussian/normal (this can be confirmed through visual observation)

  
  # ## Additional statistics and uncertainty estimates based on the global or best model
  # if(i==2) {  # No unique risk distribution for pasture
  #   
  #   Results <- Final_preds
  #   # Converting log response ratio to % change
  # } else {
  #   
     Results <- Final_preds %>% 
       tibble::add_column(Indicator=ind[i],.before=1) %>% # Adding indicator name
       tibble::add_column(System=system[i],.before=1) # Adding PB system name
  #     mutate(RiskCol = ifelse(Pred_avg < Min,"GREEN", # Adding a risk colour for each Pred_avg
  #                             ifelse(Pred_avg >= Min & Pred_avg <= Mode,"YELLOW",
  #                                    ifelse(Pred_avg >= Mode & Pred_avg < Max,"ORANGE",
  #                                           ifelse(Pred_avg >= Max,"RED",NA)))))
  #     mutate(Safe = ifelse(Risk > 0.2,"NO","YES")) # Assuming 80% probability of meeting the boundary is acceptable 
  # }

## 2.2 Estimating risk by encompassing uncertainty from SD in model estimate and control variable triangular distribution
  
 fwrite(Results,paste0("Outputs/Meta-regression/Predictions/All_models_",today,"/",ind[i],"_",today,".csv"))
  # Can change to fwrite for speed but slight issues can occur with formatting
  
},future.seed = TRUE) # Option to prevent warnings and top make sure Proper Random Numbers are Produced in Parallel Processing: https://www.r-bloggers.com/2020/09/future-1-19-1-making-sure-proper-random-numbers-are-produced-in-parallel-processing/

proc.time() - ptm 
