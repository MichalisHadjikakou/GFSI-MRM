##################################################
## FUTURE FOOD SCENARIOS - Predict_scenario ##
##################################################

# Author: Michalis Hadjikakou, Deakin University m.hadjikakou@deakin.edu.au)
# Purpose: Quickly create new predictions with saved models

################################################# 1.0 INITIALISING #####################################################

## 1.1 Setting key parameters and loading packages

n <- 10000 # Number of random draws for distributions/simulations
Nsim = 2000 # Number of bootstraps
set.seed(123) # Ensures reproducibility of bootstrap results
simdate <- "2024-07-01"
today <- Sys.Date() # Today's date

# Prediction parameters
Base_year <- 2010
Scenario_year <- 2050
Pred_levels <- 4 # 4 unique levels for each predictor

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(extraDistr,here,data.table,gridExtra,car,MASS,outliers,propagate,sjPlot,lme4,glmm,hablar,wcde,lmerTest,readxl,
               LMERConvenienceFunctions,MuMIn,fitdistrplus,merTools,eatGet,tidyverse,pbkrtest,future.apply)

# Indicator and broad scenario names
ind <- c("Cropland","Pasture","Water","CH4","N2O", # Forest and LUC are derived post-hoc (on the basis of pasture and cropland)
         "Nfert","Pfert") # Using 'Nfert' instead of "Nfix' as per Springmann et al. (2018) 
system <- c(rep("Land-System Change",2),"Freshwater Use",rep("Climate Change",2),
           rep("Biogeochemical Flows",2)) # Please note that LUC is produced in another script

effect_size <- rep("LnR",length(ind)) # All indicator use LnR as effect size metric 
effect_size[ind=="Pasture"] <- "delta%" # Except for pasture
  
Base_year_data <- readxl::read_xlsx(here("Input_data/Base_year_values.xlsx"))
value_base_year <- Base_year_data$Value

## 1.3 Loading all static files required for predictions

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
plan(multisession, workers = length(ind)) # Requesting n cores where n = number of indicators
future_sapply(seq_along(ind), function(i) {
  
  #print(i) - use for debugging

  # Creating bespoke prediction dataset for each indicator
  Ind_preds <- Pred_data_indicator(Pred_data,i) %>% 
    dplyr::mutate(Model = "~0",
           Delta_initial = 0) # # Adding column names and specifying random intercept =~0# Loading indicator-specific prediction dataset
  # Loading global model,top models and underlying dataset
  
  if(ind[i]=="Pasture"){
    Ind_preds <- Ind_preds %>% 
      add_column(Pasture_yield=0) # We assume constant pasture yield into the future
  }
  
  glmm_global <- readRDS(paste0("Outputs/Meta-regression/Fitted_models/Models/",simdate,"/",All_models[grep(ind[i],All_models)])) # Loading global model
  stat_df <- readRDS(paste0("Outputs/Meta-regression/Fitted_models/Datasets/",simdate,"/",All_datasets[grep(ind[i],All_datasets)])) # Loading final dataframe
  
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
    dplyr::select(any_of(c("Pop_levels","Diet","Waste","Yield_levels","Feed_efficiency","Feed_composition",
                           "Vegetal_kcal_supply",
                           "GHG_eff_CH4","GHG_eff_N2O","C_price",
                           "WPinc",
                           "N_management","NUEinc","NrecHousehold",
                           "P_management","PUEinc","PrecHousehold",
                           "fit","upr","lwr"))) %>% 
    dplyr::mutate(Pred_avg = value_base_year[i] + fit*value_base_year[i], # Converting % predictions to physical units
           Pred_upr = value_base_year[i] + upr*value_base_year[i],
           Pred_lwr = value_base_year[i] + lwr*value_base_year[i]) %>% 
    dplyr::mutate(SD_lwr = (Pred_avg-Pred_lwr)/2,
           SD_upr = (Pred_upr-Pred_avg)/2,
           SD_avg = rowMeans(data.frame(SD_lwr,SD_upr))) # Assumption that distribution is gaussian/normal (this can be confirmed through visual observation)

     Results <- Final_preds %>% 
       tibble::add_column(Indicator=ind[i],.before=1) %>% # Adding indicator name
       tibble::add_column(System=system[i],.before=1) # Adding PB system name
     

# Recycling adjustments for nitrogen and phosphorus 
  if(ind[i]=="Nfert") { 
       
    Results <- Results %>% 
      rename(Nrec = NrecHousehold) %>% 
      mutate(across(Pred_avg:SD_avg,~ .x*(1-Nrec)))
  }
     
  if(ind[i]=="Pfert") { 
       
    Results <- Results %>% 
      rename(Prec = PrecHousehold) %>% 
      mutate(across(Pred_avg:SD_avg,~ .x*(1-Prec)))
  }

## 2.2 Estimating risk by encompassing uncertainty from SD in model estimate and control variable triangular distribution
  
 fwrite(Results,paste0("Outputs/Meta-regression/Predictions/All_models_",today,"/",ind[i],"_",today,".csv"))
  
},future.seed = TRUE) # Option to prevent warnings and top make sure Proper Random Numbers are Produced in Parallel Processing: https://www.r-bloggers.com/2020/09/future-1-19-1-making-sure-proper-random-numbers-are-produced-in-parallel-processing/

proc.time() - ptm 
