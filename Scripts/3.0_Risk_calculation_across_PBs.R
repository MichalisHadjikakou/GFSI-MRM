#################### CALCULATING AVERAGE PB VALUES AND RISK ACROSS INDICATORS ##################################

## Author: Michalis Hadjikakou, Deakin University
## Purpose: Calculating average risk across boundaries based on predictions for each individual indicator

################################################# 1.0 INITIALISING #####################################################

## 1.1  Setting up modelling parameters and working directories
rm(list = ls())
PC <- 'denethor'

if(PC=='work_laptop') {
  setwd("C:/Users/Hadj/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("M:/Food-Systems/Meta_analysis/")
} else if (PC =='denethor') {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSI-MRM/GFSI-MRM/")
} else {
  setwd("C:/Users/Michalis/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/GFSS-MM/")  
}

simdate <- "2023-03-21"# Folder date
simdate_LUC <- "2022-05-16" # For LUC model which is fitted in this script
simdate_models <- "2023-03-15"
n <- 10000 # Number of random draws
Nsim <- 2000 # Number of bootstraps
#future::availableCores()

Base_year <- 2010
Scen_year <- 2050 
#LUC_rate_year <- 2050
LUC_rate_year <- mean(c(Base_year,Scen_year))# Mid-point between 2010 and 2050 to reflect changing dynamics of land use for snapshot analysis
Cropland_base <- 1519 # Source: FAOSTAT Cropland in base year
Pasture_base <- 3277 # Source: FAO - Permanent meadows and pastures in base year
LUC_base <- mean(c(4300,5500)) # Source: UNFCCC: https://unfccc.int/topics/land-use/the-big-picture/introduction-to-land-use, Units: Mt CO2
Carbon_price <- c(0,25,100,200) # Carbon price scenario price range based on IPCC AR6 WGIII: https://report.ipcc.ch/ar6wg3/pdf/IPCC_AR6_WGIII_SummaryForPolicymakers.pdf (Figure SPM.7: Overview of mitigation options and their estimated ranges of costs and potentials in 2030.)

# 1.2 Reading necessary files

All_predictions <- list.files(paste0("Outputs/Meta-regression/Predictions/All_models_",simdate,"/")) # All predictions

PBs <- read.csv("Outputs/Environmental_limits/All_PB_limits.csv")

# 1.3  Loading necessary packages and functions

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,tidyverse,fitdistrplus,propagate,extraDistr,distr,future.apply,merTools,readxl)

plan(multisession, workers = 50)

# Composite boundaries - derived using PB_distributions_2050 script

CC_2050_bound <- data.frame(Mean=PBs %>% filter(Indicator=="DirNonCO2LUC") %>% pull(Mode), 
                            SD = sum(PBs %>% filter(Indicator=="DirNonCO2LUC") %>% pull(Max) - PBs %>% filter(Indicator=="DirNonCO2LUC") %>% pull(Mode))/2) # 2050 boundaries for climate change 
# Based on 2050 1.5C trajectories in IAMC 2.0 database (see 'All_boundary_distributions_2050.R' script) - AR5 GWPs
LSS_2050_bound <- data.frame(Min=3019,Mode=3309,Max=5460) # 2050 boundaries for land-system change (cropland + pasture total)- in Mha based on assumptions explained in article SM

LUC_expansion <- 333 # t/ha (Mt/Mha) of average CO2 emissions per hectare of expansion (cropland or pasture) - Source: Clark et al., Science 370, 705-708 (2020)
LUC_abandonment <- 211 # t/ha (Mt/Mha) of average CO2 emissions per hectare of abandonment (cropland or pasture) - Source: Clark et al., Science 370, 705-708 (2020)

# https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables - calculating sum of two normal distributions


################################################# 2.0 TOTAL RISK CALCULATIONS FOR EACH PB #####################################################


## 2.1  Land-system change (Adding together cropland and pasture)
Cropland <- fread(paste0("Outputs/Meta-regression/Predictions/All_models_",simdate,"/",All_predictions[grep("Cropland",All_predictions)])) # Loading cropland predictions
Pasture <-  fread(paste0("Outputs/Meta-regression/Predictions/All_models_",simdate,"/",All_predictions[grep("Pasture",All_predictions)])) # Loading pasture predictions

Total_Area <- Cropland %>% 
  dplyr::select(System,Pop_levels,Diet,Vegetal_kcal_supply,Waste,Yield_levels,Feed_efficiency,Feed_composition,Pred_avg,Pred_upr,Pred_lwr) %>% #C_price) %>% 
  dplyr::rename(Pred_avg_crop=Pred_avg,Pred_upr_crop=Pred_upr,Pred_lwr_crop=Pred_lwr,Plant_kcal = Vegetal_kcal_supply) %>% #Carbon_price=C_price) %>% 
  cbind(Pasture %>% dplyr::select(Pred_avg,Pred_upr,Pred_lwr) %>% 
          magrittr::set_colnames(c("Pred_avg_pasture","Pred_upr_pasture","Pred_lwr_pasture"))) %>% 
  mutate(Pred_avg=Pred_avg_crop+Pred_avg_pasture,
         SD_crop = ((Pred_avg_crop-Pred_lwr_crop)+(Pred_upr_crop-Pred_avg_crop))/2/2,# Average of 2SD divided by 2 = 1SD
         SD_pasture = ((Pred_avg_pasture-Pred_lwr_pasture)+(Pred_upr_pasture-Pred_avg_pasture))/2/2, # Average of 2SD divided by 2 = 1SD
         Pred_SD= sqrt(SD_crop^2+SD_pasture^2)) %>% # Calculating square root of average variances 
  add_column(LSS_2050_bound) %>% # Adding boundary distribution properties 
  add_column(Indicator="TotalAgArea",.after='System') %>% 
  mutate(RiskCol = ifelse(Pred_avg < Min,"GREEN", # Adding a risk colour for each Pred_avg
                          ifelse(Pred_avg >= Min & Pred_avg <= Mode,"YELLOW",
                                 ifelse(Pred_avg >= Mode & Pred_avg < Max,"ORANGE",
                                        ifelse(Pred_avg >= Max,"RED",NA))))) %>% 
  mutate(Pred_upr = Pred_avg+(2*Pred_SD),
         Pred_lwr = Pred_avg-(2*Pred_SD),
         .after='Pred_avg')
#mutate(Safe = ifelse(Risk > 0.2,"NO","YES")) %>% # Assuming 80% probability of meeting the boundary is acceptable 
# rowwise() %>% 
# mutate(Risk_Joint = sum((rnorm(n,Pred_avg,Pred_SD))>
#                           (rtriang(n,Min,Max,Mode)))/n) 

Total_Area$Risk <- future_sapply(1:dim(Total_Area)[1],function(x) # Calculating risk in parallel - simulation to calculate intersection of two distributions
  sum((rnorm(n,Total_Area$Pred_avg[x],Total_Area$Pred_SD[x]))>
        (rtriang(n,Total_Area$Min[x],Total_Area$Max[x],Total_Area$Mode[x])))/n,future.seed = TRUE)

fwrite(Total_Area,paste0("Outputs/Risk_estimates/Land_system_change_",Sys.Date(),".csv"))

## 2.2  Freshwater Use

Water <- fread(paste0("Outputs/Meta-regression/Predictions/All_models_",simdate,"/",All_predictions[grep("Water",All_predictions)])) %>%  # Loading cropland predictions %>% 
  dplyr::rename(WUEinc=WPinc) %>% 
  dplyr::select(System,Indicator,Pop_levels,Diet,Vegetal_kcal_supply,Waste,Yield_levels,Feed_efficiency,Feed_composition,WUEinc,
                Pred_avg:SD_avg) %>% 
  dplyr::rename(Plant_kcal = Vegetal_kcal_supply) %>% 
  mutate(Pred_SD = ((Pred_avg-Pred_lwr)+(Pred_upr-Pred_avg))/2/2,.after = Pred_lwr) %>% # Average SD across upper and lower prediction intervals 
  left_join(PBs) %>% 
  mutate(RiskCol = ifelse(Pred_avg < Min,"GREEN", # Adding a risk colour for each Pred_avg
                          ifelse(Pred_avg >= Min & Pred_avg <= Mode,"YELLOW",
                                 ifelse(Pred_avg >= Mode & Pred_avg < Max,"ORANGE",
                                        ifelse(Pred_avg >= Max,"RED",NA)))))
  
Water$Risk <- future_sapply(1:dim(Water)[1],function(x) # Calculating risk in parallel - simulation to calculate intersection of two distributions
  sum((rnorm(n,Water$Pred_avg[x],Water$Pred_SD[x]))>
        (rtriang(n,Water$Min[x],Water$Max[x],Water$Mode[x])))/n,future.seed = TRUE)

fwrite(Water,paste0("Outputs/Risk_estimates/Freshwater_Use_",Sys.Date(),".csv")) # Water use efficiency kept as quantitative

## 2.3  Climate change (Adding together CH4, N2O and LUC)  

CH4 <- fread(paste0("Outputs/Meta-regression/Predictions/All_models_",simdate,"/",All_predictions[grep("CH4",All_predictions)]))
N2O <- fread(paste0("Outputs/Meta-regression/Predictions/All_models_",simdate,"/",All_predictions[grep("N2O",All_predictions)]))

# Calculating annual LUC emissions estimates based on land-system change scenarios - loading fitted LMM and dataset
# Loading global model,top models and underlying dataset
glmm_global <- readRDS(paste0("Outputs/Meta-regression/Fitted_models/Models/",simdate_models,"/","CO2_LUCPBDelta_",simdate_LUC,".rds")) # Loading global model
stat_df <- readRDS(paste0("Outputs/Meta-regression/Fitted_models/Datasets/",simdate_models,"/","CO2_LUCPB",simdate_LUC,".rds")) # Loading final dataframe

# Method 1 - LUC emissions prediction using LUC model trained on IAMC database (see "LUC_model.R" script)

# Data reformatting/wrangling to allow predictions for lower/average/upper land use estimates

LUC_GHG <- Total_Area %>% 
  dplyr::rename(Average_crop=Pred_avg_crop, Upper_crop=Pred_upr_crop, Lower_crop=Pred_lwr_crop,
                Average_pasture=Pred_avg_pasture, Upper_pasture=Pred_upr_pasture, Lower_pasture=Pred_lwr_pasture) %>% 
  pivot_longer(cols = Average_crop:Lower_pasture,
               names_to = c("Land_scenarios","Land_type"),
               names_sep = "_",
               values_to = c("Land_area")) %>% 
  pivot_wider(names_from = "Land_type",values_from = "Land_area") %>% 
  dplyr::rename(Cropland = crop, Pasture = pasture) %>% 
  mutate(delta_Cropland = (Cropland-Cropland_base)/(Scen_year-Base_year),
       delta_Pasture = (Pasture-Pasture_base)/(Scen_year-Base_year)) %>% 
  add_column(Year = LUC_rate_year,
             Model = "~0", # Predict from mean intercept
             delta_Forest_net = 0) %>%  # Only consider forest deviation due to agricultural expansion/contraction
  expand_grid(Carbon_price)# Adding carbon price scenarios

# Predict using LUC_GHG_model
Pred_int <- suppressWarnings(predictInterval(merMod = glmm_global,
                                             newdata = LUC_GHG,
                                             n.sims = Nsim,
                                             which='full',seed = 101,
                                             level=0.95,
                                             include.resid.var = T,
                                             stat="mean",returnSims = FALSE)) 
            #mutate(SD = ((fit-lwr)+(upr-fit))/2/2)

# Attaching predictions to data.frame and calculating average distribution parameters
pred_dist <- LUC_GHG$Land_scenarios %>% unique() # Prediction distributions

LUC_results <- future_lapply(seq_along(pred_dist),function(x) LUC_GHG %>%
  add_column(Pred_int) %>% 
  mutate(Variance = (((fit-lwr)+(upr-fit))/2/2)^2) %>% # Calculating average variance to allow estimate of average distribution properties
  dplyr::filter(Land_scenarios==pred_dist[x]) %>% 
  dplyr::select(c(fit,Variance))) 
  #pivot_wider(names_from = "Land_scenarios",values_from = c(fit,SD)) %>%  # Pivot wider to uncover all predictions
  
Average_LUC_dist <- (Reduce("+", LUC_results) / length(LUC_results)) %>% # Average of standard deviations and variances 
    mutate(SD=sqrt(Variance)) %>% 
    magrittr::set_colnames(c("Pred_avg_LUC","Variance_LUC","SD_LUC"))

# Compiling final LUC GHG results dataframe - this will enable merging with other GHGs
Final_LUC_estimates <- LUC_GHG %>% filter(Land_scenarios=="Average") %>% 
  dplyr::select(Pop_levels,Diet,Plant_kcal,Waste,Yield_levels,Feed_efficiency,Feed_composition,Carbon_price) %>% 
  add_column(Average_LUC_dist)
    
# Method 2 - Based on historical factors in Clark et al. (2020) using only cropland

# LUC_GHG_cropland <- Cropland %>%
#   mutate(LUC_avg = Pred_avg - Cropland_base,
#          LUC_lwr = Pred_lwr - Cropland_base,
#          LUC_upr = Pred_upr - Cropland_base) %>% 
#   mutate(across(LUC_avg:LUC_upr,~ case_when(.>0 ~ .*LUC_expansion/(Scen_year-Base_year),
#                                             .<0 ~ .*LUC_abandonment/(Scen_year-Base_year),
#                                             .==0 ~ 0),.names = "{col}_GHG")) %>% # Create new columns with GHG suffix 
#   mutate(SD_GHG_H=LUC_upr_GHG-LUC_avg_GHG,
#          SD_GHG_L=LUC_avg_GHG-LUC_lwr_GHG) %>% 
#   mutate(Diff_SD = SD_GHG_H - SD_GHG_L) # Checking differences between upper and lower variances

## Adding up the distributions of all the greenhouse gas predictions (CH4, N2O & CO2) - using method 1

NonCO2LUC <- CH4 %>% 
  dplyr::select(System,Pop_levels,Diet,Vegetal_kcal_supply,Waste,Yield_levels,Feed_efficiency,Feed_composition,C_price,Organic,Pred_avg,Pred_upr,Pred_lwr) %>% 
  dplyr::rename(Carbon_price=C_price,Pred_avg_CH4=Pred_avg,Pred_upr_CH4=Pred_upr,Pred_lwr_CH4=Pred_lwr,Plant_kcal = Vegetal_kcal_supply) %>% 
  cbind(N2O %>% dplyr::select(Pred_avg,Pred_upr,Pred_lwr) %>% 
          magrittr::set_colnames(c("Pred_avg_N2O","Pred_upr_N2O","Pred_lwr_N2O"))) %>% 
  mutate(SD_CH4 = ((Pred_avg_CH4-Pred_lwr_CH4)+(Pred_upr_CH4-Pred_avg_CH4))/2/2,
         SD_N2O =((Pred_avg_N2O-Pred_lwr_N2O)+(Pred_upr_N2O-Pred_avg_N2O))/2/2) %>% 
  dplyr::filter(Organic==0) %>% dplyr::select(-Organic) %>% 
  left_join(Final_LUC_estimates,by=c("Pop_levels","Diet","Plant_kcal","Waste","Yield_levels","Feed_efficiency","Feed_composition","Carbon_price")) %>% # Ensuring compatibility
  add_column(CC_2050_bound) %>% # Adding boundary distribution properties
  add_column(Indicator="NonCO2+LUC",.after = "System") %>% 
  mutate(Pred_avg = Pred_avg_CH4 + Pred_avg_N2O + Pred_avg_LUC, # Sum of means
         Pred_SD = sqrt(SD_CH4^2 + SD_N2O^2 + SD_LUC^2)) %>% # Sum of variances of individual GHG predictions
  mutate(RiskCol = ifelse(Pred_avg < Mean-(2*SD),"GREEN", # Adding a risk colour for each Pred_avg
                          ifelse(Pred_avg >= Mean-(2*SD) & Pred_avg <= Mean,"YELLOW",
                                 ifelse(Pred_avg >= Mean & Pred_avg < Mean+(2*SD),"ORANGE",
                                        ifelse(Pred_avg >= Mean+(2*SD),"RED",NA)))))
#If distribution is triangular:
  # mutate(RiskCol = ifelse(Pred_avg < Min,"GREEN", # Adding a risk colour for each Pred_avg
  #                       ifelse(Pred_avg >= Min & Pred_avg <= Mode,"YELLOW",
  #                              ifelse(Pred_avg >= Mode & Pred_avg < Max,"ORANGE",
  #                                     ifelse(Pred_avg >= Max,"RED",NA))))) 

NonCO2LUC$Risk <- future_sapply(1:dim(NonCO2LUC)[1],function(x) # Calculating risk in parallel - simulation to calculate intersection of two distributions
  sum((rnorm(n,NonCO2LUC$Pred_avg[x],NonCO2LUC$Pred_SD[x]))>
        (rnorm(n,NonCO2LUC$Mean[x],NonCO2LUC$SD[x])))/n,future.seed = TRUE)
#If distribution is triangular:
        #(rtriang(n,NonCO2LUC$Min[x],NonCO2LUC$Max[x],NonCO2LUC$Mode[x])))/n,future.seed = TRUE)

fwrite(NonCO2LUC,paste0("Outputs/Risk_estimates/Climate_Change_",Sys.Date(),".csv")) # Water use efficiency kept as quantitative

# Writing also LUC_results for use in SI barplots
LUC_indicator_results <- Final_LUC_estimates %>%
  rename(SD_avg = SD_LUC) %>% 
  add_column(Indicator = "CO2_LUC",.before = 1) %>% 
  add_column(System = "Climate change",.before = 1) %>% 
  mutate(fit = (Pred_avg_LUC/LUC_base)-1,
         upr = ((Pred_avg_LUC+(2*SD_avg))/LUC_base)-1,
         lwr = ((Pred_avg_LUC-(2*SD_avg))/LUC_base)-1) # Calculating deviation from base year

fwrite(LUC_indicator_results,paste0("Outputs/Meta-regression/Predictions/All_models_",simdate,"/CO2_LUC_",Sys.Date(),".csv"))

## 2.4  Biogeochemical flows (Taking the average/max risk across all indicators?) 

N_indicators <- c("Nfert","Nsurplus")
P_indicators <- c("Pfert","Psurplus")
All_nutrients <- c(N_indicators,P_indicators) # String with all biogeochemical flow indicators

Nutrients <- future_lapply(seq_along(All_nutrients),function(x)
  fread(paste0("Outputs/Meta-regression/Predictions/All_models_",simdate,"/",All_predictions[grep(All_nutrients[x],All_predictions)])))
  
# Fractions to use in estimating mean/upper/lower predictions for delivery/export

fr_mouth_river <- 0.413 # Fraction of P river load that becomes P export at mouth of river (average of SSP1-5 from Beusen et al. 2022, GEC)
fr_mouth_river_low <- 0.403
fr_mouth_river_high <- 0.418

fr_runoff_surplus <- 0.435  # Fraction of P surplus that becomes runoff (average of SSP1-5 from Beusen et al. 2022, GEC)
fr_runoff_surplus_low <- 0.3438 # Lowest fraction across SSPs 
fr_runoff_surplus_high <- 0.5097 # Highest fraction across SSPs

# Converting surplus to runoff/load estimates and inserting back to main nutrients list

# Pexport <- Nutrients[[which(All_nutrients=="Psurplus")]] %>% # Selecting Psurplus only
#   data.frame() %>% 
#   mutate(Pred_avg = Pred_avg*fr_runoff_surplus*fr_mouth_river, # Calculating estimates of ocean P export
#          Pred_upr = Pred_upr*fr_runoff_surplus_high*fr_mouth_river_high,
#          Pred_lwr = Pred_lwr*fr_mouth_river_low*fr_mouth_river_low) %>% 
#   mutate(Indicator="Pexport")       

Pinstream <- Nutrients[[which(All_nutrients=="Psurplus")]] %>% # Selecting Psurplus only
  data.frame() %>% 
  mutate(Pred_avg = ((Pred_avg*fr_runoff_surplus)+(Pred_avg*fr_runoff_surplus*fr_mouth_river))/2, # Calculating estimates of ocean P export
         Pred_upr = ((Pred_upr*fr_runoff_surplus_high)+(Pred_upr*fr_runoff_surplus_high*fr_mouth_river_high))/2,
         Pred_lwr = ((Pred_lwr*fr_mouth_river_low)+(Pred_lwr*fr_mouth_river_low*fr_mouth_river_low))/2) %>% 
  mutate(Indicator="Pinstream") 

Nutrients[[which(All_nutrients=="Psurplus")]] <- Pinstream # Adding it back

Nutrients <- Nutrients %>%  
  purrr::reduce(full_join) %>% 
  #dplyr::filter(Organic==0) %>% # Only choosing 0 organic (Organic not fitted as a predictor - the rest of the rows are replicates)
  #dplyr::select(-Organic) %>% 
  mutate(Pred_SD = ((Pred_avg-Pred_lwr)+(Pred_upr-Pred_avg))/2/2) %>%  # Calculating standard deviations (to be used for risk estimates)
  left_join(PBs,by="Indicator") %>% 
  mutate(RiskCol = ifelse(Pred_avg < Min,"GREEN", # Adding a risk colour for each Pred_avg
                          ifelse(Pred_avg >= Min & Pred_avg <= Mode,"YELLOW",
                                 ifelse(Pred_avg >= Mode & Pred_avg < Max,"ORANGE",
                                        ifelse(Pred_avg >= Max,"RED",NA)))))

Nutrients$Risk <- future_sapply(1:dim(Nutrients)[1],function(x) # Calculating risk in parallel - simulation to calculate intersection of two distributions
  sum((rnorm(n,Nutrients$Pred_avg[x],Nutrients$Pred_SD[x]))>
        (rtriang(n,Nutrients$Min[x],Nutrients$Max[x],Nutrients$Mode[x])))/n,future.seed = TRUE)
            
Pooled_N <- Nutrients %>%  
  dplyr::filter(Indicator %in% N_indicators) %>% 
  dplyr::select(System,Indicator,Pop_levels,Diet,Vegetal_kcal_supply,Waste,Yield_levels,Feed_efficiency,Feed_composition,N_management,
                Pred_avg:RiskCol,Pred_SD,Risk) %>% 
  dplyr::rename(Plant_kcal = Vegetal_kcal_supply) %>% 
  mutate(System = "Biogeochemical Flows N")
   
P_indicators <- c("Pfert","Pinstream") # Replacing surplus with Pinstream

Pooled_P <- Nutrients %>%  
  dplyr::filter(Indicator %in% P_indicators) %>% 
  dplyr::select(System,Indicator,Pop_levels,Diet,Vegetal_kcal_supply,Waste,Yield_levels,Feed_efficiency,Feed_composition,P_management,
                Pred_avg:RiskCol,Pred_SD,Risk) %>% 
  dplyr::rename(Plant_kcal = Vegetal_kcal_supply) %>% 
  #mutate(P_management = dplyr::recode(P_management,`+5% PUE,+60% REC`="+15% PUE,+60% REC")) %>%  # Recoding to correct typo
  mutate(System = "Biogeochemical Flows P")

# Average risk for all nutrients 
Pooled_Nutrients <- Pooled_N %>%
  mutate(System="Biogeochemical Flows") %>% 
  dplyr::select(-Indicator) %>% 
  mutate(Risk = (Pooled_N$Risk+Pooled_P$Risk)/2) # Calculating average of N and P risks - keeping in N strategy

# Writing results
fwrite(Pooled_N,paste0("Outputs/Risk_estimates/Biogeochemical_Flows_N_",Sys.Date(),".csv")) 
fwrite(Pooled_P,paste0("Outputs/Risk_estimates/Biogeochemical_Flows_P_",Sys.Date(),".csv")) 
fwrite(Pooled_Nutrients,paste0("Outputs/Risk_estimates/Biogeochemical_Flows_",Sys.Date(),".csv")) 

##################################################################### END #######################################################################################################

