##################################################
## LAND USE CHANGE EMISSIONS ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: To determine average LUC sequestration/emissions associated with cropland and pasture expansion/contraction based on IAMC database

#### 1.0 INITIALISATION ####

## 1.1  Loading necessary packages and functions

if (!require("pacman")) install.packages("pacman")
pacman::p_load(gridExtra,car,MASS,outliers,sjPlot,lme4,AICcmodavg,jtools,lmerTest,glmmTMB,ggpubr,
               LMERConvenienceFunctions,MuMIn,fitdistrplus,merTools,eatGet,tidyverse,pbkrtest,future.apply,Amelia,readxl,writexl,
               performance,hablar,reshape2,cowplot,DHARMa,predictmeans,remotes,robustlmm,cvms,ggeffects,
               groupdata2,caret,brms,interactions,wppExplorer,wpp2019,DescTools,here)

# Please note that eatGet may require manual installation and also requires 'reshape2' - see https://rdrr.io/rforge/eatGet/
#install.packages("eatGet", repos="http://R-Forge.R-project.org")

## 1.3 Loading necessary functions, datasets, and prediction parameters

`%notin%` <- Negate(`%in%`)
diagnostic_plots <- 'yes'
trim_outliers <- 'no'
ind <- "CO2_LUC"
i <- 1
j <- 1
CV_reps <- 5 # Number of times to perform repeat cross-validation
IAMC_database <- 'AR6' # OR change to 'AR6' - note that AR6 used the latest version (v1.1) published in November 2023
GWP_CH4 <- 27.2 #AR6, 27.2 (non-fossil origin)
GWP_N2O <- 273/1000 # AR6, 273 (updated N2O factor)
base_year <- 2010
end_year <- 2050
currentDate <- Sys.Date()

# Reading in dataset (either IAMC15 or AR6 v1.1- depending on what was selected above)

if(IAMC_database=='iamc15') {
  
  variables_land <- c("Land Cover|Cropland","Land Cover|Pasture","Land Cover|Forest",
                      "Land Cover|Other Arable Land","Land Cover|Other Land","Land Cover|Other Natural Land",
                      "Land Cover|Forest|Natural Forest","Land Cover|Forest|Afforestation and Reforestation")
  
  variables_GHG <- c("Emissions|CO2|AFOLU","Carbon Sequestration|Land Use|Afforestation","Price|Carbon")
  
  non_CO2_cat <- c("Emissions|N2O|AFOLU","Emissions|CH4|AFOLU")
  
}else{
  
  variables_land <- c("Land Cover|Cropland","Land Cover|Pasture","Land Cover|Forest",
                      "Land Cover|Other Arable Land","Land Cover|Other Land",#"Land Cover|Other Natural Land", # Other natural land removed from AR6
                      "Land Cover|Forest|Natural Forest","Land Cover|Forest|Afforestation and Reforestation")
  
  variables_GHG <- c("Emissions|CO2|AFOLU","Carbon Sequestration|Land Use|Afforestation","Price|Carbon")
  non_CO2_cat <- c("Emissions|CH4|AFOLU|Agriculture","Emissions|N2O|AFOLU|Agriculture")
}

#### 2. Loading IAMC data - either IAMC15 or AR6 database depending on choice at the beginning of script

if(IAMC_database=='iamc15') {
  
  IAMC_data <- read_excel(path = here("Input_data/Planetary_boundaries/iamc15_scenario_data_world_r2.0.xlsx"),sheet = "data") #Reading all IPCC scenarios
  metadata_IAMC_sel <- read_excel(path = here("Input_data/Planetary_boundaries/sr15_metadata_indicators_r2.0.xlsx"),sheet = "meta") %>%
    pull(scenario)
  
  year_prefix <- "" 
  
  IAMC_all <- read_excel(path = "iamc15_scenario_data_world_r2.0.xlsx",sheet = "data") # Reading the entire global IAMC database
  
}else{
  
  
  IAMC_data <- read.csv(unz(here("Input_data/Planetary_boundaries/1668008312256-AR6_Scenarios_Database_World_v1.1.csv.zip"), 
                            "AR6_Scenarios_Database_World_v1.1.csv"), header = TRUE,
                        sep = ",") 
  # Extracting metadata file
  unzip(zipfile=here("Input_data/Planetary_boundaries/1668008312256-AR6_Scenarios_Database_World_v1.1.csv.zip"), files = "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx", 
        exdir=here("Input_data/Planetary_boundaries/."))
  # Selecting the key columns
  metadata_IAMC_sel <- read_excel(path = here("Input_data/Planetary_boundaries/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"),sheet = "meta_Ch3vetted_withclimate") %>%
    pull(Scenario) %>% unique()
  
  year_prefix <- "X"
  
  IAMC_all <- IAMC_data %>%  # Selecting only vetted model runs (both 1.5/2C compliant and non-compliant)
    filter(Scenario %in% metadata_IAMC_sel)
}

##  2.2 Reformatting data into right format for statistical analysis

sel_models <- read.csv("Input_data/Planetary_boundaries/Selected_AR6_LUMs.csv") %>% # Choosing only partial equilibrium models with dynamic land use (see Guivarch et al. 2022, AR6)
  filter(Included=="Y") %>% 
  pull(Model)

time_snapshot <- seq(base_year,end_year,5) %>% as.character() %>% paste0(year_prefix,.) # Timeseries until 2050 to reflect modelling period

data_AFOLU <- IAMC_all %>% # Reading all IPCC scenarios
  filter(Model %in% sel_models) %>% # Choosing only comprehensive land-use models 
  filter(Variable %in% c(variables_land,variables_GHG)) %>%  # Choosing all variables
  dplyr::select(Model,Scenario,Variable,Unit,time_snapshot) %>% # Selecting relevant columns only
  dplyr::select_if(function(x) !(all(is.na(x)))) %>% # Removing NA columns
  magrittr::set_colnames(c("Model","Scenario","Variable","Unit",seq(base_year,end_year,5) %>% as.character())) # harmonising column names
  
delta_AFOLU <- data_AFOLU %>% 
  pivot_longer(cols = seq(base_year,end_year,5) %>% as.character(),names_to = "Year") %>% # Pivoting to longer format
  mutate(Year=as.numeric(Year)) %>% 
  dplyr::select(-Unit) %>%  # Allows pivot wider (Units are MtCO2 and million ha)
  pivot_wider(names_from = "Variable",values_from = value) %>% 
  group_by(Model,Scenario) %>% 
  dplyr::mutate(across(all_of(variables_land),~(.-lag(.))/5,.names="{col}_lag")) %>% # Calculating annual land use change
  magrittr::set_colnames(c("Model","Scenario","Year","CO2_LUC","Cropland","Forest","Other_arable","Pasture","Carbon_price",
                           "Natural_Forest","Other",#"Other_natural",
                           "CO2_Afforestation","Forest_afforestation",
                           "delta_Cropland","delta_Pasture","delta_Forest","delta_Other_arable","delta_Other",#"delta_Other_natural",
                           "delta_Natural_Forest","delta_Afforestation")) %>%
  filter(Year %notin% base_year) %>% 
  mutate(delta_Other_all = rowSums(across(delta_Other_arable:delta_Other), na.rm = T)) %>% # Models treat this differently so adding it up
  mutate(delta_Ag = delta_Cropland+delta_Pasture) %>% 
  mutate(CO2_LUC_net = case_when(CO2_Afforestation==0|is.na(CO2_Afforestation) ~ CO2_LUC,
                                 CO2_Afforestation!=0 ~ CO2_LUC-CO2_Afforestation)) %>% 
  mutate(delta_Forest_net = delta_Forest+delta_Ag) %>% # Delta forest above/beyond what is lost/gained from agriculture - outside scope of analysis 
  mutate(Model = as.factor(Model)) # To allow cross-validation

# For checking data
#write_xlsx(data_AFOLU,"data_AFOLU.xlsx")

#### 3.0 MODELLING ####

## 3.1 Preparing dataset and trimming outliers

list_var <- c("delta_Cropland","delta_Pasture","Carbon_price","Year") # Does not consider other land as input to the model
list_par <- paste0("scale(",list_var,")") %>% str_c(collapse = " + ")

# Exploratory plot parameters - allows using existing exploratory plot function
key_preds <- "delta_Cropland" # Variable names for plotting
key_preds_string <- "Delta Cropland" # Label names
effect_size <- "Delta"
model_specs <- "PB"
  
# Trimming outliers and cleaning up

if(trim_outliers=="yes"){ # If yes, residuals are trimmed
  source(here("Functions/Trim_outliers.R"))
  stat_df <- trim_residuals(delta_AFOLU,ind,i) %>% data.frame()

}else{
  stat_df <- delta_AFOLU %>% data.frame()
}

stat_df <- stat_df %>% dplyr::select(any_of(c("Model","Scenario","Year",
                                              ind[[i]],list_var))) %>% 
  drop_na() %>% # Dropping NAs before fitting models
  mutate(C_price_cat = case_when(Carbon_price>0 ~ "Yes", # Creating a categorical variable for the carbon price since many studies don't apply it at all
                                 TRUE ~ "No"))

# Plotting LUC results by model
if(diagnostic_plots=="yes"){ # if enables, plots all models/studies, histogram of indicator and relationship between chosen dependent variable and indicator
  source(here("Functions/Exploratory_plot_LUC.R"))
  exploratory_plots(stat_df,key_preds,key_preds_string,ind,i)
}

 ggplot(stat_df, aes_string(x = list_var[2], y = ind[i])) + geom_point() + facet_wrap(~ Model) +
   geom_smooth(method=lm)+
   ylab("CO2 LUC (Mt/yr")

# Establishing that mixed model is better than glm
m0.glm <- glm(stat_df[,ind[i]] ~ 1, family = gaussian, data = stat_df) # Simple linear model
m0.lmer <- lmer(stat_df[,ind[i]] ~ 1 + (1|Model), data = stat_df) # Linear mixed model with model/study as intercept
AIC(logLik(m0.glm))
AIC(logLik(m0.lmer))

## 3.2 Fitting models and look`ing at diagnostics to improve model

lm_global <- lm(paste0(ind[i]," ~ ",list_par),data=stat_df)#na.action = 'na.exclude') # Simple linear model for comparison
glmm_global <- lmer(paste0(ind[i]," ~ ",list_par,"+ (1|Model)"),data=stat_df,REML=TRUE)
glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",list_par,"+ (1|Model)"),data=stat_df,REML=TRUE)

r.squaredGLMM(glmm_global)

tab_model(glmm_global,glmm_global_robust)

check_model(glmm_global)

## 3.3 Checking alternative fixed effect structures through stepwise elimination
# Dropping variables to see effect on AIC and LRT
# 
alpha_fixed=0.2 # p-value threshold

drop1(glmm_global,test="Chisq")

glmm_global <- step(glmm_global,reduce.random = FALSE,alpha.fixed = alpha_fixed) %>% get_model() # Choosing model after inspecting drop1 results
write.csv(drop1(glmm_global,test="Chisq"),paste0("Outputs/Meta-regression/LMM_diagnostics/Drop1/Drop1",ind[i],model_specs[j],effect_size,currentDate,".csv"))

# Checking for colinearity
VIF_glmm <- as.data.frame(vif(glmm_global))
#VIF_glmm$`GVIF^(1/(2*Df))` <- VIF_glmm$`GVIF^(1/(2*Df))`^2 # Squaring last column as per: https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif
#colnames(VIF_glmm) <- c("Variable","VIF")
VIF_glmm
write.csv(VIF_glmm,paste0("Outputs/Meta-regression/LMM_diagnostics/VIFs/VIF",ind[i],model_specs[j],effect_size,currentDate,".csv")) 

# DHARMA residual plots

simulationOutput <- simulateResiduals(fittedModel = glmm_global)
plot(simulationOutput, quantreg = TRUE)
recalc_resid <- recalculateResiduals(simulationOutput, group = stat_df$Model)
plot(recalc_resid, quantreg = TRUE)
predictmeans::residplot(glmm_global, level=1,newwd = FALSE) # Residuals plot pop-out

# # Cook's distance to identify and visualise important residuals

cooksd <- cooks.distance(glmm_global)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- cooksd>4*mean(cooksd, na.rm=T)  # influential row numbers
stat_df[influential, ]
glmm_residuals <- stat_df[influential, ]
write.csv(glmm_residuals,paste0("Outputs/Meta-regression/LMM_diagnostics/Residuals/Residuals_before_trim",ind[i],model_specs[j],effect_size,currentDate,".csv"))

## 3.4 Trimming residuals and refitting model

outlier_cutoff <- 3# outlier cutoff - this is better than a priori trimming outliers which biases the result and keeps more sample
res <- resid(glmm_global, type = "pearson") # Extract standardized residuals
stat_df[abs(res)>outlier_cutoff,] # Visualise the residuals to be trimmed
new_data <- romr.fnc(glmm_global, stat_df, trim = outlier_cutoff) # Extreme residuals removed to enable better fit
stat_df <- new_data$data # New dataset
glmm_global <- lmer(paste0(ind[i]," ~ ",list_par[[i]][[j]],"+ (1|Model)"),data=stat_df,REML=TRUE)

r.squaredGLMM(glmm_global)

tab_model(glmm_global,glmm_global_robust)

check_model(glmm_global)

# DjHARMA residual plots - after trimming residuals

simulationOutput <- simulateResiduals(fittedModel = glmm_global)
plot(simulationOutput, quantreg = TRUE)
recalc_resid <- recalculateResiduals(simulationOutput, group = stat_df$Model)
plot(recalc_resid, quantreg = TRUE)
png(file=paste0("Outputs/Meta-regression/LMM_diagnostics/Residual_plot_",ind[i],".png"),
    width=250, height=200,units='mm',res = 600,pointsize = 20)
predictmeans::residplot(glmm_global, level=1, newwd = FALSE)#, group = "Model") # Residuals plot pop-out
dev.off()

# # Cook's distance to identify and visualise important residuals

cooksd <- cooks.distance(glmm_global)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- cooksd>4*mean(cooksd, na.rm=T)  # influential row numbers
stat_df[influential, ]
glmm_residuals <- stat_df[influential, ]
write.csv(glmm_residuals,paste0("Outputs/Meta-regression/LMM_diagnostics/Residuals/Residuals_after_trim",ind[i],model_specs[j],effect_size,currentDate,".csv"))

## 3.5  Cross-validation of the lmer model - uses custom CV_GLMM function  

source(here("Functions/CV_GLMM.R"))
CV_lmer_results <- CV_GLMM(stat_df,CV_reps,ind,i) # This will return a table with summary results of repeat cross-validation
CV_results <- data.frame(CV_lmer_results$RMSE,CV_lmer_results$`NRMSE(RNG)`,CV_lmer_results$AIC,CV_lmer_results$AICc,CV_lmer_results$r2m,CV_lmer_results$r2c)
write.csv(CV_results,paste0("Outputs/Meta-regression/Cross_validation/CV_results_",ind[i],model_specs[j],effect_size,currentDate,".csv"))

## 3.6  Saving models and model diagnostics for prediction and reporting  

# # Additional model diagnostics - Source: https://ourcodingclub.github.io/tutorials/mixed-models/
source(here("Functions/Visualise_effects_LUC.R"))# source: Additional model diagnostics - Source: https://ourcodingclub.github.io/tutorials/mixed-models/

# Saving model summaries
tab_model(glmm_global,glmm_global_robust,show.aic = TRUE,show.aicc = TRUE,
          file = paste0(here("Outputs/Meta-regression/Model_summaries//"),ind[i],model_specs[j],effect_size,"_",currentDate,".html"))

## SAVING models for future predictions

saveRDS(glmm_global,paste0("Outputs/Meta-regression/Fitted_models/Models/",ind[i],model_specs[j],effect_size,"_",currentDate,".rds")) # Saving final global model 
saveRDS(stat_df,paste0("Outputs/Meta-regression/Fitted_models/Datasets/",ind[i],model_specs[j],currentDate,".rds")) # Saving dataset for prediction

pdf(paste0("Outputs/Meta-regression/LMM_diagnostics/Residuals/",ind[i],model_specs[j],effect_size,"_","QQplotRES.pdf"))
qqnorm(residuals(glmm_global),main = paste0("GLMMs/Normal Q-Q plot - ",ind[i],model_specs[j]))
dev.off()
# 
pdf(paste0("Outputs/Meta-regression/LMM_diagnostics/Residuals/",ind[i],model_specs[j],effect_size,"_","ScaledRES.pdf"))
plot(glmm_global,main=paste0("GLMMs/",ind[i],model_specs[j], " - residuals"))
dev.off()

########################################## END ###############################################################################