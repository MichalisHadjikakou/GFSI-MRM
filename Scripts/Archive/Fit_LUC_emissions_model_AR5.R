##################################################
## LAND USE CHANGE EMISSIONS ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: To determine average LUC sequestration/emissions associated with cropland and pasture expansion/contraction based on IAMC database

#### 1.0 INITIALISATION ####

## 1.1 Loading working directory

PC <- "denethor"

if(PC=='work_laptop') {
  setwd("C:/Users/Hadj/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("M:/Food-Systems/Meta_analysis/")
} else if (PC =='denethor') {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSS-MM/")
} else {
  setwd("C:/Users/Michalis/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/GFSS-MM/")  
}

## 1.2  Loading necessary packages and functions

if (!require("pacman")) install.packages("pacman")
pacman::p_load(extraDistr,data.table,gridExtra,car,MASS,outliers,propagate,sjPlot,lme4,glmm,glmmTMB,AICcmodavg,jtools,lmerTest,
               LMERConvenienceFunctions,MuMIn,fitdistrplus,merTools,eatGet,tidyverse,pbkrtest,future.apply,Amelia,readxl,writexl,
               performance,hablar,Rcpp,reshape2,cowplot,DHARMa,predictmeans,remotes,bernr,robustlmm,cvms,ggeffects,
               groupdata2,caret,brms,interactions,wppExplorer,wpp2019,scater)

# Please note that eatGet may require manual installation and also requires 'reshape2' - see https://rdrr.io/rforge/eatGet/
#install.packages("eatGet", repos="http://R-Forge.R-project.org")

## 1.3 Loading necessary functions, datasets, and prediction parameters

`%notin%` <- Negate(`%in%`)
diagnostic_plots <- 'yes'
trim_outliers <- 'yes'
ind <- "CO2_LUC"
i <- 1
j <- 1
CV_reps <- 5 # Number of times to perform repeat cross-validation
IAMC_database <- 'AR6' # OR change to 'AR6'
GWP_CH4 <- 27.2 #AR6, 27.2 (non-fossil origin)
GWP_N2O <- 273/1000 # AR6, 273 (updated N2O factor)
currentDate <- Sys.Date()

# Scenario family strings - allows selecting and fitting distributions for 1.5 and 2C seperately or together

# Scenario family strings - allows selecting and fitting distributions for 1.5 and 2C seperately or together
if(IAMC_database=='iamc15') {
  
  # only_1.5 <- c("1.5C low overshoot","Below 1.5C","1.5C high overshoot")
  # only_2 <- c("Higher 2C","Lower 2C")
  # all <- c(only_1.5,only_2)
  # 
  # sel_scens <- all # Selecting 1.5oC and 2oC scenarios - modify accordingly
  
  variables_land <- c("Land Cover|Cropland","Land Cover|Pasture","Land Cover|Forest",
                      "Land Cover|Other Arable Land","Land Cover|Other Land","Land Cover|Other Natural Land",
                      "Land Cover|Forest|Natural Forest","Land Cover|Forest|Afforestation and Reforestation")
  
  variables_GHG <- c("Emissions|CO2|AFOLU","Carbon Sequestration|Land Use|Afforestation","Price|Carbon")
  
  non_CO2_cat <- c("Emissions|N2O|AFOLU","Emissions|CH4|AFOLU")
  
}else{
  
  # only_1.5 <- c("C1: limit warming to 1.5°C (>50%) with no or limited overshoot")
  # only_2 <- c("C3: limit warming to 2°C (>67%)")
  # all <- c(only_1.5,only_2)
  # 
  # sel_scens <- all # Selecting only 1.5oC scenarios - modify accordingly
  
  variables_land <- c("Land Cover|Cropland","Land Cover|Pasture","Land Cover|Forest",
                      "Land Cover|Other Arable Land","Land Cover|Other Land","Land Cover|Other Natural Land",
                      "Land Cover|Forest|Natural Forest","Land Cover|Forest|Afforestation and Reforestation")
  
  variables_GHG <- c("Emissions|CO2|AFOLU","Carbon Sequestration|Land Use|Afforestation","Price|Carbon")
  non_CO2_cat <- c("Emissions|CH4|AFOLU|Agriculture","Emissions|N2O|AFOLU|Agriculture")
}

#### 2. Loading IAMC data - either IAMC15 or AR6 database depending on choice at the beginning of script

if(IAMC_database=='iamc15') {
  
  IAMC_data <- read_excel(path = "Input_data/Planetary_boundaries/iamc15_scenario_data_world_r2.0.xlsx",sheet = "data") #Reading all IPCC scenarios
  metadata_IAMC_sel <- read_excel(path = "Input_data/Planetary_boundaries/sr15_metadata_indicators_r2.0.xlsx",sheet = "meta") %>%
    filter(category %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
    pull(scenario)
  
  year_prefix <- "" 
  
  IAMC_all <- read_excel(path = "iamc15_scenario_data_world_r2.0.xlsx",sheet = "data") # Reading the entire global IAMC database
  
}else{
  
  master <- as.character(unzip("Input_data/Planetary_boundaries/1648976687084-AR6_Scenarios_Database_World_v1.0.zip", list = TRUE)$Name)
  # load the first file "file1.csv"
  IAMC_data <- read.csv(unz("Input_data/Planetary_boundaries/1648976687084-AR6_Scenarios_Database_World_v1.0.zip", 
                            "AR6_Scenarios_Database_World_v1.0.csv"), header = TRUE,
                        sep = ",") 
  # Extracting metadata file
  unzip(zipfile="Input_data/Planetary_boundaries/1648976687084-AR6_Scenarios_Database_World_v1.0.zip", files = "AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx", 
        exdir="Input_data/Planetary_boundaries/.")
  
  # metadata_IAMC <- read_excel(path = "Input_data/Planetary_boundaries/AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx",sheet = "meta_Ch3vetted_withclimate") %>%
  #   #filter(Category_name %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
  #   pull(Scenario) %>% unique()
  
  metadata_IAMC_sel <- read_excel(path = "Input_data/Planetary_boundaries/AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx",sheet = "meta_Ch3vetted_withclimate") %>%
    #filter(Category_name %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
    pull(Scenario) %>% unique()
  
  year_prefix <- "X"
  
  IAMC_all <- IAMC_data %>%  # Selecting only vetted model runs (both 1.5/2C compliant and non-compliant)
    filter(Scenario %in% metadata_IAMC_sel)
}

base_year <- 2010
end_year <- 2050
time_snapshot <- seq(base_year,end_year,5) %>% as.character() %>% paste0(year_prefix,.) # Timeseries until 2050 to reflect modelling period

##  2.2 Reformatting data into right format for statistical analysis 

data_AFOLU <- IAMC_all %>% # Reading all IPCC scenarios
  #filter(Scenario %in% metadata_IAMC) %>% # Choosing appropriate scenarios
  filter(Variable %in% c(variables_land,variables_GHG)) %>%  # Choosing all variables
  dplyr::select(Model,Scenario,Variable,Unit,time_snapshot) %>% # Selecting relevant columns only
  dplyr::select_if(function(x) !(all(is.na(x))))# Removing NA columns
  
delta_AFOLU <- data_AFOLU %>% 
  pivot_longer(cols = time_snapshot,names_to = "Year") %>% # Pivoting to longer format
  mutate(Year=as.numeric(Year)) %>% 
  dplyr::select(-Unit) %>%  # Allows pivot wider (Units are MtCO2 and million ha)
  pivot_wider(names_from = "Variable",values_from = value) %>% 
  group_by(Model,Scenario) %>% 
  dplyr::mutate(across(variables_land,~(.-lag(.))/5,.names="{col}_lag")) %>% 
  dplyr::slice(-1) %>% # Remove first row (base year) of every group 
  magrittr::set_colnames(c("Model","Scenario","Year","CO2_LUC","Cropland","Forest","Other_arable","Pasture","Carbon_price",
                           "Natural_Forest","Other","Other_natural",
                           "CO2_Afforestation","Forest_afforestation",
                           "delta_Cropland","delta_Pasture","delta_Forest","delta_Other_arable","delta_Other","delta_Other_natural",
                           "delta_Natural_Forest","delta_Afforestation")) %>% 
  mutate(delta_Other_all = rowSums(across(delta_Other_arable:delta_Other_natural), na.rm = T)) %>% # Models treat this differently so adding it up
  mutate(TotalArea = delta_Cropland+delta_Pasture) %>% 
  mutate(CO2_LUC_net = case_when(CO2_Afforestation==0|is.na(CO2_Afforestation) ~ CO2_LUC,
                                 CO2_Afforestation!=0 ~ CO2_LUC-CO2_Afforestation)) %>% 
  mutate(delta_Forest_net = delta_Forest+TotalArea) %>% # Delta forest above/beyond what is lost/gained from agriculture
  mutate(Model = as.factor(Model)) # To allow cross-validation
  #filter(CO2_LUC<0)

#write_xlsx(delta_AFOLU,"delta_AFOLU.xlsx")

#### 3.0 MODELLING ####

## 3.1 Preparing dataset and trimming outliers

list_var <- c("delta_Pasture","delta_Cropland","delta_Forest_net","Carbon_price","Year") 
#list_var <- c("delta_Pasture","delta_Cropland","delta_Other_all","Carbon_price","Year") # Considers all other land remaining as one pool 
list_par <- paste0("scale(",list_var,")") %>% str_c(collapse = " + ")

# Trimming outliers and cleaning up
if(diagnostic_plots=="yes"){ # if enables, plots all models/studies, histogram of indicator and relationship between chosen dependent variable and indicator
  source("Scripts/Exploratory_plot.R")
  exploratory_plots(delta_AFOLU,"Cropland",ind,i)
}

if(trim_outliers=="yes"){ # If yes, residuals are trimmed
  source("Scripts/Trim_residuals.R")
  stat_df <- trim_residuals(delta_AFOLU,ind,i) %>% data.frame()
}

if(diagnostic_plots=="yes"){ # if enables, plots all models/studies, histogram of indicator and relationship between chosen dependent variable and indicator
  source("Scripts/Exploratory_plot.R")
  exploratory_plots(stat_df,"Cropland",ind,i)
}

# Eliminating NA entries and outlier models
#base_year_AFOLU <- delta_AFOLU %>% filter(Year==2010) 
#boxplot(base_year_AFOLU$CO2_LUC~base_year_AFOLU$Model)

excluded_models <- c("POLES ADVANCE","POLES CD-LINKS","POLES EMF33") # The Poles models start from a very different base year values for all key parameters and are identified as significant outliers

stat_df <- stat_df %>% dplyr::select(any_of(c("Model","Scenario","Year",
                                              ind[[i]],list_var))) %>% 
  drop_na() %>% # Dropping NAs before fitting models
  filter(Model %notin% excluded_models)

# Establishing that mixed model is better than glm
m0.glm <- glm(stat_df[,ind[i]] ~ 1, family = gaussian, data = stat_df) # Simple linear model
m0.lmer <- lmer(stat_df[,ind[i]] ~ 1 + (1|Model), data = stat_df) # Linear mixed model with model/study as intercept
#m0.lmer <- lmer(stat_df[,ind[i]] ~ 1 + (1+|Model) + (1|Scenario), data = stat_df) # Linear mixed model with model/study as intercept
AIC(logLik(m0.glm))
AIC(logLik(m0.lmer))

## 3.2 Fitting models and looking at diagnostics to improve model

lm_global <- lm(paste0(ind[i]," ~ ",list_par),data=stat_df)#na.action = 'na.exclude') # Simple linear model for comparison
glmm_global <- lmer(paste0(ind[i]," ~ ",list_par,"+ (1|Model)"),data=stat_df,REML=TRUE)
glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",list_par,"+ (1|Model)"),data=stat_df,REML=TRUE)

r.squaredGLMM(glmm_global)

tab_model(glmm_global,glmm_global_robust)

# Inspecting residual plots
simulationOutput <- simulateResiduals(fittedModel = glmm_global)
plot(simulationOutput, quantreg = TRUE)
recalc_resid <- recalculateResiduals(simulationOutput, group = stat_df$Model)
plot(recalc_resid, quantreg = TRUE)
predictmeans::residplot(glmm_global, level=1) # Residuals plot pop-out

## 3.3 Trimming residuals and refitting model

outlier_cutoff <- 2.5# outlier cutoff - this is better than a priori trimming outliers which biases the result and keeps more sample
res <- resid(glmm_global, type = "pearson") # Extract standardized residuals
stat_df[abs(res)>outlier_cutoff,] # Visualise the residuals to be trimmed
new_data <- romr.fnc(glmm_global, stat_df, trim = outlier_cutoff) # Extreme residuals removed to enable better fit
stat_df <- new_data$data # New dataset
glmm_global <- lmer(paste0(ind[i]," ~ ",list_par[[i]][[j]],"+ (1|Model)"),data=stat_df,REML=TRUE)

r.squaredGLMM(glmm_global)

tab_model(glmm_global,glmm_global_robust)

## 3.4 Cross-validation of the lmer model - uses custom CV_GLMM function 
source("Scripts/CV_GLMM_LUC.R")
CV_lmer_results <- CV_GLMM(stat_df,CV_reps,ind) # This will return a table with summary results of repeat cross-validation
#CV_lmer_results <- CV # In cases with error run manually
CV_results <- data.frame(CV_lmer_results$RMSE,CV_lmer_results$AIC,CV_lmer_results$AICc,CV_lmer_results$r2m,CV_lmer_results$r2c)
write.csv(CV_results,paste0("GLMMs/CV/CV_results_",ind[i],currentDate,".csv"))


## Saving model diagnostics

# # Additional model diagnostics - Source: https://ourcodingclub.github.io/tutorials/mixed-models/
re.effects <- plot_model(glmm_global, type = "re",value.size = 3,dot.size = 1.5,line.size = 0.5, show.values = TRUE,value.offset = 0.3)
re.effects
ggsave(filename = paste0("GLMMs/Random_effects/",ind[i],".pdf"), width = 5, height = 4, device='pdf', dpi=700)

fix.effects <- plot_model(glmm_global, type = "est",value.size = 3,dot.size = 1.5,line.size = 0.5, show.values = TRUE,value.offset = 0.3)
fix.effects

ggsave(filename = paste0("GLMMs/Random_effects/Fixed_effects_",ind[i],".pdf"), width = 5, height = 4, device='pdf', dpi=700)

pred.effects <- plot_model(glmm_global, type = "pred",value.size = 3,dot.size = 1.5,line.size = 0.5, show.values = TRUE,value.offset = 0.3,grid = TRUE)
#gridExtra::grid.arrange(pred.effects)
multiplot(plotlist = pred.effects,cols=2)
cowplot::plot_grid(pred.effects$delta_Pasture,pred.effects$delta_Cropland,pred.effects$delta_Forest_net,pred.effects$Carbon_price,pred.effects$Year,ncol = 2)
ggsave(filename = paste0("GLMMs/Random_effects/Predictions_",ind[i],".pdf"), width = 6.5, height = 6.5, device='pdf', dpi=700)

# Model compared to robust version to show reduced influence of residuals

tab_model(glmm_global,glmm_global_robust,show.aic = TRUE,show.aicc = TRUE,
          file = paste0("GLMMs/All_models_",ind[i],"_",Sys.Date(),".html"))

drop1(glmm_global,test="Chisq")
step(glmm_global)
write.csv(drop1(glmm_global,test="Chisq"),paste0("GLMMs/Drop1/Drop1",ind[i],model_specs[j],effect_size,currentDate,".csv"))

VIF_glmm <- as.data.frame(vif(glmm_global))
VIF_glmm$`GVIF^(1/(2*Df))` <- VIF_glmm$`GVIF^(1/(2*Df))`^2 # Squaring last column as per: https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif
colnames(VIF_glmm) <- c("GVIF","Df","VIF")
VIF_glmm
write.csv(VIF_glmm,paste0("GLMMs/VIF/VIF",ind[i],model_specs[j],effect_size,currentDate,".csv")) 

# Writing global model characteristics and producing tables for SI
tab_model(glmm_global, show.df = TRUE,show.aic = TRUE)
qqnorm(residuals(glmm_global),main = paste0("Normal Q-Q plot - ",ind[i]))
plot(glmm_global,main=paste0(ind[i], " - residuals"))
sum(abs(residuals(glmm_global)))

## SAVING models for future predictions
saveRDS(glmm_global,paste0("GLMMs/Global_model_",ind[i],"_",currentDate,".rds")) # Saving final global model 
saveRDS(stat_df,paste0("GLMMs/Models/Dataset_",ind[i],currentDate,".rds")) # Saving dataset for prediction
write.csv(Anova(glmm_global),paste0("GLMMs/ANOVA/Anova",ind[i],currentDate,".csv")) # Significance for fixed effects predictors
save.lmer.effects(glmm_global,file=paste0("GLMMs/ANOVA/",ind[i],currentDate)) # Saving model specifications (includes r-squred for mixed models)

pdf(paste0("GLMMs/Residuals/",ind[i],"_","QQplotRES.pdf"))
qqnorm(residuals(glmm_global),main = paste0("GLMMs/Normal Q-Q plot - ",ind[i]))
dev.off()
# 
pdf(paste0("GLMMs/Residuals/",ind[i],"_","ScaledRES.pdf"))
plot(glmm_global,main=paste0("GLMMs/",ind[i], " - residuals"))
dev.off()
# 
################################################# 4.0 MAKING PREDICTIONS #####################################################

##  See script Final_risk_across_PBs for predictions
