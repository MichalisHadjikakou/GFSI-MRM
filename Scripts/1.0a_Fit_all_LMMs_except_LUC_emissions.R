##################################################
## FUTURE FOOD SCENARIOS - Statistical analysis ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Statistical analysis using GLMMs to assess impact of individual drivers and then estimate risks associated with possible prediction scenarios 


################################################# 1.0 INITIALISING #####################################################

## 1.1  Setting up modelling parameters and working directories
rm(list = ls())
PC <- 'denethor'
write_results <- 'yes'
currentDate <- Sys.Date()
imputation <- 'no' # Should missing values be imputed
base_year_norm <- "Delta_initial"# Base year normalisation method - can also change to "Initial_condition"
scale_predictors <- 'yes' # Whether predictors should be standardised
model_change <- 'yes' # Should the models predict change from base year or absolute number
effect_size <- 'lnR'# Whether the difference from the baseline/BAU should be a multiplier or absolute difference
residual_simulations <-'yes' # Should residuals be simulated
trim_outliers <- "yes"
trim_residuals <- 'no' # Should residuals be trimmed in LMM
remove_disruptive <- 'no' # Should disruptive scenarios be removed (e.g artificial meat)
existing_models <- 'no' # Use already fitted models?
diagnostic_plots <- 'yes' # Produced exploratory plots?

# Prediction parameters
Base_year <- 2010
Scenario_year <- 2050
Pred_levels <- 4 # 4 unique levels for each predictor

# Simulation parameters
Nsim = 1000 # Number of bootstraps
Nimp = 10 # Number of imputed datasets
n <- 10000 # Number of random draws for distributions/simulations
set.seed(123) # Ensures reproducibility of bootstrap results

if(PC=='work_laptop') {
  setwd("C:/Users/Hadj/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/Meta_analysis/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("M:/Food-Systems/Meta_analysis/")
} else if (PC =='denethor') {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSS-MM/")
} else {
  setwd("C:/Users/Michalis/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/Meta_analysis/GFSS-MM/")  
}

# 1.2  Loading necessary packages and functions

if (!require("pacman")) install.packages("pacman")
pacman::p_load(gridExtra,car,MASS,outliers,sjPlot,lme4,AICcmodavg,jtools,lmerTest,ggpubr,
               LMERConvenienceFunctions,MuMIn,fitdistrplus,merTools,eatGet,tidyverse,pbkrtest,future.apply,readxl,writexl,
               performance,hablar,reshape2,cowplot,DHARMa,predictmeans,remotes,robustlmm,cvms,ggeffects,
               groupdata2,caret,interactions,wppExplorer,wpp2019,DescTools,ciTools)

# Please note that eatGet may require manual installation and also requires 'reshape2' - see https://rdrr.io/rforge/eatGet/
#install.packages("eatGet", repos="http://R-Forge.R-project.org")

## 1.3 Loading necessary functions, datasets, and prediction parameters

`%notin%` <- Negate(`%in%`)
source("Functions/Remove_outliers.R") # Function to remove outliers

## 1.4 Loading input files

data_raw = readxl::read_xlsx("Input_data/Data_S1_Scenario_database_230201.xlsx",sheet = "T1 - Main scenario database") # Reading in values as dataframe
PBs <- read.csv("Outputs/Environmental_limits/All_PB_limits.csv") # Reading in PB distribution parameters

## 1.5 Recoding dataframe to simplify column names and ensure unit consistencies (necessary conversions are detailed in the following steps)
#source("Functions/Pre_processing.R") # Carries out pre-processing of the raw data extracted
#DF <- DF_preprocess(data_raw,PBs) %>% data.frame()# Uses custom-made function 'DF_preprocess' loaded previously

source("Functions/Pre_processing_disaggregated_feed.R") # Carries out pre-processing of the raw data extracted
DF <- DF_preprocess(data_raw,PBs) %>% data.frame()
# Indicator and broad scenario names
ind <- c("Cropland","Pasture","Water","CH4","N2O", # Forest and LUC are derived post-hoc (on the basis of pasture and cropland)
         "Nfert","Nsurplus","Pfert","Psurplus") # Using 'Nfert' instead of "Nfix' as per Springmann et al. (2018)

# Base year distributions - simulation

value_base_year <- c(1519, # Source: FAOSTAT Cropland in base year (2010)
                     3277, # Source: FAO - Permanent meadows and pastures in base year (2010)
                     1807, # Freshwater use Source: Springmann et al. 2018 - compatible with water boundary definition
                     #5623.622, #AR5: 5673.004 # 2010 Non-CO2 emissions FAO Tier 1 IPCC Agriculture, Source: Tubiello et al. 2021, FAOSTAT: https://www.fao.org/faostat/en/#data/GT
                     134.5250821,#AR6: 3659.082, #AR5: 3766.702 # 2010 CH4 emissions FAO Tier 1 IPCC Agriculture, Source: Tubiello et al. 2021, FAOSTAT: https://www.fao.org/faostat/en/#data/GT
                     7.1935945, #AR6: 1963.54, #AR5: 1906.303 # 2010 N2O emissions FAO Tier 1 IPCC Agriculture, Source: Tubiello et al. 2021, FAOSTAT: https://www.fao.org/faostat/en/#data/GT
                     103.68, # 101.05, # Nfert, Source: 2010 FAOSTAT, https://www.fao.org/faostat/en/#data/RFN
                     134.39, # Nsurplus Source: Willet et al. 2019, The Lancet
                     17.85, # 18.97, # Nfert, Source: 2010 FAOSTAT, https://www.fao.org/faostat/en/#data/RFN
                     11.1122899) # Source: Mogollon et al. (2018), 

                     #Forest <- 4102
                     #Minx_et_al_2021_CH4 <- 4.1097 # Source: https://zenodo.org/record/5566761#.YZDsZ2BBw3s, AR6
                     #Minx_et_al_2021_N2 <- 1.719  # Source: https://zenodo.org/record/5566761#.YZDsZ2BBw3s, AR6

Units <- c("Mha","Mha","km3/yr","GtCO2-eq","GtCO2-eq","GtCO2-eq","Tg N/yr","Tg N/yr","Tg P/yr","Tg P/yr")

scenario_types <- c("BAU","EFF","DIET","MIXED","OTHER") # Major scenario types

## OPTIONAL BASE YEAR (2005/10) DIAGNOSTICS
#source("Functions/Key_variable_distributions.R")
#Key_predictors_plot
#PB_indicator_plot

## 1.6  Specifying global model parameters for each indicator and saving in list
  
  # [22/04/21 - Added imputation option]
  # [25/02/21 - Added option to model differences from baseline as precentages or absolute numbers]

  ################################################# 2.0 SETTING UP ANALYSIS #####################################################
  
## 2.1 Cross-validation and model parameterisation scripts/functions

# Creating empty list of data.frames for saving result
model_specs <- c("All","Hybrid","Process-based") # Alternative model specifications

val_metrics <- c("RMSE","AIC","AICc","r2m","r2c")
CV_reps <- 5 # How many times to repeat cross-validation

CV_result_mat <- matrix(NA,1,length(val_metrics)) %>% 
  data.frame() %>% 
  set_names(val_metrics) %>% 
  add_column(Predictors=NA,.before=1) %>% 
  add_column(Indicator=NA,.before=1)

list_CV_result_mat <- replicate(length(model_specs),CV_result_mat,simplify = FALSE) # replicating for each model specification
list_CV <- replicate(length(ind),list_CV_result_mat,simplify=FALSE)  # replicating for each indicator

# Calling 'model_par' function that provides model specification for each indicator model
source("Functions/Model_parameterisation.R")
list_var <- model_par(ind) 

# Creating a list of parameters for each lmer model - string varies depending on whether predictors are standardised or not

if(scale_predictors=="yes"){ # If user decides to model change instead of absolute numbers (default is to model change as a multiplier)
  
  list_par <- lapply(seq_along(list_var),function(x) 
    lapply(1:3,function(y) paste0("scale(",list_var[[x]][[y]],")") %>% str_c(collapse = " + ")))

} else { # If effect size is % change
  
  list_par <- lapply(seq_along(list_var),function(x) 
    lapply(1:3,function(y) paste0(list_var[[x]][[y]]) %>% str_c(collapse = " + ")))
  
  }

#Key predictors vector - Key predictors for plotting against each indicator (can be modified)
key_preds <- c("Yield","Rum_meat_grass_feed","WPinc","Rum_meat_kcal_GHG","Yield","NUEinc","NUEinc","PUEinc","PUEinc")
key_preds_string <- c("Crop yields","Ruminant grass feed","Water use efficiency","Ruminant meat","Crop yields",
                      "Nitrogen use efficiency","Nitrogen use efficiency",
                      "Phosphorus use efficiency","Phoshorus use efficiency")
  
## 2.2 Setting up multi-core analysis

#plan(multiprocess, workers = length(list_par)) # Can change this accordingly (If 2010, use crop_num)
# 
# test_list <- lapply(1:4, function(i) { # Loop across each indicator
#   lapply(1:3, function(j) { # Loop across each regression equation


  i <- 4# Indicator selection -for testing
  j <- 3 # LMM model-type for testing (default=3)
  
  if(i==2){
    effect_size="delta (%)"
  }
  
  ## 2.1  Create a list with dataframes for each selected indicator (within each PB system) 
  
  df_stat <- DF[!is.na(DF[,ind[i]]),] %>% #%>% filter(Scenario.year..numeric.==2050)# filtering only rows that have indicator values 
    #filter(ScenYear<2051) %>% # Only consider data up to 2050 (reduces sample size but tightens the study scope)
    filter(Included=="Yes") %>%  # Some papers with very incomplete data are excluded completely
    #mutate(Aqua_kcal = replace_na(Aqua_kcal, 0)) %>% 
    mutate(Scope = dplyr::recode (Scope, # Recoding to create factor with two levels (Food & Agriculture)
                           `Food production`= "Food",
                           `Food system` = "Food",
                           `AFOLU` = "Agriculture")) %>%
    data.frame() # Converting to data.frame if tibble
  
 # Trimming outliers and cleaning up
  source("Functions/Study_selection.R")
  df_stat <- select_studies(df_stat)
  
  ## 2.2  Reformatting dataframe depending on mode of analysis (absolute numbers, absolute differences, log response ratio, relative differences)
  
  stat_df <- df_stat %>% # Cleaning dataframe
    dplyr::select(any_of(c("Study","Model","ScenYear","Scope","Cropland.scope","Pasture.scope","Water.scope","Scen_desc",
                           "Scenario..author.code.","C_price_cat",ind[[i]],list_var[[i]][[j]]))) %>%  # Selecting only relevant columns
    mutate(Study=as.factor(Study)) %>% # Ensuring that random intercept ID is a factor
    mutate(Model=as.factor(Model)) %>%  # Ensuring that random intercept ID is a factor
    mutate(Scope=as.factor(Scope)) %>% # Scope also adjusted to ensure it's a factor
    group_by(Study) %>% 
    mutate(Initial_condition = first(!!sym(ind[[i]]))) %>% # Adding initial condition (base year value)
    mutate(Delta_initial = first(!!sym(ind[[i]]))-value_base_year[i]) %>% # Adding base year delta
    ungroup()
  
  pred_index <- list_var[[i]][[j]] %notin% c("Organic","C_price","WPinc","GHG_eff_CH4","GHG_eff_N2O","GHG_eff_all",
                                             "Cropland.scope","Pasture.scope","NUEinc","NrecHousehold",
                                             "PUEinc","PrecHousehold") # Removing indicators where relative change is meaningless (start at zero)
  sel_pred <- list_var[[i]][[j]][pred_index] # Selecting model predictors
  
  sel_pred_plus <- c(sel_pred,base_year_norm)
    
  if(model_change=="yes"){ # If user decides to model change instead of absolute numbers (default is to model change as a multiplier)
    source("Functions/Effect_size_normalisation.R")
    stat_df <- ES_metric(ind,stat_df,effect_size,sel_pred)
  }
  
  if(trim_outliers=="yes"){ # If yes, extreme outliers are trimmed - creating a more normal distribution
    source("Functions/Trim_outliers.R")
    stat_df <- trim_residuals(stat_df,ind,i)
  }
  
  #stat_df <- stat_df %>% mutate(across(c(ind[i]),~Winsorize(., probs = c(0.01, 0.99))))
  
   if(diagnostic_plots=="yes"){ # if enables, plots all models/studies, histogram of indicator and relationship between chosen dependent variable and indicator
     source("Functions/Exploratory_plot.R")
     exploratory_plots(stat_df,key_preds,key_preds_string,ind,i)
   }
  
  # Visualising relationships between key predictors and response variable to decide on random effect structure 
  
  # ggplot(stat_df, aes_string(x = 'Yield', y = ind[i])) + geom_point() + facet_wrap(~ Model) +
  #   geom_smooth(method=lm)+
  #   ylab("Water lnR")
  
  #stat_df <- stat_df %>% filter(Dairy_grass_feed<1)
  
  ################################################# 3.0 FITTING THE MODELS #####################################################
  
  ## 3.1 Keeping only necessary columns and dropping NAs
  
  ## GLOBAL MODELS - Model fitted with lmer (Regulation not significant)
  if (i==1){
    
    stat_df <- stat_df %>% dplyr::select(any_of(c("Study","ScenYear","Model","Scope","Scenario..author.code.","Cropland.scope","Initial_condition","Delta_initial","C_price_cat",
                                                  ind[[i]],list_var[[i]][[j]]))) %>% drop_na() # Dropping NAs before fitting models
    
  } else if (i==2){

  stat_df <- stat_df %>% dplyr::select(any_of(c("Study","ScenYear","Model","Scope","Scenario..author.code.","Pasture.scope","Initial_condition","Delta_initial","C_price_cat",
                                                ind[[i]],list_var[[i]][[j]]))) %>% drop_na() # Dropping NAs before fitting models

  } else if (i==3) {

  stat_df <- stat_df %>% dplyr::select(any_of(c("Study","ScenYear","Model","Scope","Scenario..author.code.","Water.scope","Initial_condition","Delta_initial",
                                                  ind[[i]],list_var[[i]][[j]]))) %>% drop_na()

  } else {

  stat_df <- stat_df %>% dplyr::select(any_of(c("Study","ScenYear","Model","Scope","Scenario..author.code.","Initial_condition","Delta_initial",
                                                  ind[[i]],list_var[[i]][[j]]))) %>% drop_na()
   }
  
  ## 3.2 Testing to verify that random intercept improves the fit
  
  
  m0.glm <- glm(stat_df[,ind[i]] ~ 1, family = gaussian, data = stat_df) # Simple linear model
  m0.lmer <- lmer(stat_df[,ind[i]] ~ 1 + (1|Model), data = stat_df) # Linear mixed model with model/study as intercept
  # m1.lmer <- lmer(stat_df[,ind[i]] ~ 1 + (1+Yield+WPinc|Model), data = stat_df) # Linear mixed model with model and study as intercepts
  # m2.lmer <- lmer(stat_df[,ind[i]] ~ 1 + (WPinc|Model) + (Yield|Model), data = stat_df)
  # m3.lmer <- lmer(stat_df[,ind[i]] ~ 1 + (WPinc||Model) + (Yield||Model), data = stat_df)
  # 
  AIC(logLik(m0.glm))
  AIC(logLik(m0.lmer))
  # AIC(logLik(m1.lmer))
  # AIC(logLik(m2.lmer))
  # AIC(logLik(m3.lmer))
  
  anova(m0.glm,m0.lmer)
  ## 3.3 Fitting both an lmer and rlmer (robustLMM) version of the model
  
  All_preds <- paste0(list_par[[i]][[j]],"+ scale(",base_year_norm,")")#,"+ C_price_cat") # Adding C_price cat (where relevant)
  
  #All_preds <- paste0(list_par[[i]][[j]])
  
  if(i==3|i==5){
    
    glmm_global <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Yield|Model)"),data=stat_df,REML=TRUE)
    glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Yield|Model)"),data=stat_df,REML=TRUE) # RobustLMM version (less sensitive to outliers, heteroscedasticity and heterogeneity of variance)
    
    r.squaredGLMM(glmm_global)
    
  #} else if(i==3) {
    
  #  glmm_global <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (Yield||Model) + (Plant_food_water||Model)"),data=stat_df,REML=TRUE)
  #  glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",All_preds,"+ (Yield||Model) + (WPinc||Model)"),data=stat_df,REML=TRUE) # RobustLMM version (less sensitive to outliers, heteroscedasticity and heterogeneity of variance)
    # glmm_global1 <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1|Model) + (Yield+0|Model) + (WPinc+0|Model)"),data=stat_df,REML=TRUE)
    # glmm_global2 <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Yield+WPinc|Model)"),data=stat_df,REML=TRUE)
    # glmm_global3 <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (Yield|Model) + (WPinc|Model)"),data=stat_df,REML=TRUE)
    # glmm_global4 <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Yield|Model)"),data=stat_df,REML=TRUE)
    # 
    # a <- brm(paste0(ind[i]," ~ ",All_preds,"+ (Yield + WPinc|Model)"),data=stat_df)
    
    
  } else {
    
    glmm_global <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1|Model)"),data=stat_df,REML=TRUE)
    glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",All_preds,"+ (1|Model)"),data=stat_df,REML=TRUE) # RobustLMM version (less sensitive to outliers, heteroscedasticity and heterogeneity of variance)
    r.squaredGLMM(glmm_global)
  }
  
  #glmm_global1 <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Yield||Model)"),data=stat_df,REML=TRUE) uncorrelated intercept and slope
  check_model(glmm_global)
  tab_model(glmm_global,glmm_global_robust)
  
  ## 3.4 Checking alternative fixed effect structures through stepwise elimination
  # Dropping variables to see effect on AIC and LRT
  # 
  drop1(glmm_global,test="Chisq")
  
  # Making exception for Cropland since the robust model shows that delta initial is significant
  # Pasture also shows monogastric feed having an influence because one model assumes free range eggs/chicken but reduces performance
  
  if(i==1|i==3|i==5){
    
    alpha_fixed=0.3
  
  }else{
  
    alpha_fixed=0.2
  }

  glmm_global <- step(glmm_global,reduce.random = FALSE,alpha.fixed = alpha_fixed) %>% get_model() # Choosing model after inspecting drop1 results
  write.csv(drop1(glmm_global,test="Chisq"),paste0("Outputs/Meta-regression/LMM_diagnostics/Drop1/Drop1",ind[i],model_specs[j],effect_size,currentDate,".csv"))
  
  # Checking for colinearity
  VIF_glmm <- as.data.frame(vif(glmm_global))
  #VIF_glmm$`GVIF^(1/(2*Df))` <- VIF_glmm$`GVIF^(1/(2*Df))`^2 # Squaring last column as per: https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif
  #colnames(VIF_glmm) <- c("Variable","VIF")
  VIF_glmm
  write.csv(VIF_glmm,paste0("Outputs/Meta-regression/LMM_diagnostics/VIFs/VIF",ind[i],model_specs[j],effect_size,currentDate,".csv")) 
  
  # Testing alternative formulations (if necessary)
  #glmm_global <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Dairy_grass_feed+Rum_meat_grass_feed|Model)"),data=stat_df,REML=TRUE)
  # 
  # glmm_global <- lmer(paste0(ind[i]," ~ ",list_par[[i]][[j]],"+ scale(Delta_initial) + (1|Model)"),data=stat_df,REML=TRUE)
  # 
  # glmm_global2 <- lmer(paste0(ind[i]," ~ ",list_par[[i]][[j]],"+ scale(Initial_condition) + (1|Model)"),data=stat_df,REML=TRUE)
  

  ## 3.5 Exploring basic model plots and diagnostics and visualising residuals
 
  # vary.int.graph
   plot(glmm_global, Model ~ resid(.), abline = 0 ) # generate diagnostic plots
   
   plot(glmm_global, resid(., type = "pearson") ~ fitted(.) | Model, id = 0.05,
        adj = -0.3, pch = 20, col = "gray40")
   
   plot(glmm_global_robust, Model ~ resid(.), abline = 0 ) # generate diagnostic plots
   
   plot(glmm_global_robust, resid(., type = "pearson") ~ fitted(.) | Model, id = 0.05,
        adj = -0.3, pch = 20, col = "gray40")

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
 
     
     # Trimming of residuals (if deemed necessary after looking at diagnostics)  
     if (i %notin% c(9)){
     
     
       if (i==1|i==2){ # Carry this to minimise influence of extreme residuals based on rlmer results and overall sample size)
       
        threshold_trim <- 2.5
       
       }else{
       
        threshold_trim <- 3
        
       }
          # stat_df <- stat_df %>% mutate(across(c(sel_pred),~Winsorize(., probs = c(0.01, 0.99))))
           residual_cutoff <- threshold_trim# extreme residual cutoff 
           res <- resid(glmm_global, type = "pearson") # Extract standardized residuals
           stat_df[abs(res)>residual_cutoff,] # Visualise the residuals to be trimmed
           new_data <- romr.fnc(glmm_global, stat_df, trim = residual_cutoff) # Extreme residuals removed to enable better fit
           stat_df <- new_data$data #%>% filter(!grepl('Intensive dairy and aquaculture',Scenario..short.description.)) #%>% filter(!grepl('Low_kcal|high meat|high milk',Scenario..short.description.))# Extreme residuals removed to enable better fit
  
         # Refitting model without residuals
         glmm_global <- lmer(formula(glmm_global),data=stat_df,REML=TRUE)
         #glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",list_par[[i]][[j]],"+ (1|Model)"),data=stat_df,REML=TRUE) 
     
     }   
       
     # Revisiting model diagnostics
     
     tab_model(glmm_global,glmm_global_robust)
     check_model(glmm_global)
     # DHARMA residual plots - after trimming residuals
     
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
     
  ## 3.6  Cross-validation of the lmer model - uses custom CV_GLMM function  
     
     source("Functions/CV_GLMM.R")
     CV_lmer_results <- CV_GLMM(stat_df,CV_reps,ind,i) # This will return a table with summary results of repeat cross-validation
     #CV_lmer_results <- CV # In cases with error run manually
     CV_results <- data.frame(CV_lmer_results$RMSE,CV_lmer_results$`NRMSE(RNG)`,CV_lmer_results$AIC,CV_lmer_results$AICc,CV_lmer_results$r2m,CV_lmer_results$r2c)
     write.csv(CV_results,paste0("Outputs/Meta-regression/Cross_validation/CV_results_",ind[i],model_specs[j],effect_size,currentDate,".csv"))
 
  ## 3.7  Testing nlme model which uses weights to compensate for heterogeneity of variance and also brms (bayesian linear mixed model)
     #source("Scripts/nlme+brms.R")

  ## 3.8  Plotting random and fixed effects - and saving results
     
     source("Functions/Visualise_effects.R")# source: Additional model diagnostics - Source: https://ourcodingclub.github.io/tutorials/mixed-models/

  ## 3.9  Saving models and model table for future predictions
     
  saveRDS(glmm_global,paste0("Outputs/Meta-regression/Fitted_models/Models/",ind[i],model_specs[j],effect_size,"_",currentDate,".rds")) # Saving final global model 
  saveRDS(stat_df,paste0("Outputs/Meta-regression/Fitted_models/Datasets/",ind[i],model_specs[j],currentDate,".rds")) # Saving dataset for prediction
  #write.csv(Anova(glmm_global),paste0("GLMMs/ANOVA/Anova",ind[i],model_specs[j],effect_size,currentDate,".csv")) # Significance for fixed effects predictors
  #save.lmer.effects(glmm_global,file=paste0("GLMMs/ANOVA/",ind[i],model_specs[j],currentDate)) # Saving model specifications (includes r-squred for mixed models)

  pdf(paste0("Outputs/Meta-regression/LMM_diagnostics/Residuals/",ind[i],model_specs[j],effect_size,"_","QQplotRES.pdf"))
  qqnorm(residuals(glmm_global),main = paste0("GLMMs/Normal Q-Q plot - ",ind[i],model_specs[j]))
  dev.off()
  # 
  pdf(paste0("Outputs/Meta-regression/LMM_diagnostics/Residuals/",ind[i],model_specs[j],effect_size,"_","ScaledRES.pdf"))
  plot(glmm_global,main=paste0("GLMMs/",ind[i],model_specs[j], " - residuals"))
  dev.off()
  # 

  ## Saving model details and setting up multi-model prediction
  
  # Saving important model information for all three models
  # 
   tab_model(glmm_global,glmm_global_robust,show.aic = TRUE,show.aicc = TRUE,
             file = paste0("Outputs/Meta-regression/Model_summaries/",ind[i],model_specs[j],effect_size,"_",currentDate,".html"))
  
  list_CV[[i]][[j]][,1] <- ind[i]
  list_CV[[i]][[j]][,2] <- model_specs[j]
  list_CV[[i]][[j]][,3]=CV_lmer_results$RMSE
  list_CV[[i]][[j]][,4]=CV_lmer_results$AIC
  list_CV[[i]][[j]][,5]=CV_lmer_results$AICc
  list_CV[[i]][[j]][,6]=CV_lmer_results$r2m
  list_CV[[i]][[j]][,7]=CV_lmer_results$r2c

Results_list <- do.call(Map, c(f = rbind, list_CV))
Final_results <- do.call(rbind, Results_list)  

write.csv(Final_results,paste0("Outputs/Meta-regression/Cross_validation/Cross_validation_results","_",currentDate,".csv"))
 
########################################## END ###############################################################################

# Multi-model ensemble - disabled in the latest revision
 
  # if(multi_model_ensemble=='yes') {
  #   
  #   options(na.action=na.fail) # Needed condition to run dredge function
  #   top_models <- dredge(glmm_global,rank = "AICc", REML = FALSE) # Fits every possible sub-model within global model
  #   write.csv(MuMIn::importance(top_models),paste0("GLMMs/Importance_",ind[i],currentDate,".csv")) # Parameter importance diagnostics across all models - used as variable significance metric
  #   ensemble <- subset(top_models, delta <= 6, recalc.weights = FALSE)
  #   write.csv(ensemble,paste0("GLMMs/Selected_models_",ind[i],currentDate,".csv")) # Writing model parameters
  #   saveRDS(ensemble,paste0("GLMMs/Selected_models_",ind[i],currentDate,".rds")) # Saving models for prediction
  # }
  # # }else {
  # #   ensemble <- subset(top_models, cumsum(weight) <= .95, recalc.weights = FALSE) # Create ensemble with models accounting for 95% of Akaike weights
  # # 
  # # }
  # 
################################################# 4.0 MAKING PREDICTIONS (see Predict_scenario script) #####################################################
  
  # ##  4.1 Loading prediction dataset and customising for each indicator
  # 
  # source("Scripts/Prediction_dataset_relative_211204.R") # Loading prediction dataset
  # Pred_data <- Prediction_dataset(Base_year,Scenario_year,Pred_levels)
  # 
  # source("Scripts/Prediction_parameters.R")# Loading bespoke prediction dataset
  # Ind_preds <- Pred_data_indicator(Pred_data,i) %>% 
  #   mutate(Model = "~0")# # Adding column names and specifying random intercept =~0# Loading indicator-specific prediction dataset
  # 
  # # To predict using existing model
  # sim_date <- "2020-04-07"
  # 
  # if(existing_models=='yes') {
  # 
  #   glmm_global <- readRDS(paste0("GLMMs/Global_model_",ind[i],"_",sim_date,".rds")) # Loading global model
  #   ensemble <- readRDS(paste0("GLMMs/Selected_models_",ind[i],sim_date,".rds")) # Loading full prediction dataset
  #   stat_df <- readRDS(paste0("GLMMs/Dataset_",ind[i],sim_date,".rds")) # Loading final dataframe
  # }
  # # 
  # if(multi_model_ensemble=='yes') {
  # 
  # #top_models <- dredge(glmm_global,rank = "AICc", REML = FALSE) # Fits every possible sub-model within global model
  # #ensemble <- subset(top_models, delta <= 6, recalc.weights = FALSE)
  # model.preds <- sapply(get.models(ensemble, subset= TRUE), predict, newdata = Pred_dataset,re.form=~0) # Predicting from mean intercept 
  # Avg_prediction<-model.preds %*% Weights(ensemble) # Zero method
  # summary(exp(Avg_prediction))
  # summary(Avg_prediction)
  # 
  # }
  # 
  # Pred_int <- suppressWarnings(predictInterval(merMod = glmm_global,
  #                                        newdata = Ind_preds,
  #                                        n.sims = Nsim,
  #                                        which='full',
  #                                        level=0.95,
  #                                        include.resid.var = T,
  #                                        stat="median",returnSims = TRUE)) %>% 
  #   mutate(SD_lwr = upr-fit,
  #          SD_upr = fit-lwr,
  #          SD = mean(SD_lwr,SD_upr)/2) # Assumption that distribution is gaussian/normal (this can be confirmed through visual observation)
  # 
  # if(effect_size=='LnR') {  # Requires correction to express change as a percentage
  # 
  #   Pred_int <- Pred_int %>% 
  #   mutate_all(~exp(.) - 1) # Converting log response ratio to % change
  # }
  #   
  # Final_preds <- cbind(Ind_preds,Pred_int) %>%  # Selecting only relevant columns for summarising/presenting prediction results
  #   dplyr::select(any_of(c("Pop_levels","Diet","Waste","Yield_levels","Feed_efficiency","Feed_composition","Rum_meat_kcal_levels",
  #                          "Dairy_kcal_levels","Non_rum_kcal_levels","Vegetal_kcal_levels",
  #                          #"FCR_rum_meat","FCF_rum_meat","FCR_dairy","FCF_dairy","FCR_monogastrics","FCF_monogastrics",
  #                          "GHG_eff_all","GHG_eff_CH4","GHG_eff_N2O","C_price",
  #                          "WUEinc","Organic",
  #                          "N_management","NUEinc","NrecHousehold",
  #                          "P_management","PUEinc","PrecHousehold",
  #                          "fit","upr","lwr"))) %>% 
  #   mutate(Pred_avg = value_base_year[i] + fit*value_base_year[i], # Converting % predictions to physical units
  #          Pred_upr = value_base_year[i] + upr*value_base_year[i],
  #          Pred_lwr = value_base_year[i] + lwr*value_base_year[i]) 
  # 
  # ggplot(aes(x=1:100, y=fit, ymin=lwr, ymax=upr), data=Pred_int[1:100,]) +
  #   geom_point() +
  #   geom_linerange() +
  #   labs(x="Index", y="Prediction w/ 95% PI") + theme_bw()
  # 
  # ## Additional statistics and uncertainty estimates based on the global or best model
  # if(i==2) {  # Requires correction to express change as a percentage
  #   
  #   Results <- Final_preds
  #    # Converting log response ratio to % change
  # } else {
  # 
  # Results <- Final_preds %>% 
  #   add_column(Indicator=ind[i],.before=1) %>% # Removing unrealistic cases
  #   merge(PBs,by="Indicator")%>% # Adding the PB specifications to allow probability estimates
  #   relocate(System,.before=1) %>% 
  #   mutate(Risk = ptriang(Pred_avg, # Running through each row of the dataframe and selecting the appropriate indicator/boundary
  #                         a = Min,
  #                         b = Max,
  #                         c = Mode,
  #                         lower.tail=TRUE))%>%
  #   mutate(RiskCol = ifelse(Pred_avg < Min,"GREEN", # Adding a risk colour for each Pred_avg
  #                           ifelse(Pred_avg >= Min & Pred_avg <= Mode,"YELLOW",
  #                                  ifelse(Pred_avg >= Mode & Pred_avg < Max,"ORANGE",
  #                                         ifelse(Pred_avg >= Max,"RED",NA))))) %>%
  #   mutate(Safe = ifelse(Risk > 0.2,"NO","YES")) # Assuming 80% probability of meeting the boundary is acceptable 
  # }
  # 
  # Results$Risk_Joint <- future_sapply(1:dim(Results)[1],function(x) 
  #   sum((rnorm(n,Results$Avg_prediction[x],data_trimmed$SD_prediction[x]))>
  #         (rtriang(n,Results$Min[x],Results$Max[x],Results$Mode[x])))/n)
  # 
  # 
  # if(existing_models=='yes'){
  #   
  #   write.csv(Results,paste0("GLMMs/",ind[i],"_predictions_",fam.link.mer(glmm_global)[[1]],fam.link.mer(glmm_global)[[2]],sim_date,".csv")) # Writing indicator predictions
  #   
  # } else {
  #   
  #   write.csv(Results,paste0("GLMMs/",ind[i],"_",model_specs[j],"_predictions_",currentDate,".csv")) # Writing indicator predictions
  #   
  # }
  
  # Full boostrap including both fixed and random effects - see: https://cran.rstudio.com/web/packages/merTools/vignettes/Using_predictInterval.html
  # https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r
  
  # mySumm <- function(.) {
  #   predict(., newdata=Ind_preds, re.form=~0)
  # }
  # 
  # sumBoot <- function(merBoot) {
  #   return(
  #     data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
  #                lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
  #                upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  #     )
  #   )
  # }
  # 
  # ##lme4::bootMer() method 1
  # PI.boot1.time <- system.time(
  #   boot1 <- lme4::bootMer(glmm_global, mySumm, nsim=250, use.u=FALSE, type="parametric")
  # )
  # 
  # PI.boot1 <- sumBoot(boot1)