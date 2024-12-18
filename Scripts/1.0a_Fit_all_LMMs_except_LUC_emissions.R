##################################################
## FUTURE FOOD SCENARIOS - Statistical analysis ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Statistical analysis using LMMs to assess impact of individual drivers and then estimate risks associated with possible prediction scenarios 

################################################# 1.0 INITIALISING #####################################################

## 1.1  Setting up modelling parameters and working directories
write_results <- 'yes'
currentDate <- Sys.Date()
imputation <- 'no' # Should missing values be imputed
base_year_norm <- "Delta_initial"# Base year normalisation method - can also change to "Initial_condition"
scale_predictors <- 'yes' # Whether predictors should be standardised
model_change <- 'yes' # Should the models predict change from base year or absolute number
effect_size <- 'lnR'# Whether the difference from the baseline/BAU should be a multiplier or absolute difference
residual_simulations <-'yes' # Should residuals be simulated
trim_outliers <- "no"
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

# 1.2  Loading necessary packages and functions

if (!require("pacman")) install.packages("pacman")
pacman::p_load(gridExtra,car,MASS,outliers,sjPlot,lme4,AICcmodavg,jtools,lmerTest,ggpubr,
               LMERConvenienceFunctions,MuMIn,fitdistrplus,merTools,eatGet,tidyverse,pbkrtest,future.apply,readxl,writexl,
               performance,hablar,reshape2,cowplot,DHARMa,predictmeans,remotes,robustlmm,cvms,ggeffects,
               groupdata2,caret,interactions,wppExplorer,wpp2019,DescTools,ciTools,here,see)

# Please note that eatGet may require manual installation and also requires 'reshape2' - see https://rdrr.io/rforge/eatGet/
#install.packages("eatGet", repos="http://R-Forge.R-project.org")

## 1.3 Loading necessary functions, datasets, and prediction parameters

`%notin%` <- Negate(`%in%`)
source(here("Functions/Remove_outliers.R")) # Function to remove outliers

## 1.4 Loading input files

data_raw = readxl::read_xlsx(here("Input_data/Data_S1_Input_database_20240624.xlsx"),sheet = "T1 - Main scenario database") # Reading in values as dataframe
PBs <- read.csv(here("Outputs/Environmental_limits/All_PB_limits.csv")) # Reading in PB distribution parameters

## 1.5 Recoding dataframe to simplify column names and ensure unit consistencies (necessary conversions are detailed in the following steps)

source(here("Functions/Pre_processing_disaggregated_feed.R")) # Carries out pre-processing of the raw data extracted
DF <- DF_preprocess(data_raw,PBs) %>% data.frame()
# Indicator and broad scenario names
ind <- c("Cropland","Pasture","Water","CH4","N2O", # Forest and LUC are derived post-hoc (on the basis of pasture and cropland)
         "Nfert","Pfert") # Using 'Nfert' instead of "Nfix' as per Springmann et al. (2018)

# Base year distributions - simulation
Base_year_data <- readxl::read_xlsx(here("Input_data/Base_year_values.xlsx"))
value_base_year <- Base_year_data$Value
Units <- c("Mha","Mha","km3/yr","GtCO2-eq","GtCO2-eq","GtCO2-eq","Tg N/yr","Tg P/yr")

  ################################################# 2.0 SETTING UP ANALYSIS #####################################################
  
## 2.1 Cross-validation and model parameterisation scripts/functions

# Creating empty list of data.frames for saving result
model_specs <- c("All","Hybrid","Process-based") # Alternative model specifications

val_metrics <- c("RMSE","NRMSE(RNG)","AIC","AICc","r2m","r2c")
CV_reps <- 5 # How many times to repeat cross-validation

CV_result_mat <- matrix(NA,1,length(val_metrics)) %>% 
  data.frame() %>% 
  set_names(val_metrics) %>% 
  add_column(Predictors=NA,.before=1) %>% 
  add_column(Indicator=NA,.before=1)

list_CV_result_mat <- replicate(length(model_specs),CV_result_mat,simplify = FALSE) # replicating for each model specification
list_CV <- replicate(length(ind),list_CV_result_mat,simplify=FALSE)  # replicating for each indicator

# Calling 'model_par' function that provides model specification for each indicator model
source(here("Functions/Model_parameterisation.R"))
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
key_preds <- c("Yield","Rum_feed_grass","WPinc","Rum_meat_kcal_GHG","GHG_eff_N2O","NUEinc","PUEinc")
key_preds_string <- c("Yield","Grass feed","Water use efficiency","Ruminant meat","N2O efficiency",
                      "Nitrogen use efficiency","Phoshorus use efficiency")
  
## 2.2 Setting up multi-core analysis

#plan(multiprocess, workers = length(list_par)) # Can change this accordingly (If 2010, use crop_num)

#i <- x
i <- 2# Indicator selection -for testing
j <- 3 # LMM model-type for testing (default=3)

# To loop through model fitting process 
# sapply(seq_along(ind), function(i) { # Loop across each indicator
#   lapply(1:3, function(j) { # Loop across each regression equation

# Effect size adjustment (the default is 'lnR')

if(i==2){
  effect_size<-"delta (%)" # This model fits better for pasture
} else {
  effect_size <- 'lnR'
}

## 2.1  Create a list with dataframes for each selected indicator (within each PB system) 
  
  df_stat <- DF[!is.na(DF[,ind[i]]),] %>% #%>% filter(Scenario.year..numeric.==2050)# filtering only rows that have indicator values 
    filter(Included=="Yes") %>%  # Some papers with very incomplete data are excluded completely
    mutate(Scope = dplyr::recode (Scope, # Recoding to create factor with two levels (Food & Agriculture)
                           `Food production`= "Food",
                           `Food system` = "Food",
                           `AFOLU` = "Agriculture")) %>%
    data.frame() # Converting to data.frame if tibble
  
 # Trimming outliers and cleaning up
  source(here("Functions/Study_selection.R"))
  df_stat <- select_studies(df_stat,i) 
  
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
    source(here("Functions/Effect_size_normalisation.R"))
    stat_df <- ES_metric(ind,stat_df,effect_size,sel_pred)
  }
  
  if(trim_outliers=="yes"){ # If yes, extreme outliers are trimmed - creating a more normal distribution
    source(here("Functions/Trim_outliers.R"))
    stat_df <- trim_residuals(stat_df,ind,i)
  }
  
   if(diagnostic_plots=="yes"){ # if enables, plots all models/studies, histogram of indicator and relationship between chosen dependent variable and indicator
     source(here("Functions/Exploratory_plot.R"))
     exploratory_plots(stat_df,key_preds,key_preds_string,ind,i)
   }
  
  # Visualising relationships between key predictors and response variable to decide on random effect structure 

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

  AIC(logLik(m0.glm))
  AIC(logLik(m0.lmer)) # A lower AIC for lmer model indicates that the use of an LMM is beneficial
  
  ## 3.3 Fitting both an lmer and rlmer (robustLMM) version of the model
  
  All_preds <- paste0(list_par[[i]][[j]],"+ scale(",base_year_norm,")")
  
  if(i==3){
    
    glmm_global <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1+WPinc|Model)"),data=stat_df,REML=TRUE)
    glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",All_preds,"+ (1+WPinc|Model)"),data=stat_df,REML=TRUE) # RobustLMM version (less sensitive to outliers, heteroscedasticity and heterogeneity of variance)
    r.squaredGLMM(glmm_global)
    
  } else if (i==4) {  

    glmm_global <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Rum_meat_kcal_GHG|Model)"),data=stat_df,REML=TRUE)
    glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Rum_meat_kcal_GHG|Model)"),data=stat_df,REML=TRUE)
    r.squaredGLMM(glmm_global)
    
  } else if (i==5) {  
    
    glmm_global <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Yield|Model)"),data=stat_df,REML=TRUE)
    glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",All_preds,"+ (1+Yield|Model)"),data=stat_df,REML=TRUE)
    r.squaredGLMM(glmm_global)
    
  } else {
    
    glmm_global <- lmer(paste0(ind[i]," ~ ",All_preds,"+ (1|Model)"),data=stat_df,REML=TRUE)
    glmm_global_robust <- rlmer(paste0(ind[i]," ~ ",All_preds,"+ (1|Model)"),data=stat_df,REML=TRUE) # RobustLMM version (less sensitive to outliers, heteroscedasticity and heterogeneity of variance)
    r.squaredGLMM(glmm_global)
  }
  
  tab_model(glmm_global,glmm_global_robust)
  performance::check_model(glmm_global)
  
  ## 3.4 Checking alternative fixed effect structures - stepwise elimination not used anymore since cross-validation was introduced
  
  # Dropping variables to see effect on AIC and LRT
  drop1(glmm_global,test="Chisq")
  
  # Checking for colinearity
  VIF_glmm <- as.data.frame(vif(glmm_global))
  VIF_glmm
  write.csv(VIF_glmm,paste0(here("Outputs/Meta-regression/LMM_diagnostics/VIFs/VIF"),ind[i],model_specs[j],effect_size,currentDate,".csv")) 
  
  ## 3.5 Exploring basic model plots and diagnostics and visualising residuals
 
  # vary.int.graph
   plot(glmm_global, Model ~ resid(.), abline = 0 ) # generate diagnostic plots
   
   plot(glmm_global, resid(., type = "pearson") ~ fitted(.) | Model, id = 0.05,
        adj = -0.3, pch = 20, col = "gray40")

  # DHARMA residual plots (for testing residual plots - simulation take significant time)
    
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
     write.csv(glmm_residuals,paste0(here("Outputs/Meta-regression/LMM_diagnostics/Residuals/Residuals_before_trim"),ind[i],model_specs[j],effect_size,currentDate,".csv"))
 
     # Trimming of residuals (if deemed necessary after looking at diagnostics but ensuring coefficient are same as in robust model)  
     
     if(i==7){
       
       stat_df <- stat_df
       
     }else{
       
       if (i==4|i==5|i==6){ # Carry this to minimise influence of extreme residuals based on rlmer results and overall sample size)
       
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
  
     } 
           
         # Refitting model without residuals
         glmm_global <- lmer(formula(glmm_global),data=stat_df,REML=TRUE)
       
     # Revisiting model diagnostics
     
     tab_model(glmm_global,glmm_global_robust)
     performance::check_model(glmm_global)
     # DHARMA residual plots - after trimming residuals
     
     simulationOutput <- simulateResiduals(fittedModel = glmm_global)
     plot(simulationOutput, quantreg = TRUE)
     recalc_resid <- recalculateResiduals(simulationOutput, group = stat_df$Model)
     plot(recalc_resid, quantreg = TRUE)
     png(file=paste0(here("Outputs/Meta-regression/LMM_diagnostics/Residual_plot_"),ind[i],".png"),
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
     write.csv(glmm_residuals,paste0(here("Outputs/Meta-regression/LMM_diagnostics/Residuals/Residuals_after_trim"),ind[i],model_specs[j],effect_size,currentDate,".csv"))
     
  ## 3.6  Cross-validation of the lmer model - uses custom CV_GLMM function  
     
     source(here("Functions/CV_GLMM.R"))
     CV_lmer_results <- CV_GLMM(stat_df,CV_reps,ind,i) # This will return a table with summary results of repeat cross-validation
     CV_results <- data.frame(CV_lmer_results$RMSE,CV_lmer_results$`NRMSE(RNG)`,CV_lmer_results$AIC,CV_lmer_results$AICc,CV_lmer_results$r2m,CV_lmer_results$r2c)
     write.csv(CV_results,paste0(here("Outputs/Meta-regression/Cross_validation/CV_results_"),ind[i],model_specs[j],effect_size,currentDate,".csv"))

  ## 3.7  Plotting random and fixed effects - and saving results
     
     source(here("Functions/Visualise_effects.R"))# source: Additional model diagnostics - Source: https://ourcodingclub.github.io/tutorials/mixed-models/

  ## 3.8  Saving models and model table for future predictions
     
  saveRDS(glmm_global,paste0(here("Outputs/Meta-regression/Fitted_models/Models//"),ind[i],model_specs[j],effect_size,"_",currentDate,".rds")) # Saving final global model 
  saveRDS(stat_df,paste0(here("Outputs/Meta-regression/Fitted_models/Datasets//"),ind[i],model_specs[j],currentDate,".rds")) # Saving dataset for prediction
  
  pdf(paste0(here("Outputs/Meta-regression/LMM_diagnostics/Residuals//"),ind[i],model_specs[j],"_",currentDate,"QQplotRES.pdf"))
  qqnorm(residuals(glmm_global),main = paste0(here("GLMMs/Normal Q-Q plot - "),ind[i],model_specs[j]))
  dev.off()
 
  pdf(paste0(here("Outputs/Meta-regression/LMM_diagnostics/Residuals//"),ind[i],model_specs[j],"_",currentDate,"ScaledRES.pdf"))
  plot(glmm_global,main=paste0(here("GLMMs//"),ind[i],model_specs[j], " - residuals"))
  dev.off()

  # Saving important model information for all three models
  # 
   tab_model(glmm_global,glmm_global_robust,show.aic = TRUE,show.aicc = TRUE,
             file = paste0(here("Outputs/Meta-regression/Model_summaries//"),ind[i],model_specs[j],effect_size,"_",currentDate,".html"))
  
  list_CV[[i]][[j]][,1] <- ind[i]
  list_CV[[i]][[j]][,2] <- model_specs[j]
  list_CV[[i]][[j]][,3] <- CV_lmer_results$RMSE
  list_CV[[i]][[j]][,4] <- CV_lmer_results$`NRMSE(RNG)`
  list_CV[[i]][[j]][,5] <- CV_lmer_results$AIC
  list_CV[[i]][[j]][,6] <- CV_lmer_results$AICc
  list_CV[[i]][[j]][,7] <- CV_lmer_results$r2m
  list_CV[[i]][[j]][,8] <- CV_lmer_results$r2c
  
 #}) # End of loop (if running as loop)

Results_list <- do.call(Map, c(f = rbind, list_CV))
Final_results <- do.call(rbind, Results_list)  

write.csv(Final_results,paste0(here("Outputs/Meta-regression/Cross_validation/Cross_validation_results"),"_",currentDate,".csv"))
 
########################################## END ###############################################################################
