#################### LMER Model cross-validations ##################################

## Last updated: 211104
## Author: Michalis Hadjikakou, Deakin University
## Purpose: Performing repeated k-fold cross-validation for different numbers of K and repetitions
## Source: https://cran.microsoft.com/snapshot/2019-09-16/web/packages/cvms/vignettes/Introduction_to_cvms.html

CV_GLMM <- function(stat_df,CV_reps,ind,i){

  max_k <- stat_df$Model %>% unique %>% length() -1 # Number of random intercepts minus 1 to allow sufficient reps
  
  if(ind[i]=="Water"|ind[i]=="N2O"|ind[i]=="Pfert"){ # This prevents error that occurs when there is also a random slope in the model 
    min_k <- max_k
  }else{
    min_k <- 3
  }
  
  data <- stat_df #%>% # Stripping the dataframe to its basics to reduce computation
    #dplyr::select(any_of(c("Model",ind[[i]],sel_pred_plus,"C_price","C_price_cat")))
  
  #If only one cross-validation 
  # data <- fold(data, k = k,
  #              id_col = 'Model') %>%
  #   arrange(.folds)
  # 
  # CV <- cross_validate(data, paste0(ind[i]," ~ ",list_par[[i]][[j]],"+ (1|Model)"),
  #                       metrics = "all",
  #                       fold_cols = '.folds',
  #                       family = 'gaussian',
  #                       REML = TRUE,parallel = FALSE)
  
  #Repeat cross-validation (repCV)
  
  library(doParallel)
  doParallel::registerDoParallel(max_k)
  
  fold_counts <- round(seq(from = min_k, to = max_k)) %>% rep(each=CV_reps)
  
  data <- groupdata2::fold(data, k = fold_counts,
               id_col = "Model",
               num_fold_cols = length(fold_counts),
               parallel = TRUE)
  
  fold_columns <- paste0(".folds_", seq_along(fold_counts))
  
  # formulae <- combine_predictors(dependent = ind[i], # Allows testing all predictor combinations
  #                               fixed_effects = list_var[[i]][[j]],
  #                               random_effects = "(1|Model)")
  
  form_char <- formula(glmm_global) %>% as.character() # Extracting the glmm formula as character to be used in cross_validate function
  
  CV <- cross_validate(data,paste0(ind[i]," ~ ",form_char[3]),
                        metrics = "all",
                        fold_cols = fold_columns,
                        family = 'gaussian',
                        REML = TRUE,parallel = TRUE)

  return(CV)
}