#################### Visualising random and fixed effects ##################################

## Last updated: 221104
## Author: Michalis Hadjikakou, Deakin University
## Purpose: Comparing random effects to mean intercept and fixed effects to original data and saving graphs
## Source: https://cran.microsoft.com/snapshot/2019-09-16/web/packages/cvms/vignettes/Introduction_to_cvms.html

# 1.0 Random effects structure

re.effects <- plot_model(glmm_global, type = "re",value.size = 3,dot.size = 1.5,line.size = 0.5, show.values = TRUE,value.offset = 0.4)+
  #coord_cartesian(xlim=c(-0.5, 0.5))+
  labs(x = "Model", y = paste(effect_size, "(difference from mean intercept)"))
re.effects
ggsave(filename = paste0("Outputs/Meta-regression/LMM_diagnostics/Random_effects/",ind[i],model_specs[j],".tiff"),units ="in",width = 5, height = 4, device='tiff', dpi=600)

# 2.0 Fixed effects

# First plot al`l the percentage fixed effects

  final_variables <- formula(glmm_global) %>% as.character # final selected variables in model
  
  sel_pred_all <- list_var[[i]][[j]]

  used_pred <- sel_pred_all[sapply(seq_along(sel_pred_all),function(x) grepl(sel_pred_all[x],final_variables[3]))] # Finding variables that are actually in the final fitted model

list_fix_slopes <- lapply(seq_along(used_pred),function(pred_ind) { # Loop across each indicator
  
  pred.mm <- ggpredict(glmm_global, terms = c(used_pred[pred_ind]))  # this gives overall predictions for the model
  
  (ggplot(pred.mm) + 
      geom_line(aes(x = x, y = predicted)) +          # slope
      geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), 
                  fill = "lightgrey", alpha = 0.5) +  # error band
      geom_point(data = stat_df,                      # adding the raw data (scaled values)
                 aes_string(x = used_pred[pred_ind], y = ind[i], colour = "Model")) + 
      scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
      coord_cartesian(ylim=c(-0.75, 0.75))+
      labs(x = paste(used_pred[pred_ind]), y = "lnR") + 
      viridis::scale_color_viridis(discrete = TRUE)+
      theme_bw() +
      theme(legend.position = "none")
  )
  
})

# Then plot initial delta in native units, if present in the model equation

if(grepl(base_year_norm,final_variables[3])){ # If base_year norm is present then use it as a predictor

  pred.initial <- ggpredict(glmm_global, terms = base_year_norm)  # this gives overall predictions for the model
  
  initial_plot <- (ggplot(pred.initial) + 
                     geom_line(aes(x = x, y = predicted)) +          # slope
                     geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), 
                                 fill = "lightgrey", alpha = 0.5) +  # error band
                     geom_point(data = stat_df,                      # adding the raw data (scaled values)
                                aes_string(x = base_year_norm, y = ind[i], colour = "Model")) + 
                     #scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
                     coord_cartesian(ylim=c(-0.75, 0.75))+
                     labs(x = paste0(base_year_norm," (",Units[i],")"), y = "lnR") + 
                     viridis::scale_color_viridis(discrete = TRUE)+
                     theme_bw() +
                     theme(legend.position = "none")
  )
  
  list_fix_slopes[[list_fix_slopes %>% length()+1]] <- initial_plot
}

# Bring everything together in one single list

cowplot::plot_grid(plotlist = list_fix_slopes,nrow=3,ncol=3)

ggsave(filename = paste0("Outputs/Meta-regression/LMM_diagnostics/Fixed_effects/Predictions_",ind[i],model_specs[j],".pdf"), width = 8, height = 8, device='pdf', dpi=300)


# Further visualising random and fixed effects

# plot(fitted(glmm_global), residuals(glmm_global), xlab = "Fitted Values", ylab = "Residuals")
# abline(h = 0, lty = 2)
# lines(smooth.spline(fitted(glmm_global), residuals(glmm_global)))
# #
# y <- stat_df[,ind[i]] # dependent variable (for simplicity)
# 
# ## box-plots of residuals by random effect levels
# plot(glmm_global, Model ~ resid(., scaled=TRUE))
# ## observed versus fitted values by Subject
# plot(glmm_global,Cropland ~ fitted(.) | Model, abline = c(0,1))
# ## residuals by age, separated by Subject
# plot(glmm_global, resid(., scaled=TRUE) ~ Yield | Study, abline = 0)
# plot(glmm_global, resid(., scaled=TRUE) ~ Dairy_grass_feed | Study, abline = 0)
# plot(glmm_global, resid(., scaled=TRUE) ~ Population | Study, abline = 0)

