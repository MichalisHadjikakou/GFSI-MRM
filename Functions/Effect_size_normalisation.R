##################################################
##### FUTURE FOOD SCENARIOS - Data cleaning #####
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Last updated: 29 March 2022
# Purpose: Effect size calculation 

ES_metric <- function(ind,stat_df,effect_size,sel_pred){
  
  if(effect_size=="absolute"){ # If relative change is considered as absolute deviation 
    
    stat_df <- stat_df %>%
      arrange(Study,ScenYear) %>% 
      group_by(Study) %>% #Arrange by BAU first etc
      mutate_at(c(ind[[i]]), funs(c(first(.), (. - first(.))[-1])) )%>% # Calculate difference from base year across columns
      mutate_at(c(sel_pred), funs(c(first(.), (. / first(.))[-1]-1)) )%>%
      slice(-1) %>%# Remove first row (base year) of every group
      data.frame()
    
  } else if (effect_size=="lnR") { # If relative change is considered as response ratio 
    
    stat_df <- stat_df %>%
      arrange(Study,ScenYear) %>% 
      group_by(Study) %>% #Arrange by BAU first etc
      mutate_at(c(ind[[i]]), funs(c(first(.), (log(. / first(.))[-1]))) )%>% # Calculate difference from base year across columns
      mutate_at(c(sel_pred), funs(c(first(.), (. / first(.))[-1]-1)) )%>%
      slice(-1) %>%# Remove first row (base year) of every group
      dplyr::mutate(across(c(sel_pred), ~replace_na(.x, -1))) %>%
      data.frame()
    
  } else { # If effect size is % change
    
    stat_df <- stat_df %>% 
      arrange(Study,ScenYear) %>% 
      group_by(Study) %>% #Arrange by BAU first etc
      mutate_at(c(ind[[i]],sel_pred), funs(c(first(.), (. / first(.))[-1]-1)) )%>% # Calculate difference from base year across columns
      slice(-1) %>%# Remove first row (base year) of every group
      #dplyr::mutate(across(Rum_kcal_FCR_FCF:Dairy_kcal_FCR_FCF, ~replace_na(.x, -1))) %>% # Change depending on model
      #dplyr::mutate(across(Rum_meat_crop_feed:Plant_food, ~replace_na(.x, -1))) %>% 
      dplyr::mutate(across(c(sel_pred), ~replace_na(.x, -1))) %>%
      #dplyr::mutate(across(Rum_meat_grass_feed:Dairy_grass_feed, ~replace_na(.x, -1))) %>%
      #dplyr::mutate(Aqua_kcal = ifelse(is.infinite(Aqua_kcal), 1, replace_na(Aqua_kcal, 1))) %>% 
      data.frame()
    
  }

  return(stat_df)
}