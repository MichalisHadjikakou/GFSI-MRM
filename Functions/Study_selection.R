##################################################
##### FUTURE FOOD SCENARIOS - Data cleaning #####
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Last updated: 25 July 2019
# Purpose: Data cleaning and pre-processing

select_studies <- function(df_stat,i){

  Years <- c(1995:2012,seq(2015,2070,5)) # Ensures all base years are covered
  
  df_stat <- df_stat %>% # Filtering in-between years
    dplyr::filter(ScenYear %in% Years) 
  
  if (i==1){ # 
    
    excluded_studies <- c("Valin et al. (2013)","Bodirsky et al. (2012)","Bodirsky et al. (2014)",
                          "Pfister et al. (2011)", "Powell & Lenton (2012)",
                          "Springer & Duchin (2014)","Odegard & van der Voet (2014)",
                          "de Fraiture & Wichelns (2010)","Roos et al. (2017) No pasture")
                          
    df_stat <- df_stat %>% 
      filter(Study %notin% excluded_studies) %>% 
      arrange(Study,ScenYear) %>% 
      group_by(Study) 
      # mutate(C_price = case_when(Study=="Springmann et al. (2018)"~ 0, # Carbon price does not have a feedback on land use in this paper
      #                            TRUE ~ C_price)) %>% 
      # mutate(C_price_cat = case_when(C_price>0 ~ "Yes", # Creating a categorical variable for the carbon price since many studies don't apply it at all
      #                                TRUE ~ "No")) #%>% 
      #mutate(across(c(Cropland),
      #              ~ case_when(Cropland.scope=="Food crops" & ScenYear %in% c(2009,2010) ~ .+1547-dplyr::first(.),
      #                            TRUE ~ .)))# Tested normalisation for food only studies (Clark et al.2020 and EAT-Lancet) to match 2010 FAOSTAT cropland (1547 Mha)
  }
  
  if (i==2){ # 
    
    excluded_studies <- c("Roos et al. (2017) No pasture","Muller et al. (2017)",#"Theurl et al. (2020)",
                          "Pfister et al. (2011)",
                          "de Fraiture & Wichelns (2010)") # Very different or constant pasture assumptions
    
    df_stat <- df_stat %>% filter(Study %notin% excluded_studies) 
    # mutate(C_price = case_when(Study=="Springmann et al. (2018)"~ 0, # Carbon price does not have a feedback on land use in this paper
    #                            TRUE ~ C_price)) %>% 
    # mutate(C_price_cat = case_when(C_price>0 ~ "Yes", # Creating a categorical variable for the carbon price since many studies don't apply it at all
    #                                  TRUE ~ "No"))
  }
  
  if (i==3){ # 
    
    excluded_studies <- c("Pfister et al. (2011)",
                          "Davis et al. (2016)") # Stated clearly that this study was not included as data is missing 
    
    df_stat <- df_stat %>% filter(Study %notin% excluded_studies) %>%
      filter(!ScenYear %in% c(2010:2012)) #%>%  # Added to remove these years from Davis et al. (2016) otherwise too almost many duplicate scenarios
      #filter(!grepl('Case 4',Scenario..author.code.)) # This scenario has an ambiguously defined diet.
  }
  
  if (i==4){ # 
    
    excluded_studies <- c()
    df_stat <- df_stat %>% filter(Study %notin% excluded_studies)
    
  }
  
  if (i==5){ # 
    
    excluded_studies <- c("Roos et al. (2017) with pasture","Roos et al. (2017) No pasture","FAO (2018)",
                          "Clark et al. (2020)") # N2O results are not dynamic and do not consider yield/fertiliser impact on N2O emissions
    
    df_stat <- df_stat %>% filter(Study %notin% excluded_studies)
  
  }
  
  if (i==6){ # 
    
    excluded_studies <- c("Odegard & van der Voet (2014)","Pradhan et al. (2015)")
    excluded_scenarios <- c("TSS") # Organic scenarios with extreme outlier results
    
    df_stat <- df_stat %>% 
      filter(Study %notin% excluded_studies) %>% 
      filter(Scenario..author.code. %notin% excluded_scenarios) # Extreme outliers
    
  }
  
  if (i==7){ # 
    
    excluded_studies <- c("Muller et al. (2017)") # Starting values are radically different and only constant NUE assumption
    
    df_stat <- df_stat %>% 
      filter(Study %notin% excluded_studies) %>% 
      filter(Organic==0) # Removing organic scenarios (cross-validation not possible)
      
  }
  
  if (i==8){ # 
    
    excluded_studies <- c("Metson et al. (2012)") # Yield values are missing 
    excluded_scenarios <- c("TSS") # Organic scenarios with extreme outlier results
    
    df_stat <- df_stat %>% 
      filter(Study %notin% excluded_studies) %>% 
      filter(Scenario..author.code. %notin% excluded_scenarios) # Extreme outliers
  }
  
  if (i==9){ # 
    
    df_stat <- df_stat %>% 
      filter(Organic==0) # Removing organic scenarios
  }

  return(df_stat)
  
}

# Exploring distribution of dependent variables - outsourced script
#source("Scripts/Distributions.R")

# Choosing the right GLMM family based on AIC comparison of dependent variable distribution

# distributions <- c("norm","lnorm","gamma") # Testing all three potential candidates for positive continuous data
# 
# distr_aic <- list()
# distr_fit <- list()
# for (distribution in distributions) {
#   distr_fit[[distribution]] <- fitdist(df_stat[,ind[i]], distribution)
#   distr_aic[[distribution]] <- distr_fit[[distribution]]$aic
# }
# 
# distr_choice <- order(as.numeric(distr_aic)) # Index of distribution with lowest AIC (where 1=normal, 2=lognormal, 3=gamma)
# distr_choice

# qqp(df_stat[,ind[i]], "norm") # Check whether data is normalised by removing outliers?
# qqp(log(df_stat[,ind[i]]), "norm")
# gamma <- fitdistr(df_stat[,ind[i]], "gamma")
# qqp(df_stat[,ind[i]], "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#descdist(df_stat[,ind[i]], discrete = FALSE, boot = 500)

