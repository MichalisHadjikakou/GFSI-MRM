##################################################
##### FUTURE FOOD SCENARIOS - Data cleaning #####
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Data cleaning and pre-processing

select_studies <- function(df_stat,i){

  Years <- c(1995:2012,seq(2015,2070,5)) # Ensures all base years are covered
  
  df_stat <- df_stat %>% # Filtering in-between years
    dplyr::filter(ScenYear %in% Years) 
  
  if (i==1){ # 
    
    excluded_studies <- c("Bodirsky et al. (2014)","de Fraiture & Wichelns (2010)","Davis et al. (2016)",
                          "Roos et al. (2017) No pasture","Muller et al. (2017)","Weindl et al. (2017a)")
                          
    df_stat <- df_stat %>% 
      filter(Study %notin% excluded_studies) %>% 
      filter(Organic==0) %>% # Organic scenarios excluded due to yield implications
      arrange(Study,ScenYear) %>% 
      group_by(Study) %>% 
      mutate(across(c(Cropland),
                    ~ case_when(Cropland.scope=="Food crops" & ScenYear %in% c(2009,2010) ~ .+value_base_year[i]-dplyr::first(.),
                                  TRUE ~ .)))# Tested normalisation for food only studies (Clark et al.2020 and EAT-Lancet) to match 2010 FAOSTAT cropland
  }
  
  if (i==2){ # 
    
    excluded_studies <- c("Roos et al. (2017) No pasture","Muller et al. (2017)",
                          "de Fraiture & Wichelns (2010)") # Very different or constant pasture assumptions
    
    df_stat <- df_stat %>% filter(Study %notin% excluded_studies) %>% 
      filter(Pasture!=0) # Cannot calculate pasture yield change if value is zero

  }
  
  if (i==3){ # 
    
    excluded_studies <- c("Pfister et al. (2011)",
                          "Davis et al. (2016)") # Stated clearly that this study was not included as data is missing 
    
    df_stat <- df_stat %>% filter(Study %notin% excluded_studies) %>%
      filter(!ScenYear %in% c(2010:2012)) 
  }
  
  if (i==4){ # 
    
    excluded_studies <- c("FOLU (2019)","Chang et al. (2021)","Clark et al. (2020)","Roos et al. (2017) No pasture",
                          "Searchinger et al. (2018)")
    df_stat <- df_stat %>% filter(Study %notin% excluded_studies)
    
  }
  
  if (i==5){ # 
    
    excluded_studies <- c("Roos et al. (2017) No pasture","FAO (2018)",
                          "Clark et al. (2020)","FOLU (2019)","Searchinger et al. (2018)") 
    
    df_stat <- df_stat %>% 
      filter(Study %notin% excluded_studies) %>% 
      filter(Organic==0) # Removing organic scenarios as this profoundly impacts N applications and emissions

  }
  
  if (i==6){ # 
    
    excluded_studies <- c("Pradhan et al. (2015)",
                          "Bodirsky et al. (2014)",
                          "Davis et al. (2016)",
                          "Mogollon et al. (2018a)")
    excluded_scenarios <- c("TSS") # Organic scenarios 
    
    df_stat <- df_stat %>% 
      filter(Study %notin% excluded_studies) %>% 
      filter(Scenario..author.code. %notin% excluded_scenarios) # Extreme outliers
    
  }
  
  if (i==7){ # 
    
    excluded_studies <- c() # Starting values are radically different and only constant NUE assumption
    
    df_stat <- df_stat %>% 
      filter(Study %notin% excluded_studies) %>% 
      filter(Organic==0) %>% # Removing organic scenarios (cross-validation not possible due to small sample sizes)
      filter(PrecHousehold<0.5) # Removing EAT-Lancet scenarios with P recycling
  }

  return(df_stat)
  
}