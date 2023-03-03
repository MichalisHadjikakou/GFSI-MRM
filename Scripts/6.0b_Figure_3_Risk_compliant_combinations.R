##################################################
## FUTURE FOOD SCENARIOS - Parallel set plots ##
##################################################

### 1. Loading packages and functions and specifying working directory and parameters
rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,viridis,future.apply,data.table)

#library(magrittr)
#library(dplyr)
#library(tibble)
#ggalluvial,ggforce,palmerpenguins,easyalluvial
#remotes::install_github("yaweige/ggpcp", build_vignettes = TRUE)

PC <- 'denethor'
n <- 10000 
penalty <- 'no'
ambition_cutoff <- 'no'
results_date <- '23-02-07'

if(PC=='work_laptop') {
  setwd("C:/Users/Hadj/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/")
} else if (PC =='analytix') {
  setwd("M:/Current-Users/Michalis-Hadjikakou/Food_Meta_Analysis_Enayat/Notebook/")
} else {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSS-MM/Outputs/Pareto_analysis+plots/")  
}

# If there is a penalty read the penalty file
if (penalty=='yes') {
  
  All_scen <- fread(paste0("All_merged_scenarios_penalty_",results_date,".csv"))
}else{
  All_scen <- fread(paste0("All_merged_scenarios_",results_date,".csv"))
}  

# Risk cut-off parameters
threshold_high <- 0.5
threshold_CC_high <- 0.5
threshold_low <- 1/3
threshold_CC_low <- 0.40
perc_cut  <- c(1,0.5,0.25,0.1) # Choice of ambition percentile to consider 
scaling_factor <- 10 # scales the low risk scenarios to ensure they are visible

# Plotting parameters
red_spectral <- RColorBrewer::brewer.pal(n = 11, name = "Spectral")[1]
blue_spectral <- RColorBrewer::brewer.pal(n = 11, name = "Spectral")[10]

boundaries <- c("Land-system change","Climate change","Freshwater use","Biogeochemical flows","All boundaries")

dp1 <- 1 # Number of decimal places
dp_string1 <- paste0("%0.",dp1,"f")

dp2 <- 2 # Number of decimal places
dp_string2 <- paste0("%0.",dp2,"f")

### 2. Data processing and recoding

## 2.1 All scenarios

# Creating summary table with percentages of different risk threshold across boundaries

sel_cols <- c("Risk_Land_system_change","Risk_Climate_Change","Risk_Freshwater_Use","Risk_Biogeochemical_Flows")

df_table <- All_scen %>% 
  mutate(across(all_of(sel_cols), # Combinations that satisfy 0.5 risk for each boundary
                ~ case_when(
                  .<0.5 ~ "Low_risk",
                  TRUE ~ "High_risk"),
                .names = "{col}_0.50")) %>% 
  mutate(across(all_of(sel_cols), # Combinations that satisfy 0.33 risk for each boundary
                ~ case_when(
                  .<1/3 ~ "Low_risk",
                  TRUE ~ "High_risk"),
                .names = "{col}_0.33")) %>% 
  mutate(Risk_Climate_Change_0.33 = # Correction for climate change
           case_when(
             Risk_Climate_Change<0.4 ~ "Low_risk",
             TRUE ~ "High_risk"
           )) %>% 
  mutate(All_boundaries_0.50 = case_when(
    Risk_Land_system_change <= threshold_high & Risk_Freshwater_Use <= threshold_high & Risk_Biogeochemical_Flows <= threshold_high & Risk_Climate_Change <= threshold_CC_high ~ "Low_risk",
    TRUE ~ "High_risk")) %>%
  mutate(All_boundaries_0.33 = case_when(
    Risk_Land_system_change <= threshold_low & Risk_Freshwater_Use <= threshold_low & Risk_Biogeochemical_Flows <= threshold_low & Risk_Climate_Change <= threshold_CC_low ~ "Low_risk",
    TRUE ~ "High_risk")) %>% 
  select(Risk_Land_system_change_0.50:All_boundaries_0.33) # Selecting only desirable columns

# Processing the output to a summary table

n <- dim(df_table)[1] # total number of combinations

df_counts <- df_table %>% # Counts of eligible scenarios 
  apply(2,table) %>% # applying the table function to each column
  data.frame() %>% 
  slice(2) %>% 
  as.numeric()

seq_0.5 <- c(1:4,9) # Order for 2.5
seq_0.33 <- c(5:8,10) # Order for 2.5

df_table_sum <- data.frame(
  Boundary = boundaries,
  Low_risk = df_counts[seq_0.33]/n*100,
  Moderate_risk = df_counts[seq_0.5]/n*100)

# Creating dataframe for intervention level plots

df_recoded <- All_scen %>% 
  mutate(Likely = case_when(
    Risk_Land_system_change <= threshold_high & Risk_Freshwater_Use <= threshold_high & Risk_Biogeochemical_Flows <= threshold_high & Risk_Climate_Change <= threshold_CC_high ~ "Risk<0.50",
    TRUE ~ "Risk>0.50")) %>% 
  mutate(Likely = case_when(
    Risk_Land_system_change <= threshold_low & Risk_Freshwater_Use <= threshold_low & Risk_Biogeochemical_Flows <= threshold_low & Risk_Climate_Change <= threshold_CC_low ~ "Risk<0.33",
    TRUE ~ Likely)) %>% 
  select(Pop_levels:N_management,Likely,Ambition_all_boundaries,Risk_all_boundaries)


# 2.2 Lowest ambition scenarios by class
  
  if (ambition_cutoff=='yes'){
    
    df_recoded <- df_recoded %>% 
      filter(Likely!="Risk>0.50") %>% # Removing high risk scenarios
      group_by(Likely) %>%  # Grouping by risk class
      slice_min(order_by = Ambition_all_boundaries,prop = perc_cut[x], with_ties = FALSE) %>% # Ranking by ambition and selecting top 10%
      ungroup()
    #arrange(Ambition_all_boundaries,.by_group = TRUE) %>% 
    #slice(1:10)
  }else{
    
    x <- ""
    
  }
  
  N_scen <- df_recoded$Likely %>% 
    table() %>% 
    data.frame() %>% 
    rename(Risk_level=".") %>%  
    mutate(Percentage=Freq/sum(Freq)*100)
  
  N_scen_likely <- sum(N_scen$Freq[1:2]) # Selecting only the scenarios that meet high and low threshold
  
  ### 3. Plotting all figures
  
  # Compliant combinations by boundary
  
  df_table_plot <- df_table_sum %>% 
    pivot_longer(cols = Moderate_risk:Low_risk,names_to = "Risk_threshold",values_to = 'Percentage') %>% 
    mutate(Risk_threshold = case_when(
      Risk_threshold == "Low_risk" ~ "Risk<0.33",
      TRUE ~ "Risk<0.50"
    )) #%>% 
  #mutate(Percentage = case_when(
  #  Percentage<10 ~ sprintf(dp_string2,Percentage),# Number of significant figures depending on whether percentage is less or more than 10
  #  TRUE ~ sprintf(dp_string1,Percentage)
  # ))
  
  boundary_risk <- df_table_plot %>% 
    ggplot(aes(x = factor(Boundary,levels = rev(boundaries)), y = Percentage, fill= factor(Risk_threshold,levels = c("Risk<0.33","Risk<0.50")))) + 
    geom_bar(position = "dodge", stat = "identity", width = 0.6) + 
    scale_fill_manual(values = c(red_spectral,blue_spectral),
                      labels = c("Risk<0.33","Risk<0.50")) +
    scale_y_continuous(expand = expansion(mult = c(0.1,0),add = c(0,0)),limits=c(-10,65))+
    # geom_text(aes(y = 20, label = paste(Percentage, "%", sep=" ")), 
    #           hjust = 1, size = 11 * 0.8 / .pt, color = "grey30")+
    geom_text(aes(y = -9, label = paste(
      case_when(
        Percentage<10 ~ sprintf(dp_string2,Percentage),# Number of significant figures depending on whether percentage is less or more than 10
        TRUE ~ sprintf(dp_string1,Percentage)),
      "%", sep=" ")),size = 3,
      position = position_dodge(width = .6), color = "black") +
    labs(x = "Planetary boundary", y = "Compliant combinations (%)") +
    theme_light() +
    coord_flip() +
    theme(#legend.position  = c(0.225, -0.2),
      #legend.direction = 'horizontal',
      #plot.margin      = grid::unit(c(0.1,0.1,2.5,0.1), 'lines'),
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_text(size=11.5),
      axis.text = element_text(size=10),
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank())
  
  # Comparing Low and lowest risk scenarios
  
  df_likely <- df_recoded %>% 
    filter(Likely!="Risk>0.50") %>%  # Removing scenarios that don't meet any threshold
    mutate_if(is.numeric,as.factor) %>% 
    mutate(n = case_when(
      Likely=="Risk<0.33" ~ 1/N_scen$Freq[1], 
      Likely=="Risk<0.50" ~ 1/N_scen$Freq[2])) %>% 
    mutate(Feed_composition = case_when(
      Feed_composition=="TREND" ~ "BAU",
      TRUE ~ Feed_composition)) %>% 
    mutate(N_management = case_when( # Recoding N_management name to ensure consistency
      N_management=="Current" ~ "Present",
      TRUE ~ N_management))
  
  df_best <- df_recoded %>% 
    filter(Likely=="Risk<0.33") %>% 
    mutate_if(is.numeric,as.factor)
  
  # Creating summary table with count data for each intervention
  Int_string <- df_likely %>% 
    select(Pop_levels:N_management) %>% 
    colnames()
  
  Int_names <- c("Population","Animal calories","Plant calories","Waste",
                 "Crop yields","Feed conversion ratio","Feed composition","Water-use efficiency","Climate action","N & P management")
  
  # df_summary <- lapply(seq_along(Int_string), function (x) df_likely %>% # Looping across interventions 
  #   group_by(across(c("Likely",Int_string[x]))) %>% 
  #   count() %>%
  #   magrittr::set_colnames(c("Risk_level","Int_level","Count")) %>% 
  #   add_column(Intervention=Int_names[x],.before = 1)) %>% 
  #   bind_rows() %>% 
  #   mutate(Int_level = case_when( # Harmonising level names by ambition
  #     Int_level %in% c("8.5","LOW ASF","2300","Half","1.6","HIGH","HIGH GRAIN/LOW GRASS","0.3","200","+30% NUE,30% REC") ~ "VH",
  #     Int_level %in% c("8.9","REDUCED MEAT","2500","BAU_Low","1.45","ACCELERATED","INTENSIFIED","0.2","100","+20% NUE,+20% REC") ~ "H",
  #     Ints _level %in% c("9.7","BAU DIET","2700","BAU_High","1.3","TREND","BAU","0.1","25","+10% NUE,10% REC") ~ "T",
  #     TRUE ~ "L"))
  
  df_summary <- lapply(seq_along(Int_string), function (x) df_likely %>% # Looping across interventions 
                         group_by(across(c("Likely",Int_string[x]))) %>% 
                         count() %>%
                         magrittr::set_colnames(c("Risk_level","Int_level","Count")) %>% 
                         add_column(Intervention=Int_names[x],.before = 1)) %>% 
    bind_rows() %>% 
    mutate(Int_level = case_when( # Harmonising level names by ambition
      Int_level %in% c("8.5","LOW ASF","2300","Half","1.6","HIGH","HIGH GRAIN/LOW GRASS","0.3","200","+30% NUE,30% REC") ~ "Very High",
      Int_level %in% c("8.9","REDUCED MEAT","2500","BAU_Low","1.45","ACCELERATED","INTENSIFIED","0.2","100","+20% NUE,+20% REC") ~ "High",
      Int_level %in% c("9.7","BAU DIET","2700","Current","1.3","TREND","BAU","0.1","25","+10% NUE,+10% REC") ~ "Trend",
      TRUE ~ "Low"))
  
  # Percentage stacked barplot
  
  df_summary_perc <- df_summary %>%  # Calculating a percentage column to enable stacked barplot
    group_by(Intervention,Risk_level) %>%
    mutate(pct= prop.table(Count) * 100)
  
  summary_plot <- df_summary_perc %>% 
    ggplot() + aes(Risk_level, pct, fill=factor(Int_level,levels = c("Low","Trend","High","Very High"))) +
    scale_fill_viridis(discrete = TRUE,begin = 0, end=1,option = "C",direction = -1,alpha = 0.85)+
    geom_bar(stat="identity",width = 0.8) +
    ylab("Level of mitigation ambition (%) ") +
    xlab("Risk mitigation threshold") + 
    #geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
    #          position=position_stack(vjust=0.5)) +
    facet_wrap(~factor(Intervention, levels=Int_names),ncol = 2) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size=11.5),
          axis.text = element_text(size=10),
          strip.text = element_text(size = 10),
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  #theme_bw()
  ggsave(paste0("Risk_level_summary_",penalty,"_",ambition_cutoff,"_",perc_cut[x],".tiff", sep = " ") , plot = summary_plot,
         dpi = 1200, width = 100, height = 200, units = "mm",device ="tiff",
         compression="lzw", type="cairo")
  
  leg_p1 <- cowplot::get_legend(boundary_risk)
  mod_p1 <- cowplot::plot_grid(boundary_risk + theme(legend.position = "none"), leg_p1, nrow = 2, rel_heights = c(1, 0.1))
  
  
  combined_bars <- cowplot::plot_grid(labels = c("A", "B"),
                                      boundary_risk,summary_plot,
                                      nrow=2,ncol=1,
                                      #scale = c(0.8,1),
                                      rel_heights = c(0.5,1))
  
  
  boundary_risk_mod <- cowplot::plot_grid(boundary_risk,NULL,nrow=2)
  
  combined_unstretched <- cowplot::plot_grid(labels = c("A","B"),
                                             boundary_risk_mod, summary_plot,
                                             rel_widths = c(1.1,1),
                                             ncol = 2,
                                             align = "h",axis = "b")
  
  # 2-column
  ggsave(paste0("Fig_3_unstretched.png") , plot = combined_unstretched,
         dpi = 1200,width = 7.35, height = 8.28, units = "in")
  
  # 1-column vertical figure
  ggsave(paste0("Fig_3.tiff") , plot = combined_bars,
         dpi = 1200, width = 100, height = 300, units = "mm",device ="tiff",
         compression="lzw", type="cairo")
  