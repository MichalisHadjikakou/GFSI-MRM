##################################################
## FUTURE FOOD SCENARIOS - Risk-compliant combinations ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Calculate intervention-level percentages of different risk-compliant combinations
# Outputs: Figure 4 in the main manuscript (using inputs from script 6.0a)

### 1. Loading packages and functions and specifying working directory and parameters
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,viridis,future.apply,data.table)

n <- 10000 
results_date <- '2024-11-01'

# Reading file with all scenario combinations

All_scen <- fread(paste0("Outputs/Pareto_analysis+plots/All_merged_scenarios_",results_date,".csv"))

# Risk cut-off parameters
threshold_high <- 0.5
threshold_CC_high <- 0.5
threshold_low <- 1/3
threshold_CC_low <- 1/3
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
    )) 
  
  figure_labels <- c("All limits","Nutrient cycles: N & P","Surface water flows","GHG emissions","Agricultural area")
  
  max_value <- df_table_plot$Percentage %>% max() %>% signif(1)
  all_breaks <- c(0,20,40,60,80)
  
  boundary_risk <- df_table_plot %>% 
    mutate(Boundary = case_match(
      Boundary,
      "Land-system change" ~ "Agricultural area",
      "Climate change" ~ "GHG emissions",
      "Freshwater use" ~ "Surface water flows",
      "Biogeochemical flows" ~ "Nutrient cycles: N & P",
      "All boundaries" ~ "All limits"
    )) %>% 
    ggplot(aes(x = factor(Boundary,levels = figure_labels), y = Percentage, fill= factor(Risk_threshold,levels = c("Risk<0.33","Risk<0.50")))) + 
    geom_bar(position = "dodge", stat = "identity", width = 0.6) + 
    scale_y_continuous(expand = expansion(mult = c(0.1,0),add = c(0,0)),
                       limits=c(-15,max_value),breaks = all_breaks)+
    geom_text(aes(y = -13, label = paste(
      case_when(
        Percentage<10 ~ sprintf(dp_string2,Percentage),# Number of significant figures depending on whether percentage is less or more than 10
        TRUE ~ sprintf(dp_string1,Percentage)),
      "%", sep=" ")),size = 3,
      position = position_dodge(width = .6), color = "black") +
    labs(x = "Environmental limit", y = "Compliant combinations (%)") +
    theme_light() +
    coord_flip() +
    scale_fill_manual(values = alpha(c(red_spectral,blue_spectral),0.75),
      breaks = c("Risk<0.50","Risk<0.33"))+
    theme(
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
                 "Crop yields","Feed conversion ratio","Feed composition","Water-use efficiency","Emissions intensity","N & P management")
  
  df_summary <- lapply(seq_along(Int_string), function (x) df_likely %>% # Looping across interventions 
                         group_by(across(c("Likely",Int_string[x]))) %>% 
                         count() %>%
                         magrittr::set_colnames(c("Risk_level","Int_level","Count")) %>% 
                         add_column(Intervention=Int_names[x],.before = 1)) %>% 
    bind_rows() %>% 
    mutate(Int_level = case_when( # Harmonising level names by ambition
      Int_level %in% c("9.1","LOW ASF","2300","Half","1.6","HIGH","HIGH GRAIN/LOW GRASS","0.15","200","+30% NUE,30% REC") ~ "Very High",
      Int_level %in% c("9.5","REDUCED MEAT","2500","BAU_Low","1.45","ACCELERATED","INTENSIFIED","0.1","100","+20% NUE,+20% REC") ~ "High",
      Int_level %in% c("9.7","BAU DIET","2700","Current","1.3","TREND","BAU","0.05","25","+10% NUE,+10% REC") ~ "Trend",
      TRUE ~ "Low"))
  
  # Percentage stacked barplot
  
  df_summary_perc <- df_summary %>%  # Calculating a percentage column to enable stacked barplot
    group_by(Intervention,Risk_level) %>%
    mutate(pct= prop.table(Count) * 100)
  
  summary_plot <- df_summary_perc %>% 
    ggplot() + aes(factor(Risk_level,levels = c("Risk<0.50","Risk<0.33")), pct, fill=factor(Int_level,levels = c("Low","Trend","High","Very High"))) +
    scale_fill_viridis(discrete = TRUE,begin = 0, end=1,option = "C",direction = -1,alpha = 0.85)+
    geom_bar(stat="identity",width = 0.8) +
    ylab("Level of mitigation ambition (%) ") +
    xlab("Risk mitigation threshold") + 
    facet_wrap(~factor(Intervention, levels=Int_names),ncol = 2) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size=11.5),
          axis.text = element_text(size=9.5),
          strip.text = element_text(size = 10),
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  leg_p1 <- cowplot::get_legend(boundary_risk)
  mod_p1 <- cowplot::plot_grid(boundary_risk + theme(legend.position = "none"), leg_p1, nrow = 2, rel_heights = c(1, 0.1))
  
  
  combined_bars <- cowplot::plot_grid(labels = c("a", "b"),
                                      boundary_risk,summary_plot,
                                      nrow=2,ncol=1,
                                      #scale = c(0.8,1),
                                      rel_heights = c(0.5,1))
  
  boundary_risk_mod <- cowplot::plot_grid(boundary_risk,NULL,nrow=2)
  
  combined_unstretched <- cowplot::plot_grid(labels = c("a","b"),
                                             boundary_risk_mod, summary_plot,
                                             rel_widths = c(1.1,1),
                                             ncol = 2,
                                             align = "h",axis = "b")
  
  # 2-column
  ggsave(paste0("Outputs/Pareto_analysis+plots/Fig_4_unstretched_",Sys.Date(),".png") , plot = combined_unstretched,
         dpi = 1200,width = 7.35, height = 8.28, units = "in")
  
  # 1-column vertical figure
  ggsave(paste0("Outputs/Pareto_analysis+plots/Fig_4_",Sys.Date(),".tiff") , plot = combined_bars,
         dpi = 1200, width = 100, height = 300, units = "mm",device ="tiff",
         compression="lzw", type="cairo")
  
# Tidying up results dataframe
  df_results <- df_summary_perc %>% 
    tidyr::pivot_wider(names_from = Int_level,values_from=c(Count,pct)) %>% 
    rename(Risk_class = Risk_level)
  
#openxlsx::write.xlsx(df_results, "Intervention_ambition_by_risk_class.xlsx", sheetName = paste0("Ambition cut-off",as.character(perc_cut[x]*100)," %"), append=TRUE)
  write.csv(df_results,paste0("Outputs/Pareto_analysis+plots/Intervention_ambition_by_risk_class_",Sys.Date(),".csv"))
  
  