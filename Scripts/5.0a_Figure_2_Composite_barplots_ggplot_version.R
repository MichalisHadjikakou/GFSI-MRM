##################################################
## Figure 1 - Average risk crossbars ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Last updated: October 2022
# Purpose: Plot exceedance risk across all planetary boundary and predictor combinations
# Outputs: Figure 1 in main manuscript

# Notes: In this script the crossbars are plotted using mean exceedance and standard deviation. Previously we were using mean/median and 25th/75th. 

################################################################ 1. SETTING UP SCRIPT #########################################################################

## 1.1  Working directories, packages and key variables

rm(list = ls())
PC <- 'denethor'
if(PC=='work_laptop') {
  setwd("N:/LES/Burwood/Brett-lab/Michalis/Future_food_systems_review/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("N:/Future_food_systems_review/GFSS-MM/")
} else {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSS-MM/")  
}

sim_date <- "2023-02-07" # Date of simulation - Change this depending on file being read

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,RColorBrewer,Hmisc,extraDistr,formattable,future.apply,svMisc,wppExplorer,wpp2019,readxl)

## 1.2 Loading dataset setting up colours and strings for labels etc.

# Data
data <- read_excel(paste0("Outputs/Composite_barplots/Figure_2_data_table_",sim_date,".xlsx")) #%>% 
  #mutate(row_number= 1:n(),.before = 1) # Adding row number

# PB colours used throughout
PB_cols <- c(brewer.pal(5,"Greens")[4],brewer.pal(8,"Oranges")[5],brewer.pal(3,"Blues")[3],brewer.pal(7,"Purples")[c(5)])

# String of intervention names used in projection outputs
Int_names <- c("Population","Animal calories","Plant calories","Waste",
               "Crop yields","Feed efficiency","Feed composition","Water-use efficiency","Climate change action","N/P management")

################################################################ 2. PLOTTING FIGURE #########################################################################

## 2.1 Data pre-processing 

# Making naming convention for N/P management level 'current' to ensure different name to Waste level
data_mod <- data %>%
  mutate(Level = case_when(
    Indicator=="Biogeochemical_Flows"& Predictor=="N_management" & Level=="Current" ~ "Present",
    TRUE ~ Level)) %>% 
# Recoding all levels, indicator and predictor names
  dplyr::rename(Int_level = Level) %>% 
  mutate(Int_level = case_when( # Harmonising level names by ambition
    Int_level %in% c("8.5","LOW ASF","2300","Half","1.6","HIGH","HIGH GRAIN/LOW GRASS","0.3","200","+30% NUE,30% REC") ~ "Very High",
    Int_level %in% c("8.9","REDUCED MEAT","2500","BAU_Low","1.45","ACCELERATED","INTENSIFIED","0.2","100","+20% NUE,+20% REC") ~ "High",
    Int_level %in% c("9.7","BAU DIET","2700","Current","1.3","TREND","BAU","0.1","25","+10% NUE,+10% REC") ~ "Trend",
    TRUE ~ "Low")) %>% 
  mutate(Indicator = dplyr::recode(Indicator, 'Land_system_change' = 'Land-system change', 'Freshwater_Use' = 'Freshwater use', 
                       'Climate_change' = 'Climate change', 'Biogeochemical_Flows' = 'Biogeochemical flows'), 
         Predictor = dplyr::recode(Predictor, 'Pop_levels' = 'Population', 'Diet' = 'Animal\ncalories','Plant_kcal' = 'Plant calories',
                           'Yield_levels' = 'Crop yields', 'Feed_efficiency' = 'Feed\nconversion', 'Feed_composition' = 'Feed\ncomposition',
                           'Carbon_price' = "Climate\naction", 'WUEinc' = "Water-use\nefficiency",'N_management' = 'N & P\nmanagement'))

# Adding the risk difference calculation

dp <- 2 # Number of decimal places
dp_string <- paste0("%0.",dp,"f")
text_colours <- c("black","gray30")

# Adding risk difference character vector
Barplot_data <- data_mod %>% 
  filter(Int_level=="Trend") %>% 
  dplyr::select(Indicator,Predictor,Risk_joint_mean) %>%
  rename(Trend_risk = Risk_joint_mean) %>% 
  right_join(data_mod) %>% # Merging back to original dataframe
  mutate(RD = Risk_joint_mean - Trend_risk) %>%  # Calculatng risk difference and round up to two decimal places
  mutate(RD_char = case_when(
    RD > 0 ~ paste0("+",as.character(sprintf(dp_string,RD))),
    RD < 0 ~ as.character(sprintf(dp_string,RD)),
    Int_level=='Trend' ~ as.character(sprintf(dp_string,Trend_risk)))) %>% 
# Capping upper SD limits
  mutate(upper = Risk_joint_mean + Risk_SD, lower = Risk_joint_mean - Risk_SD) %>% 
  mutate(upper = case_when(
    upper > 1 ~ 1,
    TRUE ~ upper
  )) %>% 
  mutate(text_col = case_when(
    Int_level=="Trend" ~ text_colours[1],
    TRUE ~ text_colours[2]
  ))

## 2.2 Plotting
  
summary_plot <- Barplot_data %>% 
  ggplot() + aes(Indicator, Risk_joint_mean,
                 fill=factor(Indicator,levels = c("Land-system change","Climate change","Freshwater use","Biogeochemical flows")),
                 colour = factor(Indicator,levels = c("Land-system change","Climate change","Freshwater use","Biogeochemical flows"))) +
  scale_fill_manual(values = alpha(PB_cols,0.85))+
  scale_color_manual(values = alpha(PB_cols,1))+
  #geom_bar(stat="identity",width = 0.8) +
  scale_x_discrete(limits = c("Land-system change","Climate change","Freshwater use","Biogeochemical flows"),
                   position = "top")+
  scale_y_continuous(expand = expansion(mult = c(0.1,0.35),add = c(0,0)), limits=c(0,1),breaks = c(0,0.5,1),
                     sec.axis = sec_axis(~ ., name = "Intervention", breaks = NULL, labels = NULL)) +
  # geom_crossbar(aes(ymin=lower, ymax= upper), width=.6,size=0.25,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=lower, ymax= upper),fatten = 3.5)+
  geom_text(aes(label=RD_char,colour=text_col),size = 3,y=1.2,colour=Barplot_data$text_col,
            )+
  ylab("Risk of exceeding environmental limits") +
  xlab("Level of mitigation ambition") + 
  #geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
  #          position=position_stack(vjust=0.5)) +
  facet_grid(factor(Predictor,levels = Barplot_data$Predictor %>% unique())~factor(Int_level,levels = c("Low","Trend","High","Very High"))) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        #legend.box.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size=9),
        legend.key.size = unit(0.7, 'cm'), #change legend key size
        legend.spacing.x = unit(0.05, 'cm')
        # strip.text.x = element_text(
        #   size = 8, color = "black", face = "bold"
        # ),
        # strip.text.y = element_text(#angle = 0,
        #   size = 8, color = "black", face = "bold")
        )
#theme_bw()
ggsave(paste0("Outputs/Composite_barplots/Fig_1_Composite_barplot_ggplot_",Sys.Date(),".tiff", sep = " ") , plot = summary_plot,
       dpi = 1200, width = 175, height = 250, units = "mm",device ="tiff", compression="lzw",
       type="cairo")
