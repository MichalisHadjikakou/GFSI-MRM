####################################################################################
## Figures showing risk zones and mitigation potential of alternative interventions
####################################################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Plot exceedance risk across all indicators and intervention combinations
# Outputs: Figure S4 (using inputs from script 4.0b)

################################################################ 1. SETTING UP SCRIPT #########################################################################

## 1.1  Working directories, packages and key variables

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,ggtext,RColorBrewer,Hmisc,extraDistr,formattable,ggpubr,
               future.apply,svMisc,wppExplorer,wpp2019,readxl,svglite,patchwork)

sim_date <- "2024-11-06" # Date of simulation - Change this depending on file you want to read

# Constants
GWP_CH4 <- 27.2 #AR6, 27.2 (non-fossil origin)
GWP_N2O <- 273 # AR6, 273 (updated N2O factor)

## 1.2 Loading dataset setting up colours and strings for labels etc.

# Data
data <- read_excel(paste0("Outputs/Composite_barplots/Figure_SM_additional_indicators_data_table_",sim_date,".xlsx")) %>% 
  mutate(Level = case_when(Level=="+5% PUE,+15% REC" ~ "+10% NUE,+10% REC", # Changing P management to N management 
                           Level=="+10% PUE,+30% REC" ~ "+20% NUE,+20% REC",
                           Level=="+15% PUE,+45% REC" ~ "+30% NUE,30% REC",
                           .default = Level)) %>% 
  mutate(Predictor = case_when(Predictor=="P_management" ~ "N_management",
                               .default = Predictor))

# Environmental limits (not used directly for this figure as limits do not apply to sub-indicators)
PBs <- read.csv("Outputs/Environmental_limits/All_PB_limits.csv") %>% 
  dplyr::select(Indicator,Min,Mode,Max) %>% 
  mutate(Indicator = data$Indicator %>% unique()) %>% 
  mutate(across(Min:Max,
                ~case_when(
                  Indicator %in% "Land-System Change"~ ./10,
                  Indicator %in% c("Climate Change","Freshwater Use")~ ./1000,
                  .default = ./1)))

# PB colours used throughout
PB_cols <- c(brewer.pal(5,"Greens")[3:4],brewer.pal(8,"Oranges")[4:6]) # Colours corresponding to sub-indicators

red_spectral <- RColorBrewer::brewer.pal(n = 11, name = "Spectral")[1]
blue_spectral <- RColorBrewer::brewer.pal(n = 11, name = "Spectral")[10]

# String of intervention names used in projection outputs
Int_names <- c("Population","Animal calories","Plant calories","Waste",
               "Crop yields","Feed efficiency","Feed composition","Water-use efficiency","Climate change action","N/P management")

Nutrients <- c("Biogeochemical Flows N","Biogeochemical Flows P")

################################################################ 2. PLOTTING FIGURES #########################################################################

## 2.1 Data pre-processing 

# Making naming convention for N/P management level 'Current' to ensure different name to Waste level
data_mod <- data %>%
  rename(Pred_SD = SD_avg) %>% 
  mutate(Level = case_when(
    Indicator %in% Nutrients & Predictor!="Waste" & Level=="Current" ~ "Present",
    .default = Level)) %>% 
# Recoding all levels, indicator and predictor names
  dplyr::rename(Int_level = Level) %>% 
  mutate(Int_level = case_when( # Harmonising level names by ambition
    Int_level %in% c("9.1","LOW ASF","2300","Half","1.6","HIGH","HIGH GRAIN/LOW GRASS","0.15","200","+30% NUE,30% REC") ~ "Very High",
    Int_level %in% c("9.5","REDUCED MEAT","2500","BAU_Low","1.45","ACCELERATED","INTENSIFIED","0.1","100","+20% NUE,+20% REC") ~ "High",
    Int_level %in% c("9.7","BAU DIET","2700","Current","1.3","TREND","BAU","0.05","25","+10% NUE,+10% REC") ~ "Trend",
    TRUE ~ "Low")) %>% 
  mutate(#Indicator= Indicator %>% str_to_lower() %>% str_to_sentence(),# Capitalising only first word
         Predictor = dplyr::recode(Predictor, 'Pop_levels' = 'Population', 'Diet' = 'Animal\ncalories','Plant_kcal' = 'Plant\ncalories',
                           'Yield_levels' = 'Crop yields', 'Feed_efficiency' = 'Feed\nconversion', 'Feed_composition' = 'Feed\ncomposition',
                           'Carbon_price' = "Emissions\nintensity", 'WUEinc' = "Water-use\nefficiency",'N_management' = 'N & P\nmanagement')) %>% 
  mutate(across(Pred_avg:Pred_SD,
                ~case_when(
                  Indicator=="CH4"~ .*GWP_CH4,
                  Indicator=="N2O"~ .*GWP_N2O,
                  .default = .*1)))

# Adding the risk difference calculation

dp <- 2 # Number of decimal places
dp_string <- paste0("%0.",dp,"f")
text_colours <- c("black","gray30")

# Adding risk difference character vector
data_PB_risk <- data_mod %>% 
  filter(Int_level=="Trend") %>% 
  dplyr::select(Indicator,Predictor,Pred_avg) %>%
  rename(Trend_pred = Pred_avg) %>%
  dplyr::select(-Predictor) %>% 
  distinct() %>% 
  right_join(data_mod,by = "Indicator") %>% # Merging back to original dataframe
  mutate(RD = Pred_avg - Trend_pred) %>%  # Calculating risk difference and round up to two decimal places
  mutate(RD_char = case_when(
    RD > 0 & Indicator!="Biogeochemical Flows N" ~ paste0("+",as.character(sprintf(dp_string,RD))),
    RD < 0 & Indicator!="Biogeochemical Flows N" ~ as.character(sprintf(dp_string,RD)),
    Int_level=='Trend' & Indicator!="Biogeochemical Flows N" ~ formatC(signif(Trend_pred,digits=3), digits=3, flag="#"),
    RD > 0 & Indicator=="Biogeochemical Flows N" ~ paste0("+",formatC(signif(RD,digits=3), digits=3, flag="#")),
    RD < 0 & Indicator=="Biogeochemical Flows N" ~ formatC(signif(RD,digits=3), digits=3, flag="#"),
    Int_level=='Trend' & Indicator=="Biogeochemical Flows N" ~ Trend_pred %>% round(0) %>% as.character())) %>% 
  mutate(Pred_avg_char = Pred_avg %>% round())%>% 
# Capping upper SD limits
  mutate(exc_avg = Pred_avg/Trend_pred, # Calculating exceedance levels
         exc_upper = case_when(Indicator=="CO2_LUC" ~(Pred_avg + 1*Pred_SD)/Trend_pred,
                               .default = (Pred_avg + 2*Pred_SD)/Trend_pred),
         exc_lower = case_when(Indicator=="CO2_LUC" ~(Pred_avg - 1*Pred_SD)/Trend_pred,
                               .default = (Pred_avg - 2*Pred_SD)/Trend_pred))%>% 
  mutate(text_col = case_when(
    Int_level=="Trend" ~ text_colours[1],
    TRUE ~ text_colours[2]
  )) %>% 
  left_join(PBs) %>% 
  mutate(Risk_lwr = qtriang(0.3, a = Min, b = Max, c = Mode, lower.tail = TRUE, log.p = FALSE),
         Risk_upr = qtriang(0.5, a = Min, b = Max, c = Mode, lower.tail = TRUE, log.p = FALSE))

## Illustrative figure
Barplot_data <- data_PB_risk %>%
  mutate(across(Min:Risk_upr,
                ~./Trend_pred))

## 2.3 Plotting figure showing interventions and combinations

indicators <- unique(Barplot_data$Indicator)

my_lab <- c("Cropland <br>(billion ha)",
            "Pasture <br>(billion ha)",
            "Methane <br>(GtCO<sub>2</sub>-eq yr<sup>-1</sup>)",
            "Nitrous oxide <br>(GtCO<sub>2</sub>-eq yr<sup>-1</sup>)",
            "Carbon dioxide <br>(GtCO<sub>2</sub>-eq yr<sup>-1</sup>)")

# Basic plotting parameters
  y_lim <- 2.05
  y_low <- 0.0
  
  summary_plot <- Barplot_data %>% 
    mutate(Indicator_strip = case_when(Indicator==indicators[1]~my_lab[1],
                                       Indicator==indicators[2]~my_lab[2],
                                       Indicator==indicators[3]~my_lab[3],
                                       Indicator==indicators[4]~my_lab[4],
                                       Indicator==indicators[5]~my_lab[5])) %>% 
    rename(Mode_est = Mode) %>%
    mutate(Int_level = case_when(Int_level=="Very High" ~ "VHigh",
                          .default = Int_level)) %>% 
    ggplot() + aes(Int_level, exc_avg,
                   colour = factor(Indicator,levels = indicators)) +
    scale_color_manual(values = alpha(PB_cols,1), labels=my_lab
    )+
    scale_x_discrete(limits = c("Low","Trend","High","VHigh"),
                     position = "bottom")+
    geom_hline(yintercept=1,linewidth = 0.075,linetype = 2)+
    geom_hline(yintercept=2,linewidth = 0.15,linetype = 1)+
    geom_pointrange(aes(ymin=exc_lower, ymax= exc_upper),fatten = 2.5)+
    scale_y_continuous(expand = expansion(mult = c(0.0,0.15),add = c(0,0.1)), limits=c(y_low,y_lim+0.1),breaks = c(0,0.5,1,1.5,2),labels = scales::percent_format(accuracy = 1),
                       sec.axis = sec_axis(~ ., name = "Intervention", breaks = NULL, labels = NULL)) +
    geom_text(aes(label=Pred_avg_char,colour="gray20"),size = 3.75,y=y_lim+0.25,colour="gray20",
    )+
    ylab("Percentage relative to 2050 TREND (TREND=100%)") +
    xlab("Level of mitigation ambition") + 
    facet_grid(factor(Predictor,levels = Barplot_data$Predictor %>% unique())~factor(Indicator_strip,levels=my_lab)) +
    theme_bw()+
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text = element_text(size=10.5),
          axis.title = element_text(size=13),
          strip.background = element_rect(colour="black", fill=NA),
          strip.text.x = element_markdown(size=11),
          strip.text.y = element_text(size=11),
          strip.background.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.text = element_markdown(size=11),
          legend.key.height= unit(0.01, 'line'),
          legend.key.width= unit(0.2, 'line'),
          legend.spacing.x = unit(1, 'line')
    )
  
  # Adding custom legend to explain shading
  #Source: https://stackoverflow.com/questions/74179948/get-names-of-a-color-based-on-alpha-value
  #Source: https://stackoverflow.com/questions/73286221/how-to-add-legend-manually-without-overwriting-the-others-legends-in-ggplot2
  
ggsave(paste0("Outputs/Composite_barplots/Fig_SM_Composite_barplot_ggplot_",Sys.Date(),".png", sep = " ") , plot = summary_plot,
       dpi = 1200, width = 250, height = 300, units = "mm",device ="png", #compression="lzw",
       type="cairo")

writexl::write_xlsx(Barplot_data,paste0("Outputs/Composite_barplots/Figure_SM_all_data_",Sys.Date(),".xlsx"))

