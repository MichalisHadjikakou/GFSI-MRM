####################################################################################
## Figures showing risk zones and mitigation potential of alternative interventions
####################################################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Plot exceedance risk across all indicators and intervention combinations
# Outputs: Figures 2 and 3 in the main manuscript (using inputs from script 4.0a)

################################################################ 1. SETTING UP SCRIPT #########################################################################

## 1.1  Working directories, packages and key variables

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,ggtext,RColorBrewer,Hmisc,extraDistr,formattable,ggpubr,
               future.apply,svMisc,wppExplorer,wpp2019,readxl,svglite,patchwork)

sim_date <- "2024-11-04" # Date of simulation - Change this depending on file you want to read

## 1.2 Loading dataset setting up colours and strings for labels etc.

# Data
data <- read_excel(paste0("Outputs/Composite_barplots/Figure_2_data_table_",sim_date,".xlsx")) %>% 
  mutate(Level = case_when(Level=="+5% PUE,+15% REC" ~ "+10% NUE,+10% REC", # Changing P management to N management 
                           Level=="+10% PUE,+30% REC" ~ "+20% NUE,+20% REC",
                           Level=="+15% PUE,+45% REC" ~ "+30% NUE,30% REC",
                           .default = Level)) %>% 
  mutate(Predictor = case_when(Predictor=="P_management" ~ "N_management",
                               .default = Predictor))

# Environmental limits
PBs <- read.csv("Outputs/Environmental_limits/All_PB_limits.csv") %>% 
  dplyr::select(Indicator,Min,Mode,Max) %>% 
  mutate(Indicator = data$Indicator %>% unique()) %>% 
  mutate(across(Min:Max,
                ~case_when(
                  Indicator %in% "Land-System Change"~ ./10,
                  Indicator %in% c("Climate Change","Freshwater Use")~ ./1000,
                  .default = ./1)))

# PB colours used throughout
PB_cols <- c(brewer.pal(5,"Greens")[4],brewer.pal(8,"Oranges")[5],brewer.pal(3,"Blues")[3],brewer.pal(7,"Purples")[c(5)],brewer.pal(7,"Purples")[c(4)])

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
  rename(Risk_joint_mean = Risk) %>% 
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
  mutate(
         Predictor = dplyr::recode(Predictor, 'Pop_levels' = 'Population', 'Diet' = 'Animal\ncalories','Plant_kcal' = 'Plant\ncalories',
                           'Yield_levels' = 'Crop yields', 'Feed_efficiency' = 'Feed\nconversion', 'Feed_composition' = 'Feed\ncomposition',
                           'Carbon_price' = "Emissions\nintensity", 'WUEinc' = "Water-use\nefficiency",'N_management' = 'N & P\nmanagement')) %>% 
  mutate(across(Pred_avg:Pred_SD,
                ~case_when(
    Indicator %in% Nutrients~ .*1,
    .default = ./1000)))

# Adding the risk difference calculation

dp <- 2 # Number of decimal places
dp_string <- paste0("%0.",dp,"f")
text_colours <- c("black","gray30")

# Adding risk difference character vector
data_PB_risk <- data_mod %>% 
  filter(Int_level=="Trend") %>% 
  dplyr::select(Indicator,Predictor,Pred_avg,Risk_joint_mean) %>%
  rename(Trend_risk = Risk_joint_mean,Trend_pred = Pred_avg) %>%
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
  mutate(Pred_avg_char = case_when(
    Indicator=="Biogeochemical Flows N" ~ Pred_avg %>% round(0) %>% as.character(),
    .default = formatC(signif(Pred_avg,digits=3), digits=3, flag="#")
  )) %>% 
# Capping upper SD limits
  mutate(exc_avg = Pred_avg/Trend_pred, # Calculating exceedance levels
         exc_upper = (Pred_avg + 2*Pred_SD)/Trend_pred,
         exc_lower = (Pred_avg - 2*Pred_SD)/Trend_pred)%>% 
  mutate(upper = Risk_joint_mean, lower = Risk_joint_mean) %>% 
  mutate(upper = case_when(
    upper > 1 ~ 1,
    TRUE ~ upper
  )) %>% 
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

## 2.2 Plotting illustrative figure

# Creating markdown labels for legend (enabled using element markdown and 'ggtext' package)
indicators <- c(unique(Barplot_data$Indicator)[c(1,3)], unique(Barplot_data$Indicator)[-c(1,3)])

labels_mod <- c("Agricultural area (billion ha)",
            "GHG emissions (GtCO<sub>2</sub>-eq yr<sup>-1</sup>)",
            "Water withdrawals (km<sup>3</sup> yr<sup>-1</sup>)",
            "Nitrogen fertiliser (TgN yr<sup>-1</sup>)",
            "Phosphorus fertiliser (TgP yr<sup>-1</sup>)")

y_range <- list(c(2,6),c(-5,12),c(0,9000),c(40,120),c(0,20))
y_ticks <- list(seq(2,6,0.5),seq(-4,12,2),seq(0,9000,1000),seq(40,120,20),seq(0,20,4))

for (i in seq_along(indicators)){
  
  assign(paste0("p", i),ggplot(data_PB_risk %>% 
                                 dplyr::select(-c(Pred_avg_char,RD_char,text_col)) %>% 
                                 mutate(across(Pred_avg:Risk_upr,
                                               ~case_when(
                                                 Indicator %in% "Freshwater Use"~ .*1000,
                                                 .default = .*1))) %>% 
                                 filter(Indicator==indicators[i]) %>% 
                                 rename(Mode_est = Mode),
                                 aes(x=Int_level, y=Pred_avg))+
           
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = Max, ymax = Inf),
            alpha = 0.25,color = NA,
            fill = "grey")+
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = Min, ymax = Max),
              alpha = 0.03,color = NA,
              fill = "grey")+
    geom_hline(aes(yintercept= Risk_lwr),linewidth = 0.5,linetype = 1,color=blue_spectral,alpha=0.75)+
    geom_hline(aes(yintercept= Risk_upr),linewidth = 0.5,linetype = 1,color=red_spectral,alpha=0.75)+
    geom_hline(aes(yintercept= Mode_est),linewidth = 0.5,linetype = "dashed",color="black",alpha=0.75)+
    scale_y_continuous(expand = c(0,0),limits=y_range[[i]],breaks = y_ticks[[i]],
                       label=scales::comma)+
    ylab(labels_mod[i]) +
    theme_classic()+
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_markdown(),#element_text(size=5),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=10.5),
          axis.text.y = element_text(size=9.5),
          axis.ticks.x = element_blank()
         )
  )
}

dd <- data.frame(
  x1 = NA_real_, x2 = NA_real_,
  label = c('High risk zone (>high estimate)', 
            'Zone of increasing risk (low-high estimate)',
            'Safe operating space (<low estimate)')
)

p6 <- ggplot(dd, aes(x1, x2,
                     fill=factor(label,levels = c('High risk zone (>high estimate)', 
                                                  'Zone of increasing risk (low-high estimate)',
                                                  'Safe operating space (<low estimate)')))) +
  geom_col() +
  labs(x = '', y = '',color = "",fill="") +
  scale_fill_manual(values = c(alpha("grey", c(0.5, 0.03)),"white")) +
  theme(
        legend.justification = "left",
        legend.direction   = "vertical",
        legend.key.spacing.y = unit(0.25, 'lines'),
        legend.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=9.2),
        axis.text = element_blank(),
        axis.ticks = element_blank())  

dd2 <- data.frame(
  x1 = 1, x2 = 2,
  label = c('0.33 risk ("exceedance unlikely")',
            '0.50 risk ("exceedance about as unlikely as not")',
            'Environmental limit (best estimate)'),
  line_type = c("s","s","d")) %>% 
  slice(rep(1:dplyr::n(), each = 2))

p7 <- ggplot(dd2, aes(x1, x2, color = factor(label,levels = c('0.33 risk ("exceedance unlikely")',
                                                              '0.50 risk ("exceedance about as unlikely as not")',
                                                              'Environmental limit (best estimate)')))) +
  geom_line() +
  labs(x = '', y = '', color = '') +
  scale_linetype_manual(values = c(1,1,"dashed"))+
  scale_color_manual(values = alpha(c(blue_spectral,red_spectral,"black"),0.75)) +
  theme(
        legend.key.spacing.y = unit(0.25, 'lines'),
        legend.justification = "left",
        legend.text = element_text(size=9.2),
        legend.background = element_rect(fill = 'transparent'),
        legend.direction = "vertical",
        legend.spacing.y = unit(0, 'lines'))

p7 <- ggplot(dd2, aes(x1, x2, group=label, color=label,linetype=label)) + 
  geom_line() + 
  scale_linetype_manual(values = c("solid","solid","dashed"))+ 
  scale_color_manual(values = alpha(c(blue_spectral,red_spectral,"black"),0.75)) +
  theme(
    legend.title = element_blank(),
    legend.key.spacing.y = unit(0.25, 'lines'), 
    legend.justification = "left", 
    legend.text = element_text(size=9.2), 
    legend.background = element_rect(fill = 'transparent'), 
    legend.direction = "vertical", legend.spacing.y = unit(0, 'lines'))

legend_combined <- ggpubr::get_legend(p6) %>% as_ggplot() +
  ggpubr::get_legend(p7) %>% as_ggplot() + plot_layout(ncol=1,heights=c(1,1))

risk_guide <- p1+p2+p3+p4+p5+legend_combined + 
  plot_layout(ncol = 2,widths = c(1,1,1,1,1,1))+
  plot_annotation(tag_levels=list(c("a","b","c","d","e","",""),""))

ggsave(paste0("Outputs/Composite_barplots/Fig_2_Risk_guide_plot",Sys.Date(),".png", sep = " ") , 
       plot = risk_guide,
       dpi = 1200, width = 200, height = 200, units = "mm",device ="png", #compression="lzw",
       type="cairo")

## 2.3 Plotting figure showing interventions and combinations

my_lab <- c("Agricultural area<br>(billion ha)",
            "GHG emissions<br>(GtCO<sub>2</sub>-eq yr<sup>-1</sup>)",
            "Water withdrawals<br>(1000 km<sup>3</sup> yr<sup>-1</sup>)",
            "Nitrogen fertiliser<br>(TgN yr<sup>-1</sup>)",
            "Phosphorus fertiliser<br>(TgP yr<sup>-1</sup>)")

# Basic plotting parameters
  y_lim <- 1.5
  y_low <- 0.0
  
  summary_plot <- Barplot_data %>% 
    mutate(Indicator_strip = case_when(Indicator==indicators[1]~my_lab[1],
                                       Indicator==indicators[2]~my_lab[2],
                                       Indicator==indicators[3]~my_lab[3],
                                       Indicator==indicators[4]~my_lab[4],
                                       Indicator==indicators[5]~my_lab[5])) %>% 
    mutate(Max = case_when(Max>y_lim ~ y_lim,
                           .default = Max),
           Min = case_when(Min<y_low ~ y_low,
                           .default = Min),
           Int_level = case_when(Int_level=="Very High" ~ "VHigh",
                                 .default = Int_level)) %>% 
    rename(Mode_est = Mode) %>% 
    ggplot() + aes(Int_level, exc_avg,
                   colour = factor(Indicator,levels = indicators)) +
    scale_color_manual(values = alpha(PB_cols,1), labels=my_lab
    )+
    scale_x_discrete(limits = c("Low","Trend","High","VHigh"),
                     position = "bottom")+
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = Max, ymax = y_lim),
              alpha = 0.25,color = NA,
              fill = "grey")+
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = Min, ymax = Max),
              alpha = 0.04,color = NA,
              fill = "grey")+
    geom_hline(yintercept=1,linewidth = 0.075,linetype = 2)+
    geom_hline(yintercept=1.5,linewidth = 0.15,linetype = 1)+
    geom_hline(aes(yintercept= Risk_lwr),linewidth = 0.3,linetype = 1,color=blue_spectral,alpha=0.75)+
    geom_hline(aes(yintercept= Risk_upr),linewidth = 0.3,linetype = 1,color=red_spectral,alpha=0.75)+
    geom_pointrange(aes(ymin=exc_lower, ymax= exc_upper),fatten = 2.5)+
    scale_y_continuous(expand = expansion(mult = c(0.0,0.15),add = c(0,0.1)), limits=c(y_low,y_lim+0.1),breaks = c(0,0.5,1,y_lim),labels = scales::percent_format(accuracy = 1),
                       sec.axis = sec_axis(~ ., name = "Intervention", breaks = NULL, labels = NULL)) +
    geom_text(aes(label=Pred_avg_char,colour="gray20"),size = 4,y=y_lim+0.25,colour="gray20",
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
  dd <- data.frame(
    x1 = NA_real_, x2 = NA_real_,
    label = c('High risk zone', 'Zone of increasing risk','Safe operating space')
  )

    p2 <- ggplot(dd, aes(x1, x2,
                         fill=factor(label,levels = c('High risk zone', 'Zone of increasing risk','Safe operating space')))) +
    geom_col() +
    labs(x = '', y = '',color = "",fill="") +
    scale_fill_manual(values = c(alpha("grey", c(0.5, 0.03)),"white")) +
    theme(legend.position = c(0.5, 0), legend.direction   = 'horizontal',
          legend.background = element_rect(fill = 'transparent'),legend.text = element_text(size=10.5),
          axis.text = element_blank(),axis.ticks = element_blank())  
    
    dd2 <- data.frame(
      x1 = NA_real_, x2 = NA_real_,
      label = c('0.50 risk ("exceedance about as unlikely as not")','0.33 risk ("exceedance unlikely")')
    )
    
    p3 <- ggplot(dd2, aes(x1, x2, color = label)) +
      geom_line() +
      labs(x = '', y = '', color = '') +
      scale_color_manual(values = alpha(c(blue_spectral,red_spectral),0.75)) +
      theme(legend.position = c(0.5, 0), legend.direction   = 'horizontal',legend.text = element_text(size=10.5),
            legend.background = element_rect(fill = 'transparent'))
  
final_plot <- summary_plot + plot_spacer() + p2 + p3 + plot_layout(heights = c(1,0,0,0))
  
ggsave(paste0("Outputs/Composite_barplots/Fig_3_Composite_barplot_ggplot_",Sys.Date(),".png", sep = " ") , plot = final_plot,
       dpi = 1200, width = 260, height = 330, units = "mm",device ="png", #compression="lzw",
       type="cairo")

writexl::write_xlsx(Barplot_data,paste0("Outputs/Composite_barplots/Figure_3_all_data_",Sys.Date(),".xlsx"))

