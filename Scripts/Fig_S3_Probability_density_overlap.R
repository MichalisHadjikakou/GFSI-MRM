##### Calculating area of overlapping density #####

## 1.0 Setting up script
sim_date <- "2024-11-01" # Date of simulation
today <- Sys.Date() # Today's date
unit_conv <- 100 # Passing from Mkm2 to Mha

# Loading necessary packages and functions

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,fitdistrplus,propagate,extraDistr,distr,future.apply,merTools,readxl)

# Reading necessary files 
PBs <- read.csv("Outputs/Environmental_limits/All_PB_limits.csv") # Reading in PB distribution parameters
# Indicator and broad scenario names
ind <- "TotalAgArea"
PB_name <- "Land_system_change"

## 2.0 Producing distributions
i <- 1 # Selecting agricultural area as an example indicator

# Reading data and selecting TREND (i.e.,BAU) scenario
data <- fread(paste0("Outputs/Risk_estimates/",PB_name,"_",sim_date,".csv"))
BAU_scen <- data %>% filter(Pop_levels==9.664,Plant_kcal==2700, Waste =="Current",
                            Diet=="BAU DIET",Yield_levels==1.3,Feed_efficiency=="TREND",
                            Feed_composition=="TREND")


n <- 10000 # Number of simulations
mean_pred <- BAU_scen$Pred_avg
SD <- BAU_scen$Pred_SD
a <- rnorm(n,mean_pred,SD)
b <- rtriang(n,PBs$Min[PBs$Indicator==ind[i]],PBs$Max[PBs$Indicator==ind[i]],PBs$Mode[PBs$Indicator==ind[i]])*unit_conv # Triangular distribution of chosen boundary

## Method 1 = Simulation
c <- a > b
Risk <- sum(c)/n  

## 3.0 Plotting illustrative example 

df_a <- data.frame(a)
df_b <- data.frame(b)

df_a$distribution <- "LMM prediction"
df_b$distribution <- "Environmental limits"

colnames(df_a) <- c("num","PDF")
colnames(df_b) <- c("num","PDF")

Distributions <- rbind(df_a, df_b)

theme_update(plot.title = element_text(hjust = 0.5)) # Ensures the graph title is in the middle
mytheme <- theme(axis.line = element_line(size = 1.5, colour = "grey"),
                 panel.background = element_rect(fill = "white"),
                 axis.ticks = element_line(colour = "black", size=(2)),
                 plot.title = element_text(family="Helvetica",size=12,colour="black",face = "bold"),
                 axis.title = element_text(family="Helvetica",size=10.5,colour="black"), 
                 axis.text = element_text(family="Helvetica",size=9,colour="black"))

Plot_titles <- c("Land-system change","","","Freshwater use","Climate change","","","","Biogeochemical flows","","","")

Axis_names <- c(expression("Total agricultural area (Mha)"),
                expression("Forest (Mkm"^2*")"),
                expression(paste("Water (km"^3, " yr"^-1*")")),
                expression(paste("DirNonCO"[2]," (GtCO"[2], "e yr"^-1*")")),
                expression(paste("DirGHG"," (GtCO"[2], "e yr"^-1*")")),
                expression(paste("Dir+IndGHG"," (GtCO"[2], "e yr"^-1*")")),
                expression(paste("DirNonCO"[2]*"+LUC (GtCO"[2], "e yr"^-1*")")),
                expression("Nfert (TgN yr"^-1*")"),
                expression("Nsurplus (TgN yr"^-1*")"),
                expression("Pfert (TgP yr"^-1*")"),
                expression("Psurplus (TgP yr"^-1*")"))

Limits <- c(10,17)

PB_mode <- PBs %>% filter(Indicator==ind[i]) %>% dplyr::select(Mode) %>% as.numeric
PB_max <- PBs %>% filter(Indicator==ind[i]) %>% dplyr::select(Max) %>% as.numeric
PB_min <- PBs %>% filter(Indicator==ind[i]) %>% dplyr::select(Min) %>% as.numeric

Fig_S3 <- ggplot(Distributions, aes(num, fill = PDF))+ 

  geom_histogram(alpha = 0.55,bins = 500,position='identity') +
  scale_fill_manual(values=c("#003BEB","#66D9FF","#66D9FF"))+
  scale_y_continuous(expand = c(0, 0))+
  labs(y="",x=Axis_names[i])+
  annotate("text", label = "a", x = PBs$Min[PBs$Indicator==ind[i]]*unit_conv, y = 5, color="grey20") +
  annotate("text", label = "b", x = PBs$Max[PBs$Indicator==ind[i]]*unit_conv, y = 5, color="grey20")+
  annotate("text", label = "c", x = PBs$Mode[PBs$Indicator==ind[i]]*unit_conv, y = 5, color="grey20")+
  theme(legend.position = "top",
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),legend.key.size = unit(15,"points"),#legend.box = "horizontal", 
  legend.title = element_blank(),axis.line.y =element_blank(),axis.ticks.y = element_blank(),axis.text.y= element_blank(),#element_text(size=12, face="bold"),
  legend.text = element_text(size = 8))+
  mytheme  
ggsave(paste0("Outputs/Environmental_limits/Figure_S3_Exceedance_risk_illustration",today,".png"), Fig_S3, width=3.19, height=2.63, units="in", dpi=1200) # inches width restriction    
