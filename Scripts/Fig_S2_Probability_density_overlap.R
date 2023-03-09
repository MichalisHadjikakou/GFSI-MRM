##### Calculating area of overlapping density #####

## 1.0 Setting up script
PC <- "denethor"
sim_date <- "2022-12-07" # Date of simulation
today <- Sys.Date() # Today's date
unit_conv <- 100 # Passing from Mkm2 to Mha

if(PC=='work_laptop') {
  setwd("C:/Users/Hadj/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/Meta_analysis/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("M:/Food-Systems/Meta_analysis/")
} else if (PC =='denethor') {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSI-MRM/GFSI-MRM/")
} else {
  setwd("C:/Users/Michalis/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/Meta_analysis/GFSS-MM/")  
}

# Loading necessary packages and functions

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,fitdistrplus,propagate,extraDistr,distr,future.apply,merTools,readxl)

# Reading necessary files 
PBs <- read.csv("Outputs/Environmental_limits/All_PB_limits.csv") # Reading in PB distribution parameters
# Indicator and broad scenario names
ind <- PBs$Indicator 

## 2.0 Producing distributions
i <- 1 # Selecting cropland indicator

# Generating random samples
data <- read.csv(paste0("Outputs/Risk_estimates/",ind[i],"_",sim_date,".csv"))
BAU_scen <- data %>% filter(Pop_levels==9.7350339,Plant_kcal==2700, Waste =="Current",
                            Diet=="BAU DIET",Yield_levels==1.3,Feed_efficiency=="TREND",
                            Feed_composition=="TREND")


n <- 10000 # Number of simulations
mean_pred <- BAU_scen$Pred_avg
SD <- BAU_scen$Pred_SD
a <- rnorm(n,mean_pred,SD)
b <- rtriang(n,PBs$Min[PBs$Indicator==ind[i]],PBs$Max[PBs$Indicator==ind[i]],PBs$Mode[PBs$Indicator==ind[i]])*unit_conv # Triangular distribution of chosen boundary
mean_risk <- ptriang(mean_pred,PBs$Min[PBs$Indicator==ind[i]]*unit_conv,PBs$Max[PBs$Indicator==ind[i]]*unit_conv,PBs$Mode[PBs$Indicator==ind[i]]*unit_conv)

# aa <-  dnorm(c(400:4000), 746.2766204,63.62904609)
# bb <- dtriang(c(400:4000),669.95511,4044.23325,2452.93944)

## Method 1 = Simulation

c <- a > b
Risk <- sum(c)/n  

## Method 2 - Integration - source: https://stackoverflow.com/questions/41914257/calculate-area-of-overlapping-density-plot-by-ggplot-using-r

# FDensity = approxfun(density(a,from=4000, to=5500))
# MDensity = approxfun(density(b,from=4135, to=5477))
# 
# ## Solve for the intersection and plot to confirm
# FminusM = function(x) { FDensity(x) - MDensity(x) }
# Intersect = uniroot(FminusM, c(3000, 6000))$root
# #points(Intersect, FDensity(Intersect), pch=20, col="red")
# 
# integrate(MDensity, 40,Intersect)$value + 
#   integrate(FDensity, Intersect, 80)$value

## Method 3 - Overlapping package - source: https://stackoverflow.com/questions/41914257/calculate-area-of-overlapping-density-plot-by-ggplot-using-r
library(overlapping)
library(lattice)

x <- list(X1=a, X2=b)
out <- overlap(x, plot=TRUE)
#boot_out <- boot.overlap( x, B = 1000)

## 3.0 Plotting illustrative example (Cropland Ambitious Action scenario)

# Now, combine your two dataframes into one.  
# First make a new column in each that will be 
# a variable to identify where they came from later.
df_a <- data.frame(a)
df_b <- data.frame(b)

df_a$distribution <- "LMM prediction"
df_b$distribution <- "Environmental limits"

colnames(df_a) <- c("num","PDF")
colnames(df_b) <- c("num","PDF")

# and combine into your new data frame vegLengths
Distributions <- rbind(df_a, df_b)

#ggplot(df_a, aes(num, fill = letter)) + geom_histogram(alpha = 0.5,bins = 1000)
#ggplot(df_b, aes(num, fill = letter)) + geom_histogram(alpha = 0.5,bins = 1000)
theme_update(plot.title = element_text(hjust = 0.5)) # Ensures the graph title is in the middle
mytheme <- theme(axis.line = element_line(size = 1.5, colour = "grey"),
                 panel.background = element_rect(fill = "white"),
                 axis.ticks = element_line(colour = "black", size=(2)),
                 plot.title = element_text(family="Helvetica",size=12,colour="black",face = "bold"),
                 axis.title = element_text(family="Helvetica",size=10.5,colour="black"), 
                 axis.text = element_text(family="Helvetica",size=9,colour="black"))

Plot_titles <- c("Land-system change","","","Freshwater use","Climate change","","","","Biogeochemical flows","","","")
# Axis_names <- c("Cropland (Mha)","Total Area (Mha)","Remaining forest (Mha)",expression(paste("Water consumption (km"^3,"/yr)")),
#                 expression(paste("GHGnonCO"[2],"(Gt CO"[2],"eq/yr)")),expression(paste("GHGLCA","(Gt CO"[2],"eq/yr)")),
#                 expression(paste("GHGdirectLUC","(Gt CO"[2],"eq/yr)")),expression(paste("LUC (Gt CO"[2],"eq/yr)")),
#                 "N fertiliser (Tg N/yr)","N loss (Tg N/yr)","P fertiliser (Tg P/yr)","P loss (Tg P/yr)")
# Axis_names <- c("Cropland (Mha)","Total Area (Mha)","Remaining forest (Mha)",expression(paste("Water consumption (km"^3,"/yr)")),
#                 expression("DirNonCO"[2]),"Dir+IndGHG",expression("DirNonCO"[2]*"+LUC"),expression(paste("LUC (Gt CO"[2],"eq/yr)")),
#                 "N fertilizer (Tg N/yr)","N surplus (Tg N/yr)","P fertilizer (Tg P/yr)","P surplus (Tg P/yr)")
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

Fig_S2 <- ggplot(Distributions, aes(num, fill = PDF))+ 

  # Creating background shading
  #annotate("rect", xmin = 0, xmax = Inf, ymin = min(Limits[[i]]), ymax = PB_min,alpha = 0.7,fill="#1A9900")+#,color = "grey", size = 0.5)+
  #annotate("rect", xmin = 0, xmax = Inf, ymin = PB_min, ymax = PB_mode,alpha = 0.7,fill="#FFFF00")+#,color = "grey", size = 0.5)+
  #annotate("rect", xmin = 0, xmax = Inf, ymin = PB_max, ymax = max(Limits[[i]]),alpha = 0.7,fill="#FF0000")+#,color = "grey", size = 0.5)+
  #annotate("rect", xmin = 0, xmax = Inf, ymin = PB_mode, ymax = PB_max,alpha = 0.7,fill="#ff8c00")+#,color = "grey", size = 0.5)+    

  geom_histogram(alpha = 0.55,bins = 500,position='identity') +
  scale_fill_manual(values=c("#003BEB","#66D9FF","#66D9FF"))+
  #coord_cartesian(xlim = Limits)+
  scale_y_continuous(expand = c(0, 0))+
  #scale_x_continuous(expand = c(0, 0),breaks = c(10,11,12,13,14,15,16))+
  #scale_x_continuous(limits=Limits,expand = c(0,0))+
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
ggsave(paste0("Outputs/Environmental_limits/Histograms",today,".png"), Fig_S2, width=3.19, height=2.63, units="in", dpi=600) # inches width restriction    
