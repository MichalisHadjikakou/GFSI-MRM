##################################################
## FUTURE FOOD SCENARIOS - Model Avg_predictions ##
##################################################

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Last updated: 04 April 2020
# Purpose: Generate Avg_predictions and plot exceedance ratios across all indicators and predictor combinations
# Outputs: igure 3 in main manuscript

# Notes: In this script the barplots are plotted using exceedance relative to the mode (Risk_avg) and the uncertainty is over the min and max of
# the boundary (Risk_L & Risk_H) - previously we were using exceedance relative to the median boundary. Both are possible by modifying the script

################################################################ 1. SETTING UP SCRIPT #########################################################################

## 1.1  Working directories
rm(list = ls())
PC <- 'denethor'
write_results <- 'yes'
n <- 10000 # Number of random draws for distributions/simulations
sim_date <- "2022-10-06" # Date of simulation - Change this depending on file being read
today <- Sys.Date() # Today's date
Base_year <- 2010
Scen_year <- 2050
levels <- 4
Risk_metric <- 'relative' # Change to relative for +/- relative to trend

if(PC=='work_laptop') {
  setwd("N:/LES/Burwood/Brett-lab/Michalis/Future_food_systems_review/GFSS-MM/")
} else if (PC =='analytix') {
  setwd("N:/Future_food_systems_review/GFSS-MM/")
} else {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSS-MM/")  
}

capFirst <- function(s) { # Capitalise first letter
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr,tidyverse,RColorBrewer,Hmisc,extraDistr,formattable,future.apply,svMisc,wppExplorer,wpp2019,readxl)

# Setting up parallel processing for future.apply
#plan(multiprocess, workers = 4)

## Reading in necessary data

source("Scripts/All_variable_levels_bad_to_good.R") # All levels for all indicators

All_pred_lists <- readRDS(paste0("Outputs/Composite_barplots/Figure_1_data_list_",sim_date,".rds")) # Saving models for prediction

# Indicator names
ind <- c("Land_system_change","Climate_change","Freshwater_Use","Biogeochemical_Flows")
#"Nfert","Nsurplus","Pfert","Psurplus")

ind_names <- c("Land-system change","Climate change","Freshwater use","Biogeochemical Flows") # Revised indicator names

# Intervention/predictor names

All_preds <- as.character(expression(Pop_levels,Diet,Plant_kcal,Waste,
                                     Yield_levels,Feed_efficiency,Feed_composition,Carbon_price,WUEinc,N_management))

# Making adjustments to intervention/prediction levels

All_levels <- head(All_levels,-1) # Removing P_management as this intervention has been aggregated here 

All_levels[[which(All_preds=="Pop_levels")]] <- All_levels[[which(All_preds=="Pop_levels")]] %>% round(1)
All_levels[[which(All_preds=="Carbon_price")]] <- All_levels[[which(All_preds=="Carbon_price")]] %>% round(1)
All_levels_char <- lapply(All_levels, as.character) # Converting factors to strings

Trend_levels <- sapply(seq_along(All_levels_char),function(x)All_levels_char[[x]][2])  # BAU levels for each predictor

n_preds <- length(All_preds)

######################################### 2. CREATING COMPOSITE MULTIPLOT OF ALL PREDICTOR/INDICATOR COMBINATIONS ################################################

## Plotting using R base graphics - setting parameters to allow grid arrangement

# Creating new names for labels if necessary

All_levels_mod <- All_levels # Saving as fresh file

# Population
All_levels_mod[[1]] <- paste(round(All_levels_mod[[1]],1), rep("billion", 4))

# Diet
All_levels_mod[[2]] <- c("High-income (R:70, D:180, M:360)","BAU (R:55, D:160, M:295)","Reduced meat (R:40, D:160, M:230)","Low ASF (R:25, D:120, M:165)")

All_levels_mod[[2]] <- c("High-income (R:65, D:170, M:320)",
                         "BAU (R:50, D:150, M:260)",
                         "Reduced meat (R:35, D:150, M:200)",
                         "Low ASF (R:25, D:115, M:145)")
#Vegetal calories
# All_levels_mod[[3]] <- c(expression("2900 kcal cap"^-1*" day"^-1*"",
#                                     "2700 kcal cap"^-1*" day"^-1*"",
#                                     "2500 kcal cap"^-1*" day"^-1*"",
#                                     "2300 kcal cap"^-1*" day"^-1*""))
# 
# All_levels_mod[[3]] <- c(expression("2350 kcal cap"^-1*" day"^-1*"",
#                                     "2185 kcal cap"^-1*" day"^-1*"",
#                                     "2020 kcal cap"^-1*" day"^-1*"",
#                                     "1860 kcal cap"^-1*" day"^-1*""))

All_levels_mod[[3]] <- c(expression("2350 kcal cap"^-1*" day"^-1*"",
                                    "2200 kcal cap"^-1*" day"^-1*"",
                                    "2000 kcal cap"^-1*" day"^-1*"",
                                    "1850 kcal cap"^-1*" day"^-1*""))
# Waste 
All_levels_mod[[4]] <- c(expression(paste("+25 %", Delta), 
                                    paste("0 %", Delta), 
                                    paste("-25 %", Delta), 
                                    paste("-50 %", Delta)))

# # Animal calories
# All_levels_mod[[3]] <- c(expression("700 kcal cap"^-1*" day"^-1*"", 
#                                     "550 kcal cap"^-1*" day"^-1*"", 
#                                     "400 kcal cap"^-1*" day"^-1*"", 
#                                     "250 kcal cap"^-1*" day"^-1*""))
# Crop yields 
All_levels_mod[[5]] <- c(expression(paste("+15 %", Delta), 
                                    paste("+30 %", Delta), 
                                    paste("+45 %", Delta), 
                                    paste("+60 %", Delta)))
# Feed efficiency
All_levels_mod[[6]] <- c(expression(paste("Current",", 0 %", Delta), 
                                    paste("Trend gap closure",", +12.5 %", Delta), 
                                    paste("Accelerated gap closure",", +25 %", Delta), 
                                    paste("Full gap closure",", +37.5 %", Delta)))

# Feed efficiency
All_levels_mod[[7]] <- c("Low grain/high grass %","Historical ","Intensified","High grain/low grass %")

# GHG mitigation # All_levels_mod[[6]] <- c("0, 0","15, 1","30, 2","45, 3")
All_levels_mod[[8]] <- c(expression(paste("0/0 %", Delta, " CH4"[EI],"/N2O"[EI], ", $0 tCO"[2]*"eq"), 
                                    paste("-13/4 %", Delta, " CH4/N2O"[EI], ", $25 tCO"[2]*"eq"),
                                    paste("-27/8 %", Delta, " CH4/N2O"[EI], ", $100 tCO"[2]*"eq"),
                                    paste("-40/12 %", Delta, " CH4/N2O"[EI], ", $200 tCO"[2]*"eq")))
# Water-use efficiency
All_levels_mod[[9]] <- c(expression(paste("0 %", Delta), 
                                    paste("+10 %", Delta), 
                                    paste("+20 %", Delta), 
                                    paste("+30 %", Delta)))

# Nitrogen management 
All_levels_mod[[10]] <- c(expression(paste("0 %", Delta, " NUE", ", 0 %", Delta, " Recycling"), 
                                    paste("+10 %", Delta, " NUE", ", +10 %", Delta, " Recycling"), 
                                    paste("+20 %", Delta, " NUE", ", +20 %", Delta, " Recycling"), 
                                    paste("+30 %", Delta, " NUE", ", +30 %", Delta, " Recycling")))

# Phosphorus management 
All_levels_mod[[11]] <- c(expression(paste("0 %", Delta, " PUE", ", 0 %", Delta, " Recycling"), 
                                    paste("+5 %", Delta, " PUE", ", +15 %", Delta, " Recycling"), 
                                    paste("+10 %", Delta, " PUE", ", +30 %", Delta, " Recycling"), 
                                    paste("+15 %", Delta, " PUE", ", +45 %", Delta, " Recycling")))
# # Nutrient recycling
# All_levels_mod[[9]] <- paste(All_levels_mod[[9]], rep('%', 4))
# # Organic agriculture
# All_levels_mod[[10]] <- paste(All_levels_mod[[10]], rep('%', 4))
# # Food-competing feedstuffs
# All_levels_mod[[11]] <- c(expression(paste("20 %", Delta), 
#                                      paste("0 %", Delta), 
#                                      paste("-20 %", Delta), 
#                                      paste("-40 %", Delta)))

#plot.new()
wd <- 7.25 # 5.75
ht <- 9.3 #  wd * (7.5/5.75)
# Save as pdf (2 columns - 4.75 inches width)    
# dev.copy(pdf,paste0('Results/Composite_barplots/Fig_3_Composite_barplot_',today,'.pdf'), width=wd, height=ht)
png(filename = paste0('Outputs/Composite_barplots/Fig_1_Composite_barplot_',Risk_metric,'_',today,'.png'), width=wd, height=ht, units='in', res=600)

# Creating layout arrangement for plot
# m <- rbind(matrix(1:(n_preds*4), ncol=4, byrow=TRUE), rep((n_preds * 4) + 1, 4)) # 12 rows for each intervention and 4 levels = 48 panels - first row left empty
m <- rbind(matrix(1:(n_preds*4), ncol=4, byrow=TRUE)) # 12 rows for each intervention and 4 levels = 48 panels - first row left empty

layout(mat = m)#heights = c(0.4,0.4,0.2)) 

par(mai = c(0, 0, 0.35, 0),omi=c(0.5, 0.4, 0.4, 0.7)) # set margins mar=c(0.3, 0.25, 0, 0.25),oma=c(0.2,0,4,0.2),

#layout.show(max(m))

level_num <- length(All_pred_lists) # Total number of predictors
alpha_num <- 0.85 # Adding alpha to plot colours

#par(mfrow = c(12, 5)) # 12-by-5 grid of plots

ylimits <- c(0,1) # Setting limits for y-axis
lw <- 0.5
cx <- 0.9
hpadj <- rep(1.15,length(All_pred_lists))
hpadj[1] <- 0.95
hpadj[2] <- 1
hpadj[7] <- 1
#hpadj <- c(0.3, 0.6, 0.6, 0.6, 0.6, 0.55, 0.6, 0.55, 0.3, 0.3, 0.6) # for adjusting vertical height of the panel titles

# Looping through All_pred_lists that contains individual indicator/level combinations

for (i in 1:level_num){
  
  Pred_levels <- All_levels[[i]] # Determining the number of levels in the entire dataset
  
  for (j in 1:length(Pred_levels)){
  
    plot_data <- All_pred_lists[[i]] %>% filter(Level==as.character(Pred_levels[[j]])) %>%# dataset selection for each individual plot
                  dplyr::arrange(factor(Indicator, levels = ind)) %>% 
                  #mutate(Low_unc = Exc_Q25-((abs(Q75_PB-Median)/Median)))%>% #*Exc_Q25)) %>%
                  #mutate(Upp_unc = Exc_Q75+((abs(Q25_PB-Median)/Median)))%>%#*Exc_Q75))
                  #mutate(Exc_mode_L = Exc_mode/(Mode/Min), Exc_mode_H = Exc_mode/(Mode/Max)) %>% # Adding low and High exceedance boundaries 
                  dplyr::select(Level,Risk_joint_mean,Risk_joint_median,n,Risk_L,Risk_H,Risk_Q25,Risk_Q75,Indicator,Predictor) # Creating summarised dataframe of data to plot
      
    synth_data <- data.frame(Level= Pred_levels[j],Risk_joint_mean=NA,Risk_joint_median=NA,n=NA,Risk_L=NA,Risk_H=NA,Risk_Q25=NA,Risk_Q75=NA) %>% 
                      dplyr::slice(rep(1:dplyr::n(),length(ind))) # Creating synthetic dataframe specific to the predictor level
    synth_data$Indicator <- ind # Adding indicator column with all indicators as vector
    synth_data$Predictor <- rep(All_preds[i],length(ind)) # Adding the name of the Predictor
    ind_match <- base::match(plot_data$Indicator,synth_data$Indicator) # Finding non-zero indicators available in plot_data
    synth_data[ind_match,] <- plot_data # Complete dataframe with all values plus missing values
    synth_data$Colour <- c(brewer.pal(5,"Greens")[4],brewer.pal(8,"Oranges")[5],brewer.pal(3,"Blues")[3],brewer.pal(7,"Purples")[c(5)])
      
    # Formatting text to add above bars so that for levels 1,3,4 it considers risk difference relative to BAU levels
    
    dp <- 2 # Number of decimal places
    
    if (Risk_metric=='absolute'){
      
      Risk_abs <- synth_data$Risk_joint_mean # Trend levels just need the absolute risk number
      num_lab <- sprintf("%.*f", dp,Risk_abs)
      num_lab[num_lab=="NA"] <- "" 
      
      text_offset <- 0# Offset for text label above bar
      
    }else{
    
      if (j==2) {
    
        Risk_abs <- synth_data$Risk_joint_mean # Trend levels just need the absolute risk number
        num_lab <- sprintf("%.*f", dp,Risk_abs)
        num_lab[num_lab=="NA"] <- "" 
      
        text_offset <- 0# Offset for text label above bar
    
      }else {
      
        Risk_abs <- rep(NA,length(ind))
        non_na_risk <- !is.na(synth_data$Risk_joint_mean) # determine positions of non-na numbers
        Risk_trend <- All_pred_lists[[i]] %>% 
          dplyr::filter(Level==Trend_levels[[i]]) %>%
          dplyr::arrange(factor(Indicator, levels = ind)) %>% 
          pull(Risk_joint_mean) # extract risk for trend levels of predictor
        Risk_abs[non_na_risk] <- Risk_trend
        
        Risk_diff <- synth_data$Risk_joint_mean - Risk_abs # This is risk difference
        num_lab <- sprintf("%+.*f", dp,Risk_diff)
        num_lab[num_lab=="NA"] <- "" 
        
        text_offset <- 0.1# Offset for text label above bar
      }
    }
      
    if (j==1){ # Axis only on first column (this can easily be arranged in loop)
      
      #par(oma=c(0,2,0,0))
      par(mai = c(0.15, 0.0, 0.2, 0.1), xpd=FALSE)#,oma = c(0.25, 0.1, 0.1, 0.1)) # set margins mar=c(0.3, 0.25, 0, 0.25),oma=c(0.2,0,4,0.2),
      barplot(rep(NA,length(synth_data$Risk_joint_mean)), ylim=c(0,1), axes=FALSE)
      axis(3, at=seq(-0.5, 15.5, by=1), lwd=lw, lwd.ticks=0, las=2, hadj=0.2, labels=FALSE, col="gray70")
      abline(h=0.5, col="gray70", lty=1, lwd=lw)
      #abline(h=1, col="gray70", lty=1, lwd=lw)
      # grid(nx=NA, ny=2, col="gray70", lty=1, lwd=lw)
      grouped_bar = barplot(synth_data$Risk_joint_mean, beside=T, ylim=c(0,1), xaxt="n", axes = FALSE,add=TRUE,#panel.first = 
                            #c(abline(h=1, col="gray", lty=1, lwd=0.25),
                            #  abline(h=0.5, col="gray", lty=1, lwd=0.25)),
                            col=adjustcolor(synth_data$Colour,alpha_num),names.arg="",las=2,border=NA,cex.names=cx)
      # abline(h=0, col="black", lty=1, lwd=lw)
      axis(1, at=seq(-0.5, 15.5, by=1), lwd=lw, lwd.ticks=0, las=2, hadj=0.2, labels=FALSE)
      axis(2, at = seq(0, 1, by=0.5), lwd=lw, lwd.ticks=lw, las=2, tck=-0.05, cex.axis=cx-0.05, hadj=0.2)
      #title(capFirst(tolower(Pred_levels[j])),line = 0.5)
      #title(All_levels_mod[[i]][j], line=hpadj[i], cex.main=cx, font.main=1, xpd=T)
      text(as.vector(grouped_bar), 1.125, srt = 0, adj= 0.45+text_offset, xpd = TRUE, col = "gray60", cex=cx-0.05,
           labels = num_lab)

    } else{
      
      par(mai = c(0.15, 0.0, 0.2, 0.10), xpd=FALSE) # set margins mar=c(0.3, 0.25, 0, 0.25),oma=c(0.2,0,4,0.2),
      barplot(rep(NA,length(synth_data$Risk_joint_mean)), ylim=c(0,1), axes = "FALSE")
      abline(h=0.5, col="gray70", lty=1, lwd=lw)
      axis(3, at=seq(-0.5, 15.5, by=1), lwd=lw, lwd.ticks=0, las=2, hadj=0.2, labels=FALSE, col="gray70")
      #grid(ny = 2,nx = NA,col = 'gray30',lwd = 0.25,lty = 1)
      grouped_bar = barplot(synth_data$Risk_joint_mean, beside=T, ylim=c(0,1),xaxt = "n",axes = FALSE, add=TRUE,#panel.first = 
                            #c(abline(h=1, col="gray", lty=1, lwd=0.25),
                            #  abline(h=0.5, col="gray", lty=1, lwd=0.25)),
                            col=adjustcolor(synth_data$Colour,alpha_num),names.arg="",las=2,border=NA,cex.names=cx)
      # abline(h=0, col="black", lty=1, lwd=lw)
      axis(1, at=seq(-0.5, 15.5, by=1), lwd=lw, lwd.ticks=0, las=2, tck=-0.05, cex.axis=cx, hadj=0.2, labels=FALSE)
      #title(All_levels_mod[[i]][j], line=hpadj[i], cex.main=cx, font.main=1, xpd = T)

      if (Risk_metric=='relative'& j==2){ # Different colour for Trend if risk metric is relative
      text(as.vector(grouped_bar), 1.125, srt = 0, adj= 0.45+text_offset, xpd = TRUE, col = "gray30", cex=cx-0.05,
           labels = num_lab)
      }else{
      text(as.vector(grouped_bar), 1.125, srt = 0, adj= 0.45+text_offset, xpd = TRUE, col = "gray60", cex=cx-0.05,
             labels = num_lab)  
      }
    }  
    
    up_lim <- synth_data$Risk_H
    #up_lim[up_lim>max(ylimits)] <- max(ylimits)
    low_lim <- synth_data$Risk_L
    #low_lim[low_lim<0] <- 0    
    #low_lim[low_lim>max(ylimits)] <- max(ylimits)    
    Q25_lim <- synth_data$Risk_Q25
    Q75_lim <- synth_data$Risk_Q75
    
    errbar(grouped_bar, synth_data$Risk_joint_median, # Q5/95 error bar
           up_lim, # Upper limit
           low_lim,
           add=TRUE,
           cap=0.02, lwd=lw, cex=0, errbar.col="black") # Lower limit
    
    #rect(xleft=as.vector(grouped_bar)-0.125, ybottom=synth_data$Risk_joint_median, xright=as.vector(grouped_bar)+0.125, ytop=Q75_lim, lwd=lw, border="black", col="white") # top box
    #rect(xleft=as.vector(grouped_bar)-0.125, ybottom=Q25_lim, xright=as.vector(grouped_bar)+0.125, ytop=synth_data$Risk_joint_median, lwd=lw, border="black", col="white") # bottom box
    # errbar(grouped_bar,synth_data$Risk_avg, # Q25/75 error bar
    #        Q25_lim, # Upper limit
    #        Q75_lim,
    #        add=TRUE,
    #        cap=0.02, lwd = 0.5,cex=0,errbar.col="gray40") # Lower limit
  
    #box(which = "figure",lty = 'solid', col = 'grey')

  }

}  
    #par(mfg=c(i,5))
    # Add legend
    # par(xpd=TRUE)
    #plot(0,type='n',axes=FALSE,ann=FALSE)
    #legend("center",title.col = "black",bty = "n" ,cex = cx
    #ind_names, fill=adjustcolor(synth_data$Colour,alpha_num),ncol=6,border = "white") #horiz=TRUE
    # legend("bottomleft", title.col="black", bty="n", cex=0.9, x.intersp=0.25, y.intersp=1, xpd=NA, 
    #        ind_names, fill=adjustcolor(synth_data$Colour, alpha_num), ncol=6, border="white", 
    #        inset=c(-2.9,-1.93), text.width=c(7, 7, 6.5, 6.5, 5.3, 5.3, 5.9, 5.9, 7, 7, 6.7, 6.7)) 
    #par(lend=1) # Rectangular line endings
    legend("bottomleft", title.col="black", cex=cx*1.3, pt.cex = cx*5, x.intersp=0.15, y.intersp=1.3, xpd=NA, legend=ind_names, 
           fill=adjustcolor(synth_data$Colour, alpha_num), ncol=length(ind), border="white", #bty="n",
           inset=c(-3.3,-1.2))#,text.width=c(5,4.5,4,3.8)) 
    
    # Add text
    pj <- 0.6 #0.95 # adjusts row labels vertically the higher the number the lower the label
    aj <- 0
    tp <- 0.95
    gp <- 0.1
    cx <- 0.7
    ln <- -0.4
    fnt <- 2
    
    mtext("Population",                  side=4, outer=T, at=tp-(gp*0),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("Animal\ncalories",            side=4, outer=T, at=tp-(gp*1),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("Plant\ncalories",             side=4, outer=T, at=tp-(gp*2),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("Waste",                       side=4, outer=T, at=tp-(gp*3),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("Crop\nyields",                side=4, outer=T, at=tp-(gp*4),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("Feed\nefficiency",            side=4, outer=T, at=tp-(gp*5),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("Feed\ncomposition",           side=4, outer=T, at=tp-(gp*6),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("Climate\nchange\naction",     side=4, outer=T, at=tp-(gp*7),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("Water-\nuse\nefficiency",     side=4, outer=T, at=tp-(gp*8),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    mtext("N & P\nmanagement",        side=4, outer=T, at=tp-(gp*9),  line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)
    #mtext("Phosphorus\nmanagement",      side=4, outer=T, at=tp-(gp*10), line=ln, cex=cx, adj=aj, padj=pj, las=2, font=fnt)

    mtext("Risk of exceeding environmental limits", side=2, outer=T, at=0.51, line=1.4, cex=cx*1.3, padj=-0.2, font=1)

    mtext("Level of mitigation ambition", side=3, outer=T, at=0.49, line=1.4, cex=cx*1.3, font=1)
    mtext("Low",                          side=3, outer=T, at=0.119, line=0.1, cex=cx, font=fnt)
    mtext("Trend",                        side=3, outer=T, at=0.365, line=0.1, cex=cx, font=fnt)
    mtext("High",                         side=3, outer=T, at=0.618, line=0.1, cex=cx, font=fnt)
    mtext("Very High",                    side=3, outer=T, at=0.867, line=0.1, cex=cx, font=fnt)


    dev.off() 

    