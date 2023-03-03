######### Deriving triangular distributions for PBs - for Agicultural GHG emissions, total agricultural area, water,nitrogen and Phosphorus #############

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Define agriculture environmental limits across all planetary boundaries


### 1. Loading packages and functions and specifying working directory and parameters

`%notin%` <- Negate(`%in%`)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(car,fitdistrplus,mc2d,readxl,propagate,tidyverse,future.apply)

plan(multiprocess, workers = 50)

FD <- function(x) { # Freedman-Diaconis rule used to select the size of the bins to be used in a histogram
  diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
}

mode_triang <- function(x){ # Method for estimating the mode of a triangular distribution based on mean: http://kb.palisade.com/index.php?pg=kb.page&id=44
  3*mean(x)-min(x)-max(x)
}

# 1.1 Specifying variables and working directories

PC <- 'denethor'
n <- 10000 
IAMC_database <- 'AR6' # OR change to 'AR6'
GWP_CH4 <- 27.2 #AR6, 27.2 (non-fossil origin)
GWP_N2O <- 273/1000 # AR6, 273 (updated N2O factor)
time_snapshot <- '2050'
Paris_compliance <- 'no'
scope <- "Total_AFOLU" # (Total_CH4_N2O)

if(PC=='work_laptop') {
  setwd("C:/Users/Hadj/OneDrive - Deakin University/Michalis_Brett/Future_food_systems_review/")
} else if (PC =='analytix') {
  setwd("C:/Users/hadj/OD/Michalis_Brett/Future_food_systems_review/")
} else {
  setwd("//school-les-m.shares.deakin.edu.au/school-les-m/Planet-A/Food-Systems/Meta_analysis/GFSS-MM/")  
}

# 2.1 Initialising empty dataframe with limits across all relevant indicators (for sub-indicators of GHG emissions see - 'GHG_distributions_2050_CH4_N2O_CO2_1_5+2_0C.R')

All_limits <- data.frame(Indicator = c("TotalAgArea","Water", "DirNonCO2LUC",
                                       "Nfert","Nsurplus","Pfert","Pexport","Pinstream")) %>% 
  add_column(Min=NA,Mode=NA,Max=NA)

# Scenario family strings - allows selecting and fitting distributions for 1.5 and 2C seperately or together
if(IAMC_database=='iamc15') {

  only_1.5 <- c("1.5C low overshoot","Below 1.5C","1.5C high overshoot")
  only_2 <- c("Higher 2C","Lower 2C")
  all <- c(only_1.5,only_2)
  
  sel_scens <- all # Selecting 1.5oC and 2oC scenarios - modify accordingly
  
  cat_emissions <- c("Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")
  non_CO2_cat <- c("Emissions|N2O|AFOLU","Emissions|CH4|AFOLU")

}else{

  only_1.5 <- c("C1: limit warming to 1.5°C (>50%) with no or limited overshoot")#,"C2: return warming to 1.5°C (>50%) after a high overshoot")
  only_2 <- c("C3: limit warming to 2°C (>67%)")
  all <- c(only_1.5,only_2)
  
  sel_scens <- all # Selecting only 1.5oC scenarios - modify accordingly
  
  # cat_emissions <- c("Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")
  # non_CO2_cat <- c("Emissions|N2O|AFOLU","Emissions|CH4|AFOLU")
  cat_emissions <- c("Emissions|CO2|AFOLU","Emissions|CH4|AFOLU|Agriculture","Emissions|N2O|AFOLU|Agriculture")
  non_CO2_cat <- c("Emissions|CH4|AFOLU|Agriculture","Emissions|N2O|AFOLU|Agriculture")
}

#### 2. Loading IAMC data - either IAMC15 or AR6 database depending on choice at the beginning of script

if(IAMC_database=='iamc15') {
    
  IAMC_data <- read_excel(path = "Input_data/Planetary_boundaries/iamc15_scenario_data_world_r2.0.xlsx",sheet = "data") #Reading all IPCC scenarios
  metadata_IAMC_sel <- read_excel(path = "Input_data/Planetary_boundaries/sr15_metadata_indicators_r2.0.xlsx",sheet = "meta") %>%
    filter(category %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
    pull(scenario)
  
  year_prefix <- ""
  
  Paris_compliant <- read_excel("Input_data/Planetary_boundaries/Schleussner_et_al_2022/sr15_updated_meta.xlsx") %>% #Source: https://www.nature.com/articles/s43247-022-00467-w#code-availability
    filter(category_new=="PA (Art 4)")

}else{

  master <- as.character(unzip("Input_data/Planetary_boundaries/1648976687084-AR6_Scenarios_Database_World_v1.0.zip", list = TRUE)$Name)
  # load the first file "file1.csv"
  IAMC_data <- read.csv(unz("Input_data/Planetary_boundaries/1648976687084-AR6_Scenarios_Database_World_v1.0.zip", 
                       "AR6_Scenarios_Database_World_v1.0.csv"), header = TRUE,
                   sep = ",") 
  # Extracting metadata file
  unzip(zipfile="Input_data/Planetary_boundaries/1648976687084-AR6_Scenarios_Database_World_v1.0.zip", files = "AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx", 
        exdir="Input_data/Planetary_boundaries/.")
  
  # metadata_IAMC <- read_excel(path = "Input_data/Planetary_boundaries/AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx",sheet = "meta_Ch3vetted_withclimate") %>%
  #   #filter(Category_name %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
  #   pull(Scenario) %>% unique()
  
  metadata_IAMC_sel <- read_excel(path = "Input_data/Planetary_boundaries/AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx",sheet = "meta_Ch3vetted_withclimate") %>%
    filter(Category_name %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
    dplyr::select(Scenario,Model)# %>% unique()
  
  Paris_compliant <- read_excel("Input_data/Planetary_boundaries/Schleussner_et_al_2022/AR6_Scenarios_Updated_Meta_MHExtract_290622.xlsx") %>% # Source: https://www.nature.com/articles/s43247-022-00467-w#code-availability
    filter(category_new_rolling_mean=="PA (Art 2 and 4)") %>% 
    rename(Scenario=scenario, Model=model) %>% 
    dplyr::select(Model,Scenario) 
  
  year_prefix <- "X"
}

# If Paris compliance, replace metadata IAMC_sel
if (Paris_compliance=='yes'){
  metadata_IAMC_sel <- Paris_compliant}

# Non-compliant scenarios 
data_AFOLU_non_compliant <- IAMC_data %>% # Reading all IPCC scenarios
  filter(Scenario %notin% metadata_IAMC_sel$Scenario) %>%
  filter(Model %notin% metadata_IAMC_sel$Model) %>%
  filter(Variable %in% cat_emissions) %>% 
  dplyr::select(Model,Scenario,Variable,Unit,all_of(paste0(year_prefix,time_snapshot))) %>% 
  add_column(Compliant = "No",.before=1)

# Compliant scenarios 
data_AFOLU_compliant <- IAMC_data %>% # Reading all IPCC scenarios
  filter(Scenario %in% metadata_IAMC_sel$Scenario) %>%
  filter(Model %in% metadata_IAMC_sel$Model) %>%
  filter(Variable %in% cat_emissions) %>% 
  dplyr::select(Model,Scenario,Variable,Unit,all_of(paste0(year_prefix,time_snapshot))) %>% 
  add_column(Compliant = "Yes",.before=1)

# Compliant and non-compliant
data_AFOLU <- bind_rows(data_AFOLU_non_compliant, # Merging all scenarios
          data_AFOLU_compliant)

# NonCO2 emissions
data_nonCO2 <- data_AFOLU %>% # Calculating total non-CO2 emissions
  dplyr::filter(Variable %in% non_CO2_cat) %>% 
  dplyr::select(-Variable) %>% 
  pivot_wider(names_from = Unit,values_from = paste0(year_prefix,time_snapshot)) %>% 
  rename(CH4 = `Mt CH4/yr`,N2O = `kt N2O/yr`) %>% 
  mutate(Total_CH4_N2O= CH4*GWP_CH4 + N2O*GWP_N2O)

# Total AFOLU
data_nonCO2LUC <- data_nonCO2 %>%  # Calculating total agriculture emissions from non-CO2 and LUC for all compatible scenarios
  left_join(data_AFOLU %>% filter(Variable %in% "Emissions|CO2|AFOLU")) %>% 
  rename(CO2_LUC = paste0(year_prefix,time_snapshot)) %>% 
  dplyr::select(Compliant:Total_CH4_N2O,CO2_LUC) %>% 
  mutate(Total_AFOLU=`Total_CH4_N2O`+CO2_LUC)

# Alternative way - comparing compliance to non-compliance

# # Creating bins of compliant and non-compliant scenarios
# data_nonCO2LUC <- data_nonCO2LUC %>%
#   drop_na() %>% 
#   filter(Total_AFOLU<20000 & Total_AFOLU>-10000)
# 
# bin_size = 2000
# 
# bin_compliance <- data_nonCO2LUC %>% 
#   drop_na() %>% 
#   group_by(Compliant) %>% 
#   mutate(group = cut(Total_AFOLU, breaks = seq(-10000, 20000, bin_size))) %>%
#   count(group) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = Compliant,values_from=n) %>% 
#   mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% 
#   mutate(perc_compliant = Yes/(Yes+No))%>%
#   data.frame()

#data_energy <- read.xlsx(xlsxFile = "IIASA data/iamc15_scenario_data_all_regions_r1.xlsx",sheet = "Energy",startRow = 1,colNames = TRUE) # Reading all IPCC scenarios
#data_energy_ind <- read.xlsx(xlsxFile = "IIASA data/iamc15_scenario_data_all_regions_r1.xlsx",sheet = "Energy+Industry",startRow = 1,colNames = TRUE) # Reading all IPCC scenarios
#data_transp <- read.xlsx(xlsxFile = "IIASA data/iamc15_scenario_data_all_regions_r1.xlsx",sheet = "Transport",startRow = 1,colNames = TRUE) # Reading all IPCC scenarios

# Fitting triangular distributions to data using the propagate package - see https://www.rdocumentation.org/packages/propagate/versions/1.0-6/topics/fitDistr

#### 3. GHG emissions - nonCO2 + CO2

# Total AFOLU (CH4, N2O, CO2)

# Trimming outliers to enable distribution fitting
#outliers <- boxplot(data_nonCO2LUC$Total_AFOLU,plot=FALSE)$out # Create boxplot of all data

#data_nonCO2LUC %>% filter(Total_AFOLU %notin% outliers)


Compliance <- data_nonCO2LUC$Compliant %>% unique() # Unique compliance condition
Scen_num <- data_nonCO2LUC$Compliant %>% table() %>% as.numeric() # Numbers of non-compliant and compliant scenarios

#dist_CH4_N2O <- lapply(1:length(data_nonCO2$`Total_CH4_N2O`),function(x)fitDistr(data_nonCO2$`Total_CH4_N2O`,distsel = 8,nbin = x)) # Fitting distribution to AFOLU CH4+NO
dist_Total <- 
  lapply(seq_along(Compliance),function(x)
    future_lapply(1:Scen_num[x],function(y) data_nonCO2LUC %>% 
                                                         filter(Compliant == Compliance[x]) %>%
                                                         pull(scope) %>% 
                                                         fitDistr(distsel=1,nbin = y))) # Fitting 32 different distributions to AFOLU TOTAL - normal fits best for compliant scenarios
#dist_LU <- lapply(1:length(sel_df$`Emissions|CO2|AFOLU`),function(x)fitDistr(sel_df$`Emissions|CO2|AFOLU`,distsel = 8,nbin = x)) # Fitting distribution to AFOLU CO2

# all_sims_CH4_N2O <- t(sapply(1:length(data_nonCO2$Total_CH4_N2O),function(x)c(dist_CH4_N2O[[x]]$stat$BIC,dist_CH4_N2O[[x]]$par$Normal))) %>% 
#   data.frame() %>% 
#   filter_all(all_vars(!is.infinite(.))) %>% 
#   dplyr::filter(a>0) %>% 
#   rename(BIC=V1) %>% 
#   slice(which.min(.[[1]])) # Identifying lowest BIC value

all_sims_Total <- 
  lapply(seq_along(Compliance),function(x)
    sapply(1:Scen_num[x],function(y) c(dist_Total[[x]][[y]]$stat$BIC,dist_Total[[x]][[y]]$par$Normal)) %>% 
             t() %>% 
  data.frame() %>% 
  filter_all(all_vars(!is.infinite(.))) %>% 
  #dplyr::filter(a>0) %>% 
  rename(BIC=V1) %>% 
  slice(which.min(.[[1]])) %>% 
  add_column(Compliant = Compliance[x],.before=1)) # Identifying lowest BIC value

# Chosen compliant scenario distribution
Mean_compliant <- all_sims_Total[[2]]$mean
SD_compliant <- all_sims_Total[[2]]$sd
  
All_limits[All_limits$Indicator=="DirNonCO2LUC",2:4] <- c(Mean_compliant-2*SD_compliant,Mean_compliant,Mean_compliant+2*SD_compliant) # Saving in dataframe

#### 4. Total Agricultural Area - see SM Table S2 for explanation of how the range for cropland + pasture is derived

Area_limits <- read_excel(path = "Input_data/Planetary_boundaries/TotalAgArea_limits.xlsx",
           sheet = "Sheet1",
           range = "A1:D5") %>% 
  filter(Domain=="Agriculture") %>% 
  dplyr::select(-Domain)

All_limits[All_limits$Indicator=="TotalAgArea",2:4] <- rev(Area_limits) # Reversing since the min-mode-max was expressed for forest

#### 5. Freshwater use agriculture boundaries

NIWC <- read_excel(path = "Input_data/Planetary_boundaries/Water_consumption_2050_scenarios.xlsx",
                   sheet = "Sheet1",
                   range = "A1:E40") %>% 
  mutate(Share=Agriculture/Total) %>% # Calculating percentage share of agriculture
  group_by(Study) %>% # Grouping by study 
  summarise(Mean_share = mean(Share)) # Averaging by study to avoid over-emphasising the results of any single study

Ag_PB_shares <- c(NIWC$Mean_share %>% min(na.rm=T),
                  NIWC$Mean_share %>% mean(na.rm=T),
                  NIWC$Mean_share %>% max(na.rm=T))

Total_Freshwater_PB <- c(1100,2800,4500) # Source: Gerten et al. 2013: https://www.sciencedirect.com/science/article/abs/pii/S1877343513001498?via%3Dihub

All_limits[All_limits$Indicator=="Water",2:4] <- Ag_PB_shares*Total_Freshwater_PB

#### 5. Biogeochemical Flows boundaries

## Nitrogen fertiliser and surplus boundary - see SM Table S2 for explanation of how the ranges were derived and the sources used in each case
 
All_limits[All_limits$Indicator=="Nfert",2:4] <- c(52,69,130) # Source: Springmann et al. (2018)
All_limits[All_limits$Indicator=="Nsurplus",2:4] <- c(50,90,146) # Source: Springmann et al. (2018)

## Phosphorus fertiliser and surplus boundary - see SM Table S2 for explanation of how the ranges were derived and the sources used in each case

All_limits[All_limits$Indicator=="Pfert",2:4] <- c(6.2,16,17) # Source: Springmann et al. (2018)

## Phosphorus runoff and stream concentration boundaries - requires adjustment for other non-agricultural P flows  

Runoff_vars <- c("rPdelivery","rPdelivery_agri") # Variables associated with total P and agriculture P delivery to freshwater
#Runoff_vars <- c("rPload_river","rPrunoff_agri")
  
# Reading data from Beusen et al. (2022) GEC paper
# Calculating the percentage of P load in river that originates from agriculture
P_delivery <- read.csv("Input_data/Planetary_boundaries/Beusen_et_al_2022/1-s2.0-S0959378021002053-mmc25.csv",sep=";") %>% # source: https://ars.els-cdn.com/content/image/1-s2.0-S0959378021002053-mmc25.csv
  filter(Region=="WRLD") %>% 
  filter(Variable %in% Runoff_vars) %>% 
  dplyr::select(Scenario,Region,Variable,(paste0("X",time_snapshot))) %>% 
  rename(Value=(paste0("X",time_snapshot))) %>% 
  group_by(Scenario,Variable) %>% 
  summarise(Global_P = sum(Value)) %>% 
  pivot_wider(names_from = Variable, values_from = Global_P) %>% # Pivoting columns to wider format to allow easier calculations
  mutate(perc_agri = !! rlang::sym(Runoff_vars[2])/!! rlang::sym(Runoff_vars[1]))

# Alternative way would be to use P export to oceans - more relevant to P boundary in Steffen et al. (2015) but less comparable with other papers
# P_export <- read.csv("Input_data/1-s2.0-S0959378021002053-mmc23.csv",sep=";") %>% # source: https://ars.els-cdn.com/content/image/1-s2.0-S0959378021002053-mmc25.csv
#   #filter(Region=="WRLD") %>% 
#   filter(Variable %in% Variables) %>% 
#   select(Scenario,Region,Variable,(paste0("X",time_snapshot))) %>% 
#   rename(Value=(paste0("X",time_snapshot))) %>% 
#   group_by(Scenario,Variable) %>% 
#   summarise(Global_P = sum(Value)) %>% 
#   pivot_wider(names_from = Variable, values_from = Global_P) %>% # Pivoting columns to wider format to allow easier calculations
#   mutate(perc_agri = sro_agri/pload)
  
P_flow_shares <- read_excel(path = "Input_data/Planetary_boundaries/P_flow_2050_scenarios.xlsx",
                           sheet = "Sheet1",
                           range = "A1:C11") %>% 
  mutate(Share=Agriculture/Total) %>% # Calculating percentage share of agriculture
  group_by(Study) %>% # Grouping by study 
  summarise(Mean_share = mean(Share)) %>% 
  add_row(data.frame(Study="Beusen et al. (2022)",Mean_share=mean(P_delivery$perc_agri)))

# If using all studies

# P_PB_shares <- c(P_flow_shares$Mean_share %>% min(na.rm=T),
#                   P_flow_shares$Mean_share %>% mean(na.rm=T),
#                   P_flow_shares$Mean_share %>% max(na.rm=T))

# If using only Beusen et al. (2022)
P_PB_shares <- c(min(P_delivery$perc_agri),
                 mean(P_delivery$perc_agri),
                 max(P_delivery$perc_agri))


Total_Psurplus_PB <- c(11,11,100) # Source: Steffen et al. (2015) - Table 1: https://www.science.org/doi/10.1126/science.1259855

All_limits[All_limits$Indicator=="Pexport",2:4] <- P_PB_shares*Total_Psurplus_PB

# Alternative P boundary (in-stream P):
# Determining runoff and total load compared to critical P concentration (50-100 mg P m-3 for [P]crit, Source: Springmann et al. (2018) SM, p. 7)
P_crit <- c(0.05,0.075,0.1)/1000 # Range of critical P concentrations in freshwaters converted to mg/L

Discharge_vars <- c(#"pload",
  #"sro_nat",
  #"sro_agri",
  "rDischarge_mouth"
  #"pointsources",
  #"aquaculture",
  #"veg_flooding"
)

P_fresh <- read.csv("Input_data/Planetary_boundaries/Beusen_et_al_2022/1-s2.0-S0959378021002053-mmc25.csv",sep=";") %>% # source: https://ars.els-cdn.com/content/image/1-s2.0-S0959378021002053-mmc25.csv
  filter(Region=="WRLD") %>% 
  filter(Variable %in% Discharge_vars) %>% 
  dplyr::select(Scenario,Region,Variable,(paste0("X",time_snapshot))) %>% 
  rename(Discharge=(paste0("X",time_snapshot))) %>% 
  mutate(Pfresh_lwr = Discharge*P_crit[1],
         Pfresh_avg = Discharge*P_crit[2],
         Pfresh_upr = Discharge*P_crit[3])


All_limits[All_limits$Indicator=="Pinstream",2:4] <- c(P_fresh$Pfresh_lwr %>% min(),
                                                       P_fresh$Pfresh_avg %>% mode_triang(),
                                                       P_fresh$Pfresh_upr %>% max())

#### 6. Write results in csv in outputs folder (for subsequent use in risk calculations)

write.csv(All_limits,"Outputs/Environmental_limits/All_PB_limits.csv")

