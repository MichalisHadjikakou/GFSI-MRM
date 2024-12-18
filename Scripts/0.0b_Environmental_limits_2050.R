######### Deriving distributions for environmental limits - for agricultural GHG emissions, total agricultural area, water,nitrogen and Phosphorus #############

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Define agriculture environmental limits for all indicators

### 1. Loading packages and functions and specifying parameters

if (!require("pacman")) install.packages("pacman")
pacman::p_load(car,fitdistrplus,mc2d,readxl,propagate,tidyverse,future.apply,here)

plan(multisession, workers = 50) # Requesting 50 cores for parallel processing

`%notin%` <- Negate(`%in%`)
FD <- function(x) { # Freedman-Diaconis rule used to select the size of the bins to be used in a histogram
  diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
}

mode_triang <- function(x){ # Method for estimating the mode of a triangular distribution based on mean: http://kb.palisade.com/index.php?pg=kb.page&id=44
  3*mean(x)-min(x)-max(x)
}

# 1.1 Specifying variables and working directories

n <- 10000 
IAMC_database <- 'AR6' # OR change to 'AR6'
GWP_CH4 <- 27.2 #AR6, 27.2 (non-fossil origin)
GWP_N2O <- 273/1000 # AR6, 273 (updated N2O factor)
base_year <- '2010' # set 2010 as the base year
time_snapshot <- '2050'
Paris_compliance <- 'no'
scope <- "Total_AFOLU" 

# 1.2 Initialising empty dataframe with limits across all relevant indicators

All_limits <- data.frame(Indicator = c("TotalAgArea", # where TotalAgArea = Cropland + Pasture
                                       "Water", 
                                       "DirNonCO2LUC", # where DirNonCO2LUC = CH4 + N2O + CO2 (LUC)
                                       "Nfert","Pfert")) %>% 
  add_column(Min=NA,Mode=NA,Max=NA)

# Working directory is specified using the 'here' function throughout - see .here file in parent directory 

# 2.1 Reading in key input files

included_models <- read.csv(here("Input_data/Planetary_boundaries/Selected_AR6_LUMs.csv"))# These IPCC models have a dedicated land use/agriculture sub-model and comparable base year values
  
# Scenario family strings - allows selecting and fitting distributions for 1.5 and 2C seperately or together
if(IAMC_database=='iamc15') {

  only_1.5 <- c("1.5C low overshoot","Below 1.5C","1.5C high overshoot")
  only_2 <- c("Higher 2C","Lower 2C")
  all <- c(only_1.5,only_2)
  
  sel_scens <- all # Selecting 1.5oC and 2oC scenarios - modify accordingly
  
  cat_emissions <- c("Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")
  cat_emissions_detailed <- c("Emissions|CH4|AFOLU|Agriculture|Livestock|Manure Management",
                              "Emissions|CH4|AFOLU|Agriculture|Livestock|Enteric Fermentation",
                              "Emissions|CH4|AFOLU|Agriculture|Rice",
                              "Emissions|N2O|AFOLU|Agriculture|Livestock|Manure Management",
                              "Emissions|N2O|AFOLU|Agriculture|Managed Soils")
  non_CO2_cat <- c("Emissions|N2O|AFOLU","Emissions|CH4|AFOLU")
  land_use <- c("Land Cover|Cropland","Land Cover|Pasture")
  
}else{

  only_1.5 <- c("C1: limit warming to 1.5°C (>50%) with no or limited overshoot")#,"C2: return warming to 1.5?C (>50%) after a high overshoot")
  only_2 <- c("C3: limit warming to 2°C (>67%)")
  all <- c(only_1.5,only_2)
  
  sel_scens <- all # Selecting only 1.5oC scenarios - modify accordingly
  cat_emissions <- c("Emissions|CO2|AFOLU","Emissions|CH4|AFOLU|Agriculture","Emissions|N2O|AFOLU|Agriculture")
  non_CO2_cat <- c("Emissions|CH4|AFOLU|Agriculture","Emissions|N2O|AFOLU|Agriculture")
  land_use <- c("Land Cover|Cropland","Land Cover|Pasture")
}

#### 2. Loading IAMC data - either IAMC15 or AR6 database depending on choice at the beginning of script

if(IAMC_database=='iamc15') {
    
  # Reading the main data file
  IAMC_data <- read_excel(path = here("Input_data/Planetary_boundaries/iamc15_scenario_data_world_r2.0.xlsx"),sheet = "data") #Reading all IPCC scenarios
  # Extracting the metadata file
  metadata_IAMC_sel <- read_excel(path = here("Input_data/Planetary_boundaries/sr15_metadata_indicators_r2.0.xlsx"),sheet = "meta") %>%
    filter(category %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
    pull(scenario)
  
  year_prefix <- ""
  
  Paris_compliant <- read_excel(here("Input_data/Planetary_boundaries/Schleussner_et_al_2022/sr15_updated_meta.xlsx")) %>% #Source: https://www.nature.com/articles/s43247-022-00467-w#code-availability
    filter(category_new=="PA (Art 4)")

}else{
  # Reading the main data file
  IAMC_data <- read.csv(unz(here("Input_data/Planetary_boundaries/1668008312256-AR6_Scenarios_Database_World_v1.1.csv.zip"), 
                       "AR6_Scenarios_Database_World_v1.1.csv"), header = TRUE,
                   sep = ",") %>% 
    filter(Model %in% (included_models %>% filter(Included=="Y") %>% 
                         pull(Model))) 
  # Extracting metadata file
  unzip(zipfile=here("Input_data/Planetary_boundaries/1668008312256-AR6_Scenarios_Database_World_v1.1.csv.zip"), files = "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx", 
        exdir=here("Input_data/Planetary_boundaries/."))
  # Selecting the key columns
  metadata_IAMC_sel <- read_excel(path = here("Input_data/Planetary_boundaries/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"),sheet = "meta_Ch3vetted_withclimate") %>%
    filter(Category_name %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
    dplyr::select(Scenario,Model)# %>% unique()
  # Selecting only Paris-compliant models (can decide to use those instead)
  Paris_compliant <- read_excel(here("Input_data/Planetary_boundaries/Schleussner_et_al_2022/AR6_Scenarios_Updated_Meta_MHExtract_290622.xlsx")) %>% # Source: https://www.nature.com/articles/s43247-022-00467-w#code-availability
    filter(category_new_rolling_mean=="PA (Art 2 and 4)") %>% 
    rename(Scenario=scenario, Model=model) %>% 
    dplyr::select(Model,Scenario) 
  
  year_prefix <- "X"
}

# If Paris compliance, replace metadata IAMC_sel
if (Paris_compliance=='yes'){
  metadata_IAMC_sel <- Paris_compliant}

# Base year CO2LUC stats - to be added to base year comparison csv
data_AFOLU_base_year <- IAMC_data %>% # Reading all IPCC scenarios
  filter(Scenario %in% metadata_IAMC_sel$Scenario) %>%
  filter(Model %in% metadata_IAMC_sel$Model) %>%
  filter(Variable %in% cat_emissions) %>% 
  dplyr::select(Model,Scenario,Variable,Unit,all_of(paste0(year_prefix,"2010"))) %>% 
  add_column(Compliant = "Yes",.before=1)

CO2_LUC_base_year <- data_AFOLU_base_year %>% 
  filter(Variable=="Emissions|CO2|AFOLU") %>% 
  rename(Value=X2010) %>% 
  #group_by(Model) %>% 
  summarise(n = n_distinct(Value),
            Mean = mean(Value,na.rm=T),
            Min = min(Value,na.rm=T),
            Q_25 = quantile(Value,c(0.25),na.rm=T),
            Q_75 = quantile(Value,c(0.75),na.rm=T),
            Max = max(Value,na.rm=T)) %>% 
  slice(rep(row_number(),2)) %>% 
  add_column(Indicator = "CO2 LUC",
             Database = c("This study","IPCC AR6 v1.1"),.before=1)

Base_year_range <- read.csv("Outputs/Base_year_harmonisation/Base_year_comparison_no_CO2_LUC.csv") %>% 
  select(-c(X))

Revised_base_year_range <- Base_year_range %>% 
  add_row(CO2_LUC_base_year,.before = which(Base_year_range$Indicator=="Nfert")[1])

write.csv(Revised_base_year_range,"Outputs/Base_year_harmonisation/Base_year_comparison_with_CO2_LUC.csv")

# Compliant scenarios 
data_AFOLU <- IAMC_data %>% # Reading all IPCC scenarios
  filter(Scenario %in% metadata_IAMC_sel$Scenario) %>%
  filter(Model %in% metadata_IAMC_sel$Model) %>%
  filter(Variable %in% cat_emissions) %>% 
  dplyr::select(Model,Scenario,Variable,Unit,all_of(paste0(year_prefix,time_snapshot))) %>% 
  add_column(Compliant = "Yes",.before=1)

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

All_limits[All_limits$Indicator=="DirNonCO2LUC",2:4] <- c(data_nonCO2LUC$Total_AFOLU %>% min(),
                                                          data_nonCO2LUC$Total_AFOLU %>% mean(), # This is considered the best estimate as opposed to the mode in this case
                                                          data_nonCO2LUC$Total_AFOLU %>% max()) # Saving in dataframe

#### 4. Total Agricultural Area - see SM Table S2 for explanation of how the range for cropland + pasture is derived

Area_limits <- read_excel(path = "Input_data/Planetary_boundaries/Non_GHG_limits.xlsx",
           sheet = "AgArea",
           range = "A1:D5") %>% 
  filter(Domain=="Agriculture") %>% 
  dplyr::select(-Domain)

All_limits[All_limits$Indicator=="TotalAgArea",2:4] <- rev(Area_limits) # Reversing since the min-mode-max was expressed for forest

#### 5. Freshwater use agriculture boundaries

Ag_withdrawals <- read_excel(path = "Input_data/Planetary_boundaries/Non_GHG_limits.xlsx",
                   sheet = "Water",
                   range = "A1:E4") %>% 
  pull(`Final limits`)

All_limits[All_limits$Indicator=="Water",2:4] <- Ag_withdrawals

#### 5. Biogeochemical Flow boundary

N_P <- read_excel(path = "Input_data/Planetary_boundaries/Non_GHG_limits.xlsx",
                  sheet = "Biogeochemical flows",
                  range = "A1:D3")

## Nitrogen and phosphorus fertiliser limits - see SM Table S2 for explanation of how the ranges were derived and the sources used in each case
 
All_limits[All_limits$Indicator %in% c("Nfert","Pfert"),] <- N_P 

#### 6. Write results in csv in outputs folder (for subsequent use in risk calculations)

write.csv(All_limits,"Outputs/Environmental_limits/All_PB_limits.csv")

