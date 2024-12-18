######### Base year (2010) harmonisation across all indicators #############

# Author: Michalis Hadjikakou, Deakin University (m.hadjikakou@deakin.edu.au)
# Purpose: Comparing the distributions of selected studies to ensure comparable scopes for starting years 

## 1. Loading packages and parameters

if (!require("pacman")) install.packages("pacman")
pacman::p_load(car,fitdistrplus,mc2d,readxl,propagate,tidyverse,future.apply,here,hablar,viridis)

`%notin%` <- Negate(`%in%`)
base_year <- '2010' # set 2010 as the base year

# Indicator names and units
ind <- c("Cropland","Pasture","Water","CH4","N2O", # Forest and LUC are derived post-hoc (on the basis of pasture and cropland)
         "Nfert","Pfert") # Using 'Nfert' instead of "Nfix' as per Springmann et al. (2018)

Units <- c("Mha","Mha","km3/yr","GtCO2-eq","GtCO2-eq","GtCO2-eq","Tg N/yr","Tg P/yr")

# IPCC AR6 database filtering variables
included_models <- read.csv(here("Input_data/Planetary_boundaries/Selected_AR6_LUMs.csv"))# These IPCC models have a dedicated land use/agriculture sub-model and comparable base year values

only_1.5 <- c("C1: limit warming to 1.5°C (>50%) with no or limited overshoot")#,"C2: return warming to 1.5?C (>50%) after a high overshoot")
only_2 <- c("C3: limit warming to 2°C (>67%)")
sel_scens <- c(only_1.5,only_2) # Selecting scenarios - modify accordingly to select only 1.5 or only 2 degree pathways

cat_emissions <- c("Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")
cat_emissions_detailed <- c("Emissions|CH4|AFOLU|Agriculture|Livestock|Manure Management",
                            "Emissions|CH4|AFOLU|Agriculture|Livestock|Enteric Fermentation",
                            "Emissions|CH4|AFOLU|Agriculture|Rice",
                            "Emissions|N2O|AFOLU|Agriculture|Livestock|Manure Management",
                            "Emissions|N2O|AFOLU|Agriculture|Managed Soils")
non_CO2_cat <- c("Emissions|N2O|AFOLU","Emissions|CH4|AFOLU")
land_use <- c("Land Cover|Cropland","Land Cover|Pasture")

#### For Water, Nfert, Pfert etc. we used additional datasets

#Water: https://www.fao.org/aquastat/en/overview/methodology/water-use
# N & P: https://www.fao.org/faostat/en/#data/RFN

Base_year_data <- readxl::read_xlsx(here("Input_data/Base_year_values.xlsx"))

## 2. Loading all datasets

# 2.1 Loading and cleaning input database
data_raw <- readxl::read_excel(path = here("Input_data/Data_S1_Input_database_20240410.xlsx"),sheet = "T1 - Main scenario database") # Reading in values as dataframe
source(here("Functions/Pre_processing_disaggregated_feed.R")) # Carries out pre-processing of the raw data extracted

input_db <- DF_preprocess(data_raw,PBs) %>% 
  data.frame() %>% 
  filter(Included=="Yes",Scenario..author.code.=="Base year",
         ScenYear %in% 2007:2012, # Selecting only base year values
         Study %notin% c("Searchinger et al. (2018)","Chang et al. (2021)",
                         "Clark et al. (2020)","FOLU (2019)")) # GHG emissions not compatible but other indicators are
  
# 2.2 Loading AR6 database (URL: https://data.ece.iiasa.ac.at/ar6/#/downloads)
IAMC_data <- read.csv(unz(here("Input_data/Planetary_boundaries/1668008312256-AR6_Scenarios_Database_World_v1.1.csv.zip"), 
                          "AR6_Scenarios_Database_World_v1.1.csv"), header = TRUE,
                      sep = ",") 
# Extracting metadata file
unzip(zipfile=here("Input_data/Planetary_boundaries/1668008312256-AR6_Scenarios_Database_World_v1.1.csv.zip"), files = "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx", 
      exdir=here("Input_data/Planetary_boundaries/."))
# Selecting the key columns
metadata_IAMC_sel <- read_excel(path = here("Input_data/Planetary_boundaries/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"),sheet = "meta_Ch3vetted_withclimate") %>%
  filter(Category_name %in% sel_scens) %>% # Selecting only 1.5 or only 2C scenarios
  dplyr::select(Scenario,Model)# %>% unique()

year_prefix <- "X"

## 3. Reformatting data and merging datasets

N2O_exclusions <- c("Clark et al. (2020)","Muller et al. (2017)","FAO (2018)")

# This study (Hadjikakou et al.)
Study_db <- input_db %>% 
  select(any_of(c("Study","Model",ind))) %>%   # Selecting only key columns
  add_column(Database = "This study",.before=1)

# AR6 base year values and distributions
data_AFOLU_base_year <- IAMC_data %>% # Reading all IPCC scenarios 
  filter(Scenario %in% metadata_IAMC_sel$Scenario) %>%
  filter(Model %in% metadata_IAMC_sel$Model) %>%
  filter(Variable %in% c(cat_emissions,cat_emissions_detailed,land_use)) %>% 
  dplyr::select(Model,Scenario,Variable,all_of(paste0(year_prefix,base_year))) %>% 
  pivot_wider(names_from = Variable,values_from = paste0(year_prefix,base_year)) %>% 
  rename_with(~ c("CO2","CH4","N2O",
                  "CH4_manure_management","CH4_enteric_fermentation","CH4_rice",
                  "N2O_manure_management","N2O_managed_soils",
                  "Cropland","Pasture"), all_of(c(cat_emissions,cat_emissions_detailed,land_use))) %>% 
  select(any_of(c("Model",ind))) %>% 
  add_column(Database = "IPCC AR6 v1.1",.before=1) %>% 
  filter(Model %in% (included_models %>% filter(Included=="Y") %>% 
                       pull(Model))) 
           
data_combined <- 
  Study_db %>% add_row(data_AFOLU_base_year) %>% 
  pivot_longer(cols = any_of(ind),names_to = "Indicator",values_to = "Value") %>% 
  mutate(Value = case_when(
    (Indicator=="N2O" & Database=="This study") ~ Value*1000,
    TRUE ~ Value)) %>% 
  mutate(Value = case_when(
    (Indicator=="N2O" & Study %in% N2O_exclusions) ~ NA,
    TRUE ~ Value)) %>% 
  mutate(Value = case_when(
    (Indicator=="Cropland" & Model %in% c("SOLm","EAT-Lancet","Clark et al. (2020)")) ~ NA,
    (Indicator=="Cropland" & Study %in% c("Weindl et al. (2017a)")) ~ NA,
    (Indicator=="Nfert" & Study %in% c("Davis et al. (2016)","Mogollon et al. (2018a)")) ~ NA,
    TRUE ~ Value))
  
# The following produces warnings but please ignore these (Nfert, Pfert, Water values are not available in the AR6 database)
Final_table <- data_combined %>% 
  group_by(Indicator,Database) %>% 
  summarise(n = n_distinct(Value),
            Mean = mean(Value,na.rm=T),
            Min = min(Value,na.rm=T),
            Q_25 = quantile(Value,c(0.25),na.rm=T),
            Q_75 = quantile(Value,c(0.75),na.rm=T),
            Max = max(Value,na.rm=T)) %>% 
  arrange(match(Indicator,ind),
          desc(Database)) %>% 
  mutate(Mean = case_when(
    (Indicator=="Water" & Database=="IPCC AR6 v1.1") ~ Base_year_data %>% filter(Indicator=="Water") %>% pull(Value),
    (Indicator %in% c("Nfert") & Database=="IPCC AR6 v1.1") ~ c(Base_year_data %>% filter(Indicator=="Nfert") %>% pull(Value)),
    (Indicator %in% c("Pfert") & Database=="IPCC AR6 v1.1") ~ c(Base_year_data %>% filter(Indicator=="Pfert") %>% pull(Value)),
    TRUE ~ Mean)) %>% 
  mutate(Database = case_when(
    (Indicator=="Water" & Database=="IPCC AR6 v1.1") ~ Base_year_data %>% filter(Indicator=="Water") %>% pull(Source),
    (Indicator %in% c("Nfert","Pfert") & Database=="IPCC AR6 v1.1") ~ Base_year_data %>% filter(Indicator=="Nfert") %>% pull(Source),
    TRUE ~ Database))
  
write.csv(Final_table,"Outputs/Base_year_harmonisation/Base_year_comparison_no_CO2_LUC.csv")
