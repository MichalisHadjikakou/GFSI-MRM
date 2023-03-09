[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7710503.svg)](https://doi.org/10.5281/zenodo.7710503)
![GitHub](https://img.shields.io/github/license/MichalisHadjikakou/GFSI-MRM)

# Global Food System Intervention Meta-Regression <br> Model (GFSI-MRM)

## Abstract

Transforming the global food system is necessary to avoid exceeding planetary boundaries. A robust evidence base is crucial to assess the scale and combination of interventions required for a sustainable transformation. We developed a risk assessment framework, underpinned by a meta-regression of 60 global food system modeling studies, to quantify the potential of individual and combined interventions to mitigate the risk of exceeding the boundaries for land-system change, freshwater use, climate change, and biogeochemical flows by 2050. Limiting the risk of exceedance across four key planetary boundaries requires a high but plausible level of ambition in all demand-side (diet, population, waste) and most supply-side interventions. Attaining the required level of ambition for all interventions relies on embracing synergistic actions across the food system. 

## Description

This repository provides the underlying code to reproduce the analysis for the manuscript titled **_Mitigating risk of exceeding environmental limits requires ambitious food system interventions_**, currently under review. 

## Getting Started

### Dependencies

The following R packages are used in this repository:

* pacman (package installation and updating)
* tidyverse, cowplot, viridis, RColorBrewer (data wrangling and visualisation)
* car, lme4, merTools, propagate, extraDistr, fitdistrplus, performance, LMERConvenienceFunctions, cvms, ggeffects, DHARMa, sjPlot, lmerTest, predictmeans (statistical testing, analysis, and cross-validation)
* readxl, writexl, data.table (read and write files)
* future.apply (parallel computing)
* wppExplorer, wpp2019 (population projections)

For each script, required packages are installed through the p_load command from the 'pacman' package.

### Description of main scripts

* **_0.0_PB_distributions_2050_AR6.R_**

This script draws on published literature and databases to produce 2050 distributions for agriculture-specific environmental limits related to the [planetary boundaries](https://www.science.org/doi/10.1126/science.1259855) of land-system change, climate change, freshwater use and biogeochemical flows.

* **_1.0a_Fit_all_LMMs_except_LUC_emissions.R_** 
* **_1.0b_Fit_LUC_emissions_model_AR6_**

These scripts fit linear mixed-effects meta-regression models and perform cross-validation for each of the 10 environmental indicators (cropland, pasture, methane, nitrous oxide, carbon dioxide associated with land-use change, blue water consumption, nitrogen fertiliser, nitrogen surplus, phosphorus fertiliser, phosphorus surplus)  used in the analysis.  

* **_2.0_Predict_scenario.R_**

This script uses the fitted models from 1.0a and 1.0b to generate 2050 predictions and prediction intervals for a broad range of future intervention levels and all their combinations. 

* **_3.0_Risk_calculation_across_PBs.R_**

This script uses the predictions from 2.0 and the distributions in 0.0 to estimate exceedance risk for each intervention level combination across all planetary boundaries. 

* **_4.0a_Risk_averages_Figure_2_dataset.R_**
* **_5.0a_Figure_2_Composite_barplots_ggplot_version.R_**

These scripts generate the composite barplot panel (see Figure 2 in the [manuscript]) of mean and SD exceedance risk across different intervention levels. 

* **_4.0b_Indicator_averages_physical_units_SM.R_**
* **_5.0b_Indicator_composite_barplots_SM.R_**

These scripts generate the composite barplot panel (see Figure 2 in the [manuscript]) of mean and SD expressed in physical units, with the option of absolute or relative units. 

* **_6.0a_Tradeoff_analysis.ipynb_**

This Jupyter notebook merges all risk results for each planetary boundary into one integrated dataset based on matching intervention levels across all the common interventions (available across all the indicator statistical models): population, diet change (animal and plant calories), waste reduction, crop yields, feed efficiency (FCR), and feed composition. It then performs pareto (trade-off) analysis betweenn risk mitigation and intervention ambition levels.   

* **_6.0b_Figure_3_Risk_compliant_combinations.R_**

This script uses the integrated dataset produced by 6.0a to filter the scenarios that meet two critical [IPCC-calibrated uncertainty risk thresholds](https://www.ipcc.ch/site/assets/uploads/2018/05/uncertainty-guidance-note.pdf) across all boundaries: < 0.50 risk (exceedance about as unlikely as not) and < 0.33 risk (exceedance unlikely) and categorizes them in terms of the type and level of each intervention required to achieve each threshold (see Figure 3 in the [manuscript]).

## License

This project is licensed under the [GPL-3.0] License - see the LICENSE.md file for details.

## Acknowledgments

We would like to acknowledge the authors of all the excellent packages (see [Dependencies](#dependencies)) that have enabled this work.
