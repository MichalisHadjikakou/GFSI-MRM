[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14523155.svg)](https://doi.org/10.5281/zenodo.14523155)
![GitHub](https://img.shields.io/github/license/MichalisHadjikakou/GFSI-MRM)

# Global Food System Intervention Meta-Regression <br> Model (GFSI-MRM)

## Abstract

Transforming the global food system is essential to avoid exceeding Earth’s environmental limits. A robust evidence base is crucial to assess the scale and combination of interventions required for a sustainable transformation. We developed a risk assessment framework, underpinned by an evidence synthesis of global food system modeling studies, to quantify the potential of individual and combined interventions to mitigate the risk of exceeding global environmental limits for agricultural area, greenhouse gas (GHG) emissions, surface water flows, and nutrient cycles by 2050. GHG emissions and nutrient cycles are the most difficult limits to avoid exceeding and are conditional on shifts toward diets with a low proportion of animal-source foods; steep reductions in emissions intensity; substantial improvements in nutrient management, feed-conversion ratios, and crop yields; and efforts to limit overconsumption and food waste. Ambitious actions across the global food system are needed to ensure the required level of risk mitigation.

## Description

This repository provides the underlying code to reproduce the analysis published in **Hadjikakou et al. (2025) Ambitious food system interventions required to mitigate the risk of exceeding Earth’s environmental limits**, _One Earth_, DOI: https://doi.org/10.1016/j.oneear.2025.101351.

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

* **_0.0a_Base_year_harmonisation_check.R_**
* **_0.0b_Environmental_limits_2050.R_**

The first script ensures that selected studies have consistent base year values. The second script draws on the latest scientific consensus on global [planetary boundaries](https://www.science.org/doi/10.1126/sciadv.adh2458) and [Earth system boundaries](https://www.nature.com/articles/s41586-023-06083-8) to produce 2050 distributions for agriculture-specific environmental limits related to agricultural area, GHG emissions, surface water flows, and nutrient cycles.  

* **_1.0a_Fit_all_LMMs_except_LUC_emissions.R_** 
* **_1.0b_Fit_LUC_emissions_model_AR6.R_**

These scripts fit linear mixed-effects meta-regression models and perform cross-validation for each of the 8 environmental indicators (cropland, pasture, methane, nitrous oxide, carbon dioxide associated with land-use change, blue water withdrawals, nitrogen fertiliser, phosphorus fertiliser) used in the analysis.  

* **_2.0_Predict_scenario.R_**

This script uses the fitted models from 1.0a and 1.0b to generate 2050 predictions and 95% prediction intervals for a broad range of future intervention levels and all their combinations. 

* **_3.0_Risk_calculation_across_PBs.R_**

This script uses the predictions from 2.0 and the distributions in 0.0 to estimate exceedance risk for each intervention level combination across all environmental limits. 

* **_4.0a_Risk_averages_Figure_2_dataset_all_indicators.R_**
* **_5.0a_5.0a_Figure_3_Composite_barplots_ggplot_version_physical_units.R_**

These scripts generate the underlying dataset and composite barplot panel (see Figure 3 in the [manuscript]) which shows the mean and 2SD of risk mitigation potential at different intervention levels across each environmental limit. 

* **_4.0b_Risk_averages_SM_dataset_additional_indicators.R_**
* **_5.0b_SM_Composite_barplots_ggplot_version_physical_units.R_**

These scripts generate the composite barplot panel (see Figure S4 in the [manuscript]) which shows the mean and SD of risk mitigation potential at different intervention levels for sub-indicators (cropland, pasture, methane, nitrous oxide, carbon dioxide from land use change). 

* **_6.0a_Tradeoff_analysis.ipynb_**

This Jupyter notebook merges all risk results for each planetary boundary into one integrated dataset based on matching intervention levels across all the common interventions (available across all the indicator statistical models): population, diet change (animal and plant calories), waste reduction, crop yields, feed efficiency (FCR), and feed composition. It then performs pareto (trade-off) analysis betweenn risk mitigation and intervention ambition levels (Figure S5).   

* **_6.0b_Figure_3_Risk_compliant_combinations.R_**
* **_6.0c_SM_all_risk_compliant_combinations.R_**

These script use the integrated dataset produced by 6.0a to filter the scenarios that meet two critical [IPCC-calibrated uncertainty risk thresholds](https://www.ipcc.ch/site/assets/uploads/2018/05/uncertainty-guidance-note.pdf) across all boundaries: < 0.50 risk (exceedance about as unlikely as not) and < 0.33 risk (exceedance unlikely) and categorizes them in terms of the type and level of each intervention required to achieve each threshold (see Figure 4 and Figures S6-S9 in the [manuscript]).

## License

This project is licensed under the [GPL-3.0] License - see the LICENSE.md file for details.

## Acknowledgments

We would like to acknowledge the authors of all the excellent packages (see [Dependencies](#dependencies)) that have enabled this work.
