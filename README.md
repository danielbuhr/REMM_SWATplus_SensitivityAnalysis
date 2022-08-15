# REMM_SWATplus_SensitivityAnalysis

This repository contains code and data needed to reproduce the analysis in Buhr et al. (Global sensitivity analyses of key riparian nitrogen models - submitted to Environmental Modelling and Software). Please see this manuscript and supplemental material for details. Specific folders in this repository are described below.

# REMM

This folder contains code and data for setup and post-processing of REMM simulations. "VariableRanges.csv" specifies the statistical distribution used for each input parameter that we adjusted. "Value_Generation.R" creates the input parameters based on distributions and correlations. "Write_Inputs.R" generates input files from these values and creates the batch file to run the simulations from the command line. "Analysis.R" performs the different sensitivity analyses. "Plots.R" recreates plots seen in the manuscript and supplementary material. REMM inputs and application may be available upon request from https://www.ars.usda.gov/southeast-area/tifton-ga/southeast-watershed-research/research/models/remm-model/.

# SWATplus

This folder contains code and data for setup, simulation, and post-processing of SWATplus simulations. "Parameters_2.csv" includes the names and shorthand for the studied input parameters. "Simulation_Correlation.R" sets up the model inputs and runs the simulation. "Analysis.R" performs the different sensitivity analyses. "Plots.R" recreates plots seen in the manuscript and supplementary material. The "Results" subfolder contains four files: "inputs_3.csv", "denit_sum_3", "wtd_sd_3", and "no3_sum_3". These are outputs of "Simulation_Correlation.R" and can be used to run "Analysis.R" and "Plots.R". SWAT+ input files are too large to provide here but may be available from https://swat.tamu.edu/software/plus/.

# Sensitivity Analysis

This folder contains functions to perform the sensitivity analysis using the Plischke et al. (2013) methods. All code was written by Rod Lammers. CVD sensitivity analysis code was acquired from Baroni and Francke (2020), available at https://github.com/baronig/GSA-cvd.
