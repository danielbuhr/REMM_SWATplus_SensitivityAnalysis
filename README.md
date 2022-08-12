# REMM_SWATplus_SensitivityAnalysis

This repository contains code and data needed to reproduce the analysis in Buhr et al. (Global sensitivity analyses of key riparian nitrogen models - submitted to Environmental Modelling and Software). Please see this manuscript and supplemental material for details. Specific folders in this repository are described below.

# REMM

This folder contains code and data for setup and post-processing of REMM simulations. These simulations can be run from the command line using REMM code and executable (not provided here).

# SWATplus

This folder contains code and data for setup, simulation, and post-processing of SWATplus simulations. "Parameters_2.csv" includes the names and shorthand for the studied input parameters. "Simulation_Correlation.R" sets up the model inputs and runs the simulation. "Analysis.R" performs the different sensitivity analyses. "Plots.R" recreates plots seen in the manuscript and supplementary material. SWAT+ input files are too large to provide here.

# Sensitivity Analysis

This folder contains functions to perform the sensitivity analysis using the Plischke et al. (2013) methods. All code was written by Rod Lammers. CVD sensitivity analysis code was acquired from Baroni and Francke (2020), available at https://github.com/baronig/GSA-cvd.
