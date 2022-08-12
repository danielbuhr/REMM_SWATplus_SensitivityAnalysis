#Value_Generation
# Generate matrix of values based on parameters and ranges that have been selected

# Call external libraries needed in this script
library(EnvStats) # For random number generation from truncated distribution
library(dplyr)

path <- "Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files"
setwd(path)

mytime <- format(base::Sys.time(), "%m%d%Y_%H%M") # adds file nomenclature for date (mmmddYYYY_HrMin)

# Make sure you have saved a .csv file for the parameter names and ranges
# Format should be parameter name, distribution type, min, max, mean, std dev
# Read in the .csv file, with below name
ranges = read.csv(paste0(path, "\\VariableRanges.csv"), header = TRUE)

# Create matrix for all parameters and their values for n_sims simulations
n_sims = 1500 # Set high and trim down to 1000 after errors are removed
n_params = nrow(ranges)

# Makes the matrix 
variables <- vector(mode = "character", length = nrow(ranges))
variables <- ranges[,1]
matrix_values = matrix(nrow = n_sims, ncol = n_params)
colnames(matrix_values) <- variables
list(t, length=n_sims)

i <- 1

while (i < n_params +1) {
  if (ranges[i,2] == "Log-normal") {
    matrix_values[,i] = rlnormTrunc(n_sims, ranges[i,5],ranges[i,6],ranges[i,3],ranges[i,4]) }
  else if (ranges[i,2] == "Normal") {
    matrix_values[,i] = rnormTrunc(n_sims, ranges[i,5],ranges[i,6],ranges[i,3],ranges[i,4])  }
  else if (ranges[i,2] == "Uniform") {
    matrix_values[,i] = runif(n_sims, ranges[i,3],ranges[i,4]) }
  
  i = i+1
}       

# Create lists for holding C:N ratio values
t <- vector(length=n_sims) 
u <- vector(length=n_sims)
v <- vector(length=n_sims)
w <- vector(length=n_sims)

j <- 1

# Make sure no parameters break their mathematical relations to other parameters
while (j < n_sims+1) {
  
  # Select values for C:N ratios, with mins for humus pools based on user manual;
  t[j] <- runif(n=1, min=20, max=200) # select a value for the C:N structural from 20-200
  u[j] <- runif(n=1, min=3, max=20) # select a value for the C:N active humus from 3-20
  v[j] <- runif(n=1, min=7, max=20) # select a value for the C:N passive humus from 7-20
  w[j] <- runif(n=1, min=12, max=20) # select a value for the C:N slow humus from 12-20
  
  a <- matrix_values[j,14]
  b <- matrix_values[j,15]
  
  # Check if field capacity equals or exceeds porosity
  # If so, make FC = 7/9 * porosity (Dingman 2002)
  if (matrix_values[j,14] >= matrix_values[j,15]) {
    matrix_values[j,14] <- (7*matrix_values[j,15]/9)
  }
  
  #if (a >= b) {
  #   a <- (7*b/9)
  # }
  
  # Check if wilting point equals or exceeds field capacity
  # If so, make WP = 7/9 * FC (Dingman 2002)
  if (matrix_values[j,13] >= matrix_values[j,14]) {
    matrix_values[j,13] <- (7*matrix_values[j,14]/9)
  }
  
  # Check if VWC exceeds porosity
  # If so, set VWC to 0.001 less than porosity (some errors may have occurred due to their equivalence)
  if (matrix_values[j,16] > matrix_values[j,15]) {
    matrix_values[j,16] <- (matrix_values[j,15]-0.001)
  }
  
  # Check if layer thickness is greater than 1/3 of depth to bedrock (3 layers total) 
  # CHECK UNITS -- layer thickness is cm, depth to bedrock is m
  # If so, set thickness = 1/3 * BD rock
  if (matrix_values[j,12] > (matrix_values[j,10]*33.3)) {
    matrix_values[j,12] <- (matrix_values[j,10]*33.3)
  }
  
  # Check if rooting depth equals or exceeds maximum rooting depth
  # If so, set rooting dept to 0.9*max rooting depth
  if (matrix_values[j,35] >= matrix_values[j,34]) {
    matrix_values[j,35] <- 0.9*matrix_values[j,34]
  }
  
  # Check that the zone will not completely drain in less than a day
  # This occurs if permeability (cm/hr) * 24 hr/day * 1 m/ 100 cm exceeds Zone 1 width (m)
  # If so, reduce permeability to 90% that of maximum allowed
  if ((0.24*matrix_values[j,17]) > (matrix_values[j,4])) {
    matrix_values[j,17] <- (0.9*(matrix_values[j,4]/0.24))
  }
  
  # Check if minimum N growth concentrations exceed maximum N growth concentrations
  # If so, set min N growth concentration to 3/4 * max N growth concentration
  # Check stems or coarse/fine roots even though allowable intervals don't overlap
  
  # For bud-new leaves
  if (matrix_values[j,28] >= matrix_values[j,22]) {
    matrix_values[j,28] <- 0.75*matrix_values[j,22]
  }
  
  # For leaves
  if (matrix_values[j,29] >= matrix_values[j,23]) {
    matrix_values[j,29] <- 0.75*matrix_values[j,23]
  }
  
  # For stems
  if (matrix_values[j,30] >= matrix_values[j,24]) {
    matrix_values[j,30] <- 0.75*matrix_values[j,24]
  }
  
  # For branches
  if (matrix_values[j,31] >= matrix_values[j,25]) {
    matrix_values[j,31] <- 0.75*matrix_values[j,25]
  }
  
  # For coarse roots
  if (matrix_values[j,32] >= matrix_values[j,26]) {
    matrix_values[j,32] <- 0.75*matrix_values[j,26]
  }
  
  # For fine roots
  if (matrix_values[j,33] >= matrix_values[j,27]) {
    matrix_values[j,33] <- 0.75*matrix_values[j,27]
  }
  
  
  j = j+1
}

# Correlations
# Use clay percentage to determine BD, permeability; relate slope and area
x <- 1
bd_ranges <- read.csv(paste0(path, "\\BD_ranges.csv"), header=TRUE)
perm_ranges <- read.csv(paste0(path,"\\perm_ranges.csv"), header=TRUE)
bd <- vector(mode="double", length=n_sims)
bd_A <- vector(mode="double", length=n_sims)
bd_B <- vector(mode="double", length=n_sims)
bd_C <- vector(mode="double", length=n_sims)
slope_coef <- vector(mode="double", length=n_sims)
slope_coef <- rnormTrunc(n_sims, mean=148, sd=63.75, min=68, max=323) # Feaster, Region 4, for 50% exceedance with avg imperv (17.4%) and rainfall (8.7 in)

while (x <= n_sims) {
  clay <- matrix_values[x,18] # clay percentage
  
  xx <- 1
  while (xx <= nrow(bd_ranges)) {
    if (clay > bd_ranges[xx,1] & clay <= bd_ranges[xx,2]) {
      bd_A[x] <- rnormTrunc(1,mean=bd_ranges[xx,4], sd=0.13, min=bd_ranges[xx,7], max=bd_ranges[xx,10])
      bd_B[x] <- rnormTrunc(1,mean=bd_ranges[xx,5], sd=0.1, min=bd_ranges[xx,8], max=bd_ranges[xx,11])
      bd_C[x] <- rnormTrunc(1,mean=bd_ranges[xx,6], sd=0.074, min=bd_ranges[xx,9], max=bd_ranges[xx,12])
    }
    xx <- xx+1
  }
  bd[x] <- mean(c(bd_A[x], bd_B[x], bd_C[x]))
  matrix_values[x,19] <- bd[x]
  
  yy <- 1
  while (yy <= nrow(perm_ranges)) {
    if (clay > perm_ranges[yy,1] & clay <= perm_ranges[yy,2]) {
      matrix_values[x,17] <- rnormTrunc(1,mean=perm_ranges[yy,6], sd=perm_ranges[yy,7], min=perm_ranges[yy,4], max=perm_ranges[yy,5])
    }
    yy <- yy+1
  }
  
  area_ha <- matrix_values[x,1]
  area_mi2 <- area_ha/259
  slope_Wolman <- 0.04*((slope_coef[x]*(area_mi2^(0.5908)))^(-0.8))*100 # Wolman (1955), Fig 25, 50% exceedence flow
  slope_Leopold <- 0.021*((slope_coef[x]*(area_mi2^(0.5908)))^(-0.49))*100 # Leopold (1953), Eqn 5, mean annual Q, avg. Midwestern stream
  slope1 <- runif(1, min=slope_Leopold, max=slope_Wolman)
  slope2 <- runif(1, min=slope_Wolman, max=slope_Leopold)
  matrix_values[x,5] <- ifelse(slope_Wolman >= slope_Leopold, slope1, slope2)
  
  x <- x+1
}

