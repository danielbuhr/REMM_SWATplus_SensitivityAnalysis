# Analysis
# Prepares outputs and performs SA for full simulation using Plischke et al. and CVD methods
# Performs time-varying analyses for annual, seasonal, and 30-day windows

#Log+1 outputs for full sim
log1_wtd_sd_3 <- log(wtd_sd_3+1)
log1_no3_sum_3 <- log(no3_sum_3+1)
log1_denit_sum_3 <- log(denit_sum_3+1)

# Plischke, full sim, log+1
log1_wtd_sens_3 <- run_sensitivity(inputs_3[1:500,], log1_wtd_sd_3[1:500], nBOOT=500, plot=FALSE)
write.table(log1_wtd_sens_3[['boot']], file = paste0(path, "\\Sensitivity_Output_", mytime, "_log1_wtd_sd.txt"), quote = FALSE, row.names = FALSE)

log1_no3_sens_3 <- run_sensitivity(inputs_3[1:500,], log1_no3_sum_3[1:500], nBOOT=500, plot=TRUE)
write.table(log1_no3_sens_3[['boot']], file = paste0(path, "\\Sensitivity_Output_", mytime, "_log1_no3_sum.txt"), quote = FALSE, row.names = FALSE)

log1_denit_sens_3 <- run_sensitivity(inputs_3[1:500,], log1_denit_sum_3[1:500], nBOOT=500, plot=TRUE)
write.table(log1_denit_sens_3[['boot']], file = paste0(path, "\\Sensitivity_Output_", mytime, "_log1_denit_sum.txt"), quote = FALSE, row.names = FALSE)

# Annual, non-moving window
n_yr <- floor(nrow(wtd_3)/365)
yrs <- as.vector(c(1, 366,731, 1096, 1462)) # 1993-1996
output_WTD_sd_yr <- matrix(,nrow=n_yr, ncol=n_sims)
output_no3_sum_yr <- matrix(,nrow=n_yr, ncol=n_sims)
output_denit_sum_yr <- matrix(,nrow=n_yr, ncol=n_sims)

l <- 1
while (l < n_sims+1) {
  
  index <-1
  for (index in 1:n_yr) {
    # Pull out the subset of WTD and calculate standard deviation
    WTD_list_yr <- as.double(wtd_3[((yrs[index])):((yrs[index+1]-1)),l+1])
    output_WTD_sd_yr[index,l] <- sd(WTD_list_yr)
    
    # Pull out the subset of riverine nitrate load and calculate sum
    no3_list_yr <- as.double(no3_3[((yrs[index])):((yrs[index+1]-1)),l+1])
    output_no3_sum_yr[index,l] <- sum(no3_list_yr)
    
    # Extract the subset of denitrification and calculate sum
    denit_list_yr <- as.double(denit_3[((yrs[index])):((yrs[index+1]-1)),l+1])
    output_denit_sum_yr[index,l] <- sum(denit_list_yr)
    
    index <- index+1
  }
  
  l = l+1
}

# Use log(output+1) transform
log1_WTD_yr <- log(output_WTD_sd_yr+1)
log1_no3_yr <- log(output_no3_sum_yr+1)
log1_denit_yr <- log(output_denit_sum_yr+1)

sens_log1_WTD_yr <- vector("list", length=nrow(log1_WTD_yr))
sens_log1_no3_yr <- vector("list", length=nrow(log1_WTD_yr))
sens_log1_denit_yr <- vector("list", length=nrow(log1_WTD_yr))

i <- 1

for (i in 1:nrow(log1_WTD_yr)) {
  
  # # Plischke, log+1
  log1_WTD_sens_yr <- run_sensitivity(inputs_3[1:500,], log1_WTD_yr[i,], nBOOT = 500, plot = FALSE)
  sens_log1_WTD_yr[[i]] <- log1_WTD_sens_yr[['boot']]
  
  log1_no3_sens_yr <- run_sensitivity(inputs_3[1:500,], log1_no3_yr[i,], nBOOT = 500, plot = FALSE)
  sens_log1_no3_yr[[i]] <- log1_no3_sens_yr[['boot']]
  
  log1_denit_sens_yr <- run_sensitivity(inputs_3[1:500,], log1_denit_yr[i,], nBOOT = 500, plot = FALSE)
  sens_log1_denit_yr[[i]] <- log1_denit_sens_yr[['boot']]
  
  i <- i+1
}

# Seasonal, non-moving window
n_seas_rev2 <- floor(nrow(wtd_3)/90)
seasons_rev2 <- as.vector(c(1,80, 172, 265, 355, 445, 537, 630, 720, 810, 902,995, 1085, 1176, 1268, 1361, 1451)) # Mar21, Jun21, Sept22, Dec21
output_WTD_sd_seas_rev2 <- matrix(,nrow=n_seas_rev2, ncol=n_sims)
output_no3_sum_seas_rev2 <- matrix(,nrow=n_seas_rev2, ncol=n_sims)
output_denit_sum_seas_rev2 <- matrix(,nrow=n_seas_rev2, ncol=n_sims)

l <- 1

while (l < n_sims+1) {
  
  index <-1
  for (index in 1:n_seas_rev2) {
    # Generate a list of daily Zone 1 WTD (mm) and calculate standard deviation
    WTD_list_seas_rev2 <- as.double(wtd_3[((seasons_rev2[index]+1)):((seasons_rev2[index+1]-1)),l+1])
    output_WTD_sd_seas_rev2[index,l] <- sd(WTD_list_seas_rev2)
    
    # Generate a list of daily subsurface dissolved NO3 loading (kg/ha) and calculate sum
    no3_list_seas_rev2 <- as.double(no3_3[((seasons_rev2[index]+1)):((seasons_rev2[index+1]-1)),l+1])
    output_no3_sum_seas_rev2[index,l] <- sum(no3_list_seas_rev2)
    
    # Generate a list of Zone 1 daily denitrification after summing the denitrification in each layer and calculate standard deviation
    denit_list_seas_rev2 <- as.double(denit_3[((seasons_rev2[index]+1)):((seasons_rev2[index+1]-1)),l+1])
    output_denit_sum_seas_rev2[index,l] <- sum(denit_list_seas_rev2)
    
    index <- index+1
  }
  
  l = l+1
}

# Use log(output+1) transform
log1_WTD_seas_rev2 <- log(output_WTD_sd_seas_rev2+1)
log1_no3_seas_rev2 <- log(output_no3_sum_seas_rev2+1)
log1_denit_seas_rev2 <- log(output_denit_sum_seas_rev2+1)

sens_log1_WTD_seas_rev2 <- vector("list", length=nrow(log1_WTD_seas_rev2))
sens_log1_no3_seas_rev2 <- vector("list", length=nrow(log1_WTD_seas_rev2))
sens_log1_denit_seas_rev2 <- vector("list", length=nrow(log1_WTD_seas_rev2))

i <- 1
for (i in 1:n_seas_rev2) {
  
  # # Plischke, log+1
  log1_WTD_sens_seas_rev2 <- run_sensitivity(inputs_3[1:500,], log1_WTD_seas_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_WTD_seas_rev2[[i]] <- log1_WTD_sens_seas_rev2[['boot']]
  
  log1_no3_sens_seas_rev2 <- run_sensitivity(inputs_3[1:500,], log1_no3_seas_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_no3_seas_rev2[[i]] <- log1_no3_sens_seas_rev2[['boot']]
  
  log1_denit_sens_seas_rev2 <- run_sensitivity(inputs_3[1:500,], log1_denit_seas_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_denit_seas_rev2[[i]] <- log1_denit_sens_seas_rev2[['boot']]
  
  i <- i+1
}


# 30 day, non-moving window
n_30 <- floor(nrow(wtd_3)/30)
output_WTD_sd_30 <- matrix(,nrow=n_30, ncol=n_sims)
output_no3_sum_30 <- matrix(,nrow=n_30, ncol=n_sims)
output_denit_sum_30 <- matrix(,nrow=n_30, ncol=n_sims)

l <- 1

while (l < n_sims+1) {
  
  index <-1
  for (index in 1:n_30) {
    # Generate a list of daily Zone 1 WTD (mm) and calculate standard deviation
    WTD_list_30 <- as.double(wtd_3[((index*30)-29):((index*30)),l+1])
    output_WTD_sd_30[index,l] <- sd(WTD_list_30)
    
    # Generate a list of daily subsurface dissolved NO3 loading (kg/ha) and calculate sum
    no3_list_30 <- as.double(no3_3[((index*30)-29):((index*30)),l+1])
    output_no3_sum_30[index,l] <- sum(no3_list_30)
    
    # Generate a list of Zone 1 daily denitrification after summing the denitrification in each layer and calculate standard deviation
    denit_list_30 <- as.double(denit_3[((index*30)-29):((index*30)),l+1])
    output_denit_sum_30[index,l] <- sum(denit_list_30)
    
    index <- index+1
  }
  
  l = l+1
}

# Use log(output+1) transform
log1_WTD_30 <- log(output_WTD_sd_30+1)
log1_no3_30 <- log(output_no3_sum_30+1)
log1_denit_30 <- log(output_denit_sum_30+1)

sens_log1_WTD_30 <- vector("list", length=nrow(log1_WTD_30))
sens_log1_no3_30 <- vector("list", length=nrow(log1_WTD_30))
sens_log1_denit_30 <- vector("list", length=nrow(log1_WTD_30))

i <- 1
for (i in 1:nrow(log1_WTD_30)) {
  
  # # Plischke, log+1
  log1_WTD_sens_30 <- run_sensitivity(inputs_3[1:500,], log1_WTD_30[i,], nBOOT = 500, plot = FALSE)
  sens_log1_WTD_30[[i]] <- log1_WTD_sens_30[['boot']]
  
  log1_no3_sens_30 <- run_sensitivity(inputs_3[1:500,], log1_no3_30[i,], nBOOT = 500, plot = FALSE)
  sens_log1_no3_30[[i]] <- log1_no3_sens_30[['boot']]
  
  log1_denit_sens_30 <- run_sensitivity(inputs_3[1:500,], log1_denit_30[i,], nBOOT = 500, plot = FALSE)
  sens_log1_denit_30[[i]] <- log1_denit_sens_30[['boot']]
  
  i <- i+1
}

# CVD analysis for full simulation

# Make sure all necessary packages are installed
library("randtoolbox")
library("ks")
library("plyr")
library("lattice")
library("Matrix")
library("sensitivity")

# Set sampling and distribution parameters
n_k <- (ncol(inputs_3)-1) # Excludes dummy variable
n_m <- 49 # Plischke used m=50
N <- 500

# Run CVD analysis
WTD_CVD_log1 = CVD(inputs_3[,1:19], log1_wtd_sd_3, n_m)
rownames(WTD_CVD_log1) <- colnames(inputs_3[,1:19])

no3_CVD_log1 = CVD(inputs_3[,1:19], log1_no3_sum_3, n_m)
rownames(no3_CVD_log1) <- colnames(inputs_3[,1:19])

denit_CVD_log1 = CVD(inputs_3[,1:19], log1_denit_sum_3, n_m)
rownames(denit_CVD_log1) <- colnames(inputs_3[,1:19])