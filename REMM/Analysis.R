# Analysis
# This code prepares output files from the REMM simulations and performs the sensitivity analysis
# Four outputs in this code are WTD, subsurface dissolved NO3, GW NO3 concentration, and denitrification 

# Create an empty numeric vector of length equal to the number of simlations
output_WTD_sd <- vector(mode = "double", length = n_sims)
output_subdisNO3_sd <- vector(mode = "double", length = n_sims)
output_denitr1_sd <- vector(mode = "double", length = n_sims)
output_TN2_sd  <- vector(mode = "double", length=n_sims)
output_subdisNO3_sum <- vector(mode = "double", length = n_sims)
output_denitr1_sum <- vector(mode = "double", length = n_sims)
output_TN2_sum <-vector(mode = "double", length = n_sims)

l <- 1
while (l < n_sims+1)
{
  n_cols2 <- max(count.fields(paste0(path,"\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_", mytime,"_", l, ".DTB")))
  file_dtb = read.csv(paste0(path,"\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_", mytime, "_", l, ".DTB"), header = FALSE, col.names = paste0("V",seq_len(304)), fill = TRUE, stringsAsFactors = FALSE)
  n_rows <- nrow(file_dtb)
  
  # Generate a list of daily Zone 1 WTD (mm) and calculate standard deviation
  WTD_list <- as.double(file_dtb[2:n_rows,7])
  output_WTD_sd[l] <- sd(WTD_list)
  
  # Generate a list of daily subsurface dissolved NO3 loading (kg/ha) and calculate standard deviaiton
  subdisNO3_list <- as.double(file_dtb[2:n_rows,123])
  output_subdisNO3_sd[l] <- sd(subdisNO3_list)
  output_subdisNO3_sum[l] <-sum(subdisNO3_list)

  # Generate a list of Zone 1 daily denitrification after summing the denitrification in each layer and calculate standard deviation
  m = 2
  denitr1_list <- vector("double", n_rows-1)
  for (m in 2:n_rows){
    total <- sum(as.double(file_dtb[m,279:282]))
    denitr1_list[m-1] <- total
    m = m+1
  }
  
  output_denitr1_sd[l] <- sd(denitr1_list)
  output_denitr1_sum[l] <- sum(denitr1_list)
  
  # Generate a list of total nitrogen
  g <- 2
  TN_list2 <- vector("double", n_rows-1)
  for (g in 2:n_rows) {
    # N outputs from each zone
    total_TN2 <- sum(as.double(file_dtb[g,c(65, 70:80, 81, 86:96, 97, 102:112, 113:123)]))
    TN_list2[g-1] <- total_TN2
    g <- g+1
  }
  output_TN2_sd[l] <- sd(TN_list2)
  output_TN2_sum[l] <- sum(TN_list2)
  
  l = l+1
}

# Determine in which simulations the errors were occurring
p <- 1
errors2 <- vector(mode = "character")
o <- 1
while (o < n_sims+1) {
  
  if (is.na(output_WTD_sd[o]) | output_WTD_sd[o] <= 0) {
    errors2[p] <- paste0("WTD_", o)
    p <- p+1
  }
  
  if (is.na(output_subdisNO3_sum[o]) | output_subdisNO3_sum[o] <= 0) {
    errors2[p] <- paste0("subdisNO3_", o)
    p <- p+1
  }
  
  
  if (is.na(output_denitr1_sum[o]) | output_denitr1_sum[o] <= 0) {
    errors2[p] <- paste0("denitr1_", o)
    p <- p+1
  }
  
  if (is.na(output_TN2_sum[o]) | output_TN2_sum[o] <= 0) {
    errors2[p] <- paste0("TN2_", o)
    p <- p+1
  }
  
  o <- o+1
}

print(errors2)
print(sum(stringr::str_count(errors, "WTD")))
print(sum(stringr::str_count(errors, "subdis")))
print(sum(stringr::str_count(errors, "denitr")))
print(sum(stringr::str_count(errors, "TN2")))

# Removes all simulations with errors from whichever all output types
no_error2 <- !(is.na(output_WTD_sd) | output_WTD_sd <= 0 | is.na(output_subdisNO3_sum) | output_subdisNO3_sum <= 0 | is.na(output_TN2_sum) | output_TN2_sum <= 0 |is.na(output_denitr1_sum) | output_denitr1_sum <= 0)
subset_WTD_rev2 <- output_WTD_sd[no_error2]
subset_inputs2 <- input_values_dummy[no_error2,]
subset_subdisNO3_sum <- output_subdisNO3_sum[no_error2]
subset_denitr1_sum <- output_denitr1_sum[no_error2]
subset_TN2_sum <- output_TN2_sum[no_error2]

# Log transform +1 (with natural log) all outputs to better visualize trends
log1_output_WTD_rev2 <- log(subset_WTD_rev2[1:1000]+1)
log1_output_subdisNO3_sum <- log(subset_subdisNO3_sum[1:1000]+1)
log1_output_denitr1_sum <- log(subset_denitr1_sum[1:1000]+1)
log1_output_TN2_sum <- log(subset_TN2_sum[1:1000]+1)

# Re-run the sensitivity analysis with the log transformed outputs, and nBOOT = 500
WTD_sens_log1_500_rev2 <- run_sensitivity(subset_inputs2[1:1000,], log1_output_WTD_rev2, nBOOT = 500, plot = FALSE)
write.table(WTD_sens_log1_500_rev2[['boot']], file = paste0("Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files\\Sensitivity_Output_", mytime, "_WTD_rev2_log1.txt"), quote = FALSE, row.names = FALSE)

subdisNO3_sens_log1_500_sum <- run_sensitivity(subset_inputs2[1:1000,], log1_output_subdisNO3_sum, nBOOT = 500, plot = FALSE)
write.table(subdisNO3_sens_log1_500_sum[['boot']], file = paste0("Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files\\Sensitivity_Output_", mytime, "_subdisNO3_rev2_log1.txt"), quote = FALSE, row.names = FALSE)

denitr1_sens_log1_500_sum <- run_sensitivity(subset_inputs2[1:1000,], log1_output_denitr1_sum, nBOOT = 500, plot = FALSE)
write.table(denitr1_sens_log1_500_sum[['boot']], file = paste0("Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files\\Sensitivity_Output_", mytime, "_denitr1_rev2_log1.txt"), quote = FALSE, row.names = FALSE)

TN2_sens_log1_500_sum <- run_sensitivity(subset_inputs2[1:1000,], log1_output_TN2_sum, nBOOT = 500, plot = FALSE)
write.table(TN2_sens_log1_500_sum[['boot']], file = paste0("Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files\\Sensitivity_Output_", mytime, "_TN2_rev2_log1.txt"), quote = FALSE, row.names = FALSE)

# Time-varying analyses
# Annual, non-moving window
n_yr <- floor(1461/365)
yrs <- as.vector(c(1, 367,732, 1097, 1462))
output_WTD_sd_yr_rev2 <- matrix(,nrow=n_yr, ncol=n_sims)
output_denitr1_sum_yr_rev2 <- matrix(,nrow=n_yr, ncol=n_sims)
output_TN2_sum_yr_rev2  <- matrix(,nrow=n_yr, ncol=n_sims)

l <- 1

while (l < n_sims+1) {
  n_cols2 <- max(count.fields(paste0(path,"\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_", mytime,"_", l, ".DTB")))
  file_dtb = read.csv(paste0(path,"\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_", mytime, "_", l, ".DTB"), header = FALSE, col.names = paste0("V",seq_len(304)), fill = TRUE, stringsAsFactors = FALSE)
  n_rows <- nrow(file_dtb)
  
  index <-1
  for (index in 1:n_yr) {
    # Generate a list of daily Zone 1 WTD (mm) and calculate standard deviation
    WTD_list_yr_rev2 <- as.double(file_dtb[((yrs[index]+1)):((yrs[index+1])),7])
    output_WTD_sd_yr_rev2[index,l] <- sd(WTD_list_yr_rev2)
    
    # Generate a list of Zone 1 daily denitrification after summing the denitrification in each layer and calculate standard deviation
    m = 2
    denitr1_list_yr_rev2 <- vector("double", n_rows-1)
    for (m in ((yrs[index]+1)):((yrs[index+1]))){
      total <- sum(as.double(file_dtb[m,279:282]))
      denitr1_list_yr_rev2[m-1] <- total
      m = m+1
    }
    output_denitr1_sum_yr_rev2[index,l] <- sum(denitr1_list_yr_rev2)
    
    # Generate a list of total nitrogen as outputs from zone 1 (lines incl. zone N inputs is commented out)
    g <- 2
    TN_list2_yr_rev2 <- vector("double", n_rows-1)
    for (g in ((yrs[index]+1)):((yrs[index+1]))) {
      total_TN2 <- sum(as.double(file_dtb[g,c(65, 70:80, 81, 86:96, 97, 102:112, 113:123)]))
      TN_list2_yr_rev2[g-1] <- total_TN2
      g <- g+1
    }
    output_TN2_sum_yr_rev2[index,l] <- sum(TN_list2_yr_rev2)
    
    index <- index+1
  }
  
  l = l+1
}
subset_WTD_yr_rev2 <- output_WTD_sd_yr_rev2[,no_error2]
subset_inputs_yr_rev2 <- input_values_dummy[no_error2,]
subset_TN2_yr_rev2 <- output_TN2_sum_yr_rev2[,no_error2]
subset_denitr_yr_rev2 <- output_denitr1_sum_yr_rev2[,no_error2]

subset_matrix_values_rev2 <- matrix_values[no_error2,]

# Log transformations
# Use log(output+1) due to zero values in outputs
log1_WTD_yr_rev2 <- log(subset_WTD_yr_rev2+1)
log1_denitr1_yr_rev2 <- log(subset_denitr_yr_rev2+1)
log1_TN2_yr_rev2 <- log(subset_TN2_yr_rev2+1)

# Some denitr1 logs came out as NaN (neg values in subset_denitr...)
# Identify which sims those came from, remove from time-varying analysis
no_error_yr_rev2 <- !(is.na(log_denitr1_yr_rev2))
no_error_yr_vec2 <- vector(mode="numeric")
i <- 1
j <- 1
for (i in 1:nrow(no_error_yr_rev2)) {
  for (j in 1:ncol(no_error_yr_rev2)) {
    if (no_error_yr_rev2[i,j] == FALSE) {
      no_error_yr_vec2[j] <- j
    }
    j <- j+1
  }
  i <- i+1
}
no_error_yr_vec2 <- na.omit(no_error_yr_vec2)

log1_WTD_yr_rev2 <- log1_WTD_yr_rev2[,-no_error_yr_vec2]
log1_denitr1_yr_rev2 <- log1_denitr1_yr_rev2[,-no_error_yr_vec2]
log1_TN2_yr_rev2 <- log1_TN2_yr_rev2[,-no_error_yr_vec2]

subset_log_inputs_yr_rev2 <- subset_matrix_values_rev2[-no_error_yr_vec2,]
subset_log_inputs2 <- subset_inputs2[-no_error_yr_vec2,]

sens_log1_WTD_yr_rev2 <- vector("list", length=nrow(log1_WTD_yr_rev2))
sens_log1_TN2_yr_rev2 <- vector("list", length=nrow(log1_WTD_yr_rev2))
sens_log1_denitr_yr_rev2 <- vector("list", length=nrow(log1_WTD_yr_rev2))

i <- 1
for (i in 1:nrow(log_WTD_yr_rev2)) {
  # Run the sens_logitivity analysis with the subset of values
  # Plischke, log+1
  log1_WTD_sens_yr_rev2 <- run_sensitivity(subset_log_inputs2, log1_WTD_yr_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_WTD_yr_rev2[[i]] <- log1_WTD_sens_yr_rev2[['boot']]
  
  log1_denitr1_sens_yr_rev2 <- run_sensitivity(subset_log_inputs2, log1_denitr1_yr_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_denitr_yr_rev2[[i]] <- log1_denitr1_sens_yr_rev2[['boot']]
  
  log1_TN2_sens_yr_rev2 <- run_sensitivity(subset_log_inputs2, log1_TN2_yr_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_TN2_yr_rev2[[i]] <- log1_TN2_sens_yr_rev2[['boot']]
  
  i <- i+1
}

# 90 day, non-moving window
n_seas_rev2 <- floor(1461/90)
seasons_rev2 <- as.vector(c(1,81, 173, 266, 356, 447, 539, 632, 722, 812, 904,997, 1087, 1177, 1269, 1362, 1452)) # Mar21, Jun21, Sept22, Dec21
output_WTD_sd_seas_rev2 <- matrix(,nrow=n_seas_rev2, ncol=n_sims)
output_denitr1_sum_seas_rev2 <- matrix(,nrow=n_seas_rev2, ncol=n_sims)
output_TN2_sum_seas_rev2  <- matrix(,nrow=n_seas_rev2, ncol=n_sims)

l <- 1

while (l < n_sims+1) {
  n_cols2 <- max(count.fields(paste0("Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_", mytime,"_", l, ".DTB")))
  file_dtb = read.csv(paste0("Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_", mytime, "_", l, ".DTB"), header = FALSE, col.names = paste0("V",seq_len(304)), fill = TRUE, stringsAsFactors = FALSE)
  n_rows <- nrow(file_dtb)
  
  index <-1
  for (index in 1:n_seas_rev2) {
    # Generate a list of daily Zone 1 WTD (mm) and calculate standard deviation
    WTD_list_seas_rev2 <- as.double(file_dtb[((seasons_rev2[index]+1)):((seasons_rev2[index+1])),7])
    output_WTD_sd_seas_rev2[index,l] <- sd(WTD_list_seas_rev2)
    
    # Generate a list of Zone 1 daily denitrification after summing the denitrification in each layer and calculate standard deviation
    m = 2
    denitr1_list_seas_rev2 <- vector("double", n_rows-1)
    for (m in ((seasons_rev2[index]+1)):((seasons_rev2[index+1]))){
      total <- sum(as.double(file_dtb[m,279:282]))
      denitr1_list_seas_rev2[m-1] <- total
      m = m+1
    }
    output_denitr1_sum_seas_rev2[index,l] <- sum(denitr1_list_seas_rev2)
    
    # Generate a list of total nitrogen as outputs from zone 1 (lines incl. zone N inputs is commented out)
    g <- 2
    TN_list2_seas_rev2 <- vector("double", n_rows-1)
    for (g in ((seasons_rev2[index]+1)):((seasons_rev2[index+1]))) {
      total_TN2 <- sum(as.double(file_dtb[g,c(65, 70:80, 81, 86:96, 97, 102:112, 113:123)]))
      TN_list2_seas_rev2[g-1] <- total_TN2
      g <- g+1
    }
    output_TN2_sum_seas_rev2[index,l] <- sum(TN_list2_seas_rev2)
    
    index <- index+1
  }
  
  l = l+1
}

subset_WTD_seas_rev2 <- output_WTD_sd_seas_rev2[,no_error2]
subset_inputs_seas_rev2 <- input_values_dummy[no_error2,]
subset_TN2_seas_rev2 <- output_TN2_sum_seas_rev2[,no_error2]
subset_denitr_seas_rev2 <- output_denitr1_sum_seas_rev2[,no_error2]

subset_matrix_values_rev2 <- matrix_values[no_error2,]

sens_WTD_seas_rev2 <- vector("list", length=nrow(subset_WTD_seas_rev2))
sens_TN2_seas_rev2 <- vector("list", length=nrow(subset_WTD_seas_rev2))
sens_denitr_seas_rev2 <- vector("list", length=nrow(subset_WTD_seas_rev2))

# Log transformations
# Use log(output+1) due to zero values in outputs (even with sums)
log1_WTD_seas_rev2 <- log(subset_WTD_seas_rev2+1)
log1_denitr1_seas_rev2 <- log(subset_denitr_seas_rev2+1)
log1_TN2_seas_rev2 <- log(subset_TN2_seas_rev2+1)

# Some denitr1 logs came out as NaN (neg values in subset_denitr...)
# Identify which sims those came from, remove from time-varying analysis
# Remove from complete analysis and redo??? Right now, no
no_error_seas_rev2 <- !(is.na(log1_denitr1_seas_rev2))
no_error_seas_vec3 <- vector(mode="numeric")
i <- 1
j <- 1
for (i in 1:nrow(no_error_seas_rev2)) {
  for (j in 1:ncol(no_error_seas_rev2)) {
    if (no_error_seas_rev2[i,j] == FALSE) {
      no_error_seas_vec3[j] <- j
    }
    j <- j+1
  }
  i <- i+1
}
no_error_seas_vec3 <- na.omit(no_error_seas_vec3)


log1_WTD_seas_rev2 <- log1_WTD_seas_rev2[,-no_error_seas_vec3]
log1_denitr1_seas_rev2 <- log1_denitr1_seas_rev2[,-no_error_seas_vec3]
log1_TN2_seas_rev2 <- log1_TN2_seas_rev2[,-no_error_seas_vec3]


subset_log_inputs_seas_rev2 <- subset_matrix_values_rev2[-no_error_seas_vec3,]
subset_log_inputs3 <- subset_inputs2[-no_error_seas_vec3,]

sens_log1_WTD_seas_rev2 <- vector("list", length=nrow(log1_WTD_seas_rev2))
sens_log1_TN2_seas_rev2 <- vector("list", length=nrow(log1_WTD_seas_rev2))
sens_log1_denitr_seas_rev2 <- vector("list", length=nrow(log1_WTD_seas_rev2))

i <- 1

for (i in 1:n_seas_rev2) {
  # Run the sens_logitivity analysis with the subset of values
  
  # # Plischke, log+1
  log1_WTD_sens_seas_rev2 <- run_sensitivity(subset_log_inputs3, log1_WTD_seas_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_WTD_seas_rev2[[i]] <- log1_WTD_sens_seas_rev2[['boot']]
  # 
  
  log1_denitr1_sens_seas_rev2 <- run_sensitivity(subset_log_inputs3, log1_denitr1_seas_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_denitr_seas_rev2[[i]] <- log1_denitr1_sens_seas_rev2[['boot']]
  # 
  log1_TN2_sens_seas_rev2 <- run_sensitivity(subset_log_inputs3, log1_TN2_seas_rev2[i,], nBOOT = 500, plot = FALSE)
  sens_log1_TN2_seas_rev2[[i]] <- log1_TN2_sens_seas_rev2[['boot']]
  # 
  i <- i+1
}

# 30 day, non-moving window
n_30 <- floor(1461/30)
output_WTD_sd_30_rev2 <- matrix(,nrow=n_30, ncol=n_sims)
output_denitr1_sum_30_rev2 <- matrix(,nrow=n_30, ncol=n_sims)
output_TN2_sum_30_rev2  <- matrix(,nrow=n_30, ncol=n_sims)

# Create matrix of output sd in 90-day moving windows (row) for each simulation (col)
l <- 1

while (l < n_sims+1) {
  n_cols2 <- max(count.fields(paste0("Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_", mytime,"_", l, ".DTB")))
  file_dtb = read.csv(paste0("Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\REMM_Files\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_", mytime, "_", l, ".DTB"), header = FALSE, col.names = paste0("V",seq_len(304)), fill = TRUE, stringsAsFactors = FALSE)
  #file_dtb <- paste0("file_dtb_", l)
  n_rows <- nrow(file_dtb)
  
  index <-1
  for (index in 1:n_30) {
    # Generate a list of daily Zone 1 WTD (mm) and calculate standard deviation
    WTD_list_30_rev2 <- as.double(file_dtb[((index*30)-28):((index*30)+1),7])
    output_WTD_sd_30_rev2[index,l] <- sd(WTD_list_30_rev2)
    
    # Generate a list of Zone 1 daily denitrification after summing the denitrification in each layer and calculate standard deviation
    m = 2
    denitr1_list_30_rev2 <- vector("double", n_rows-1)
    for (m in ((index*30)-28):((index*30)+1)){
      total <- sum(as.double(file_dtb[m,279:282]))
      denitr1_list_30_rev2[m-1] <- total
      m = m+1
    }
    
    output_denitr1_sum_30_rev2[index,l] <- sum(denitr1_list_30_rev2)
    
    # Generate a list of total nitrogen as outputs from zone 1 (lines incl. zone N inputs is commented out)
    g <- 2
    TN_list2_30_rev2 <- vector("double", n_rows-1)
    for (g in ((index*30)-28):((index*30)+1)) {
      total_TN2 <- sum(as.double(file_dtb[g,c(65, 70:80, 81, 86:96, 97, 102:112, 113:123)]))
      TN_list2_30_rev2[g-1] <- total_TN2
      g <- g+1
    }
    output_TN2_sum_30_rev2[index,l] <- sum(TN_list2_30_rev2)
    
    index <- index+1
  }
  
  l = l+1
}

subset_WTD_30_rev2 <- output_WTD_sd_30_rev2[,no_error2]
subset_inputs_30_rev2 <- input_values_dummy[no_error2,]
subset_TN2_30_rev2 <- output_TN2_sum_30_rev2[,no_error2]
subset_denitr_30_rev2 <- output_denitr1_sum_30_rev2[,no_error2]

# Log transformations
# Use log(output+1) due to zero values in outputs
log1_WTD_30_rev2 <- log(subset_WTD_30_rev2+1)
log1_subdisNO3_30_rev2 <- log(subset_subdisNO3_30_rev2+1)
log1_denitr1_30_rev2 <- log(subset_denitr_30_rev2+1)
log1_TN2_30_rev2 <- log(subset_TN2_30_rev2+1)

# Some denitr1 logs came out as NaN (neg values in subset_denitr...)
# Identify which sims those came from, remove from time-varying analysis
no_error_30_rev2 <- !(is.na(log_denitr1_30_rev2))
no_error_30_vec2 <- vector(mode="numeric")
i <- 1
j <- 1
for (i in 1:nrow(no_error_30_rev2)) {
  for (j in 1:ncol(no_error_30_rev2)) {
    if (no_error_30_rev2[i,j] == FALSE) {
      no_error_30_vec2[j] <- j
    }
    j <- j+1
  }
  i <- i+1
}
no_error_30_vec2 <- na.omit(no_error_30_vec2)

log1_WTD_30_rev2 <- log1_WTD_30_rev2[,-no_error_30_vec2]
log1_subdisNO3_30_rev2 <- log1_subdisNO3_30_rev2[,-no_error_30_vec2]
log1_denitr1_30_rev2 <- log1_denitr1_30_rev2[,-no_error_30_vec2]
log1_TN2_30_rev2 <- log1_TN2_30_rev2[,-no_error_30_vec2]

subset_log_inputs_30_rev2 <- subset_matrix_values_rev2[-no_error_30_vec2,]
subset_log_inputs2 <- subset_inputs2[-no_error_30_vec2,]

sens_log1_WTD_30_rev2 <- vector("list", length=nrow(log1_WTD_30_rev2))
sens_log1_TN2_30_rev2 <- vector("list", length=nrow(log1_WTD_30_rev2))
sens_log1_denitr_30_rev2 <- vector("list", length=nrow(log1_WTD_30_rev2))

i <- 1
for (i in 1:nrow(log1_WTD_30_rev2)) {
  # # Plischke, log+1
  log1_WTD_sens_30_rev2 <- run_sensitivity(subset_log_inputs2, log1_WTD_30_rev2[i,], nBOOT = 600, plot = FALSE)
  sens_log1_WTD_30_rev2[[i]] <- log1_WTD_sens_30_rev2[['boot']]
  
  log1_denitr1_sens_30_rev2 <- run_sensitivity(subset_log_inputs2, log1_denitr1_30_rev2[i,], nBOOT = 600, plot = FALSE)
  sens_log1_denitr_30_rev2[[i]] <- log1_denitr1_sens_30_rev2[['boot']]
  
  log1_TN2_sens_30_rev2 <- run_sensitivity(subset_log_inputs2, log1_TN2_30_rev2[i,], nBOOT = 600, plot = FALSE)
  sens_log1_TN2_30_rev2[[i]] <- log1_TN2_sens_30_rev2[['boot']]
  
  i <- i+1
}

# CVD sensitivity analysis for full simulation
# Use CVD_function.R from Baroni and Francke (2020) code -- https://github.com/baronig/GSA-cvd

# Make sure all necessary packages are installed
library("randtoolbox")
library("plyr")
library("lattice")
library("Matrix")
library(ks)
library("RColorBrewer")

subset_matrix_values_rev2 <- matrix_values[no_error2,]

# Set sampling and distribution parameters
n_k <- ncol(subset_matrix_values_rev2)
n_m <- 49
N <- 1000

names <- colnames(matrix_values)
names <- gsub("_", " ", names)

# Run the CVD and SCA sensitivity methods for each target output, with log+1
log1_WTD_CVD_rev2 = CVD(subset_matrix_values_rev2[1:1000,], log1_output_WTD_rev2[1:1000], n_m)
rownames(log1_WTD_CVD_rev2) <- names

log1_TN2_CVD_rev2= CVD(subset_matrix_values_rev2[1:1000,], log1_output_TN2_sum[1:1000], n_m)
rownames(log1_TN2_CVD_rev2) <- names

log1_denitr1_CVD_rev2=CVD(subset_matrix_values_rev2[1:1000,], log1_output_denitr1_sum[1:1000], n_m)
rownames(log1_denitr1_CVD_rev2) <- names

log1_subdisNO3_CVD_rev2=CVD(subset_matrix_values_rev2[1:1000,], log1_output_subdisNO3_sum[1:1000], n_m)
rownames(log1_subdisNO3_CVD_rev2) <- names