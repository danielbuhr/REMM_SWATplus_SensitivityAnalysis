# Value_Generation
# Generate matrix of values based on parameters and ranges that have been selected

# Call external libraries needed in this script
library(EnvStats) # For random number generation from truncated distribution
library(SWATplusR) # Fpr simulating SWAT+ within R
library(dplyr) # For data manipulation

path <- "Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\SWAT+"
setwd(path)
mytime <- format(base::Sys.time(), "%m%d%Y_%H%M") # adds file nomenclature for date (mmmddYYYY_HrMin)

# Make sure you have saved a .csv file for the parameter names and ranges
# Format should be parameter name, distribution type, min, max, mean, std dev
# Read in the .csv file, with below name
ranges = read.csv(paste0(path,"\\SWAT+_VarRanges_v2.csv"), header = TRUE)

# Create matrix for all parameters and their values for n_sims simulations
n_sims = 600 # Set high and trim down to 500 after errors are removed
n_params = nrow(ranges)

# Makes the matrix 
variables <- vector(mode = "character", length = nrow(ranges))
variables <- ranges[,1]
matrix_values = matrix(nrow = n_sims, ncol = n_params)
colnames(matrix_values) <- variables
list(t, length=n_sims)

# Populate matrix
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

# Read existing REMM inputs
file_basin <- read.table(paste0(path,"\\LREW_TxtInOut\\parameters.bsn"),
                         header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_channel <- read.table(paste0(path,"\\LREW_TxtInOut\\hyd-sed-lte.cha"),
                           header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_topo <- read.table(paste0(path,"\\LREW_TxtInOut\\topography.hyd"),
                        header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_plant <- read.table(paste0(path,"\\LREW_TxtInOut\\plants.plt"),
                         header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_soil <- read.table(paste0(path,"\\LREW_TxtInOut\\soils.sol"),
                        header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_field <- read.table(paste0(path,"\\LREW_TxtInOut\\field.fld"),
                         header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_aqu <- read.table(paste0(path,"\\LREW_TxtInOut\\aquifer.aqu"),
                       header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_cio <- read.table(paste0(path,"\\LREW_TxtInOut\\file.cio"),
                       header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_hrudata <- read.table(paste0(path,"\\LREW_TxtInOut\\hru-data.hru"),
                           header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
file_routunit_def <- read.table(paste0(path,"\\LREW_TxtInOut\\rout_unit.def"),
                                header=FALSE, fill=TRUE, stringsAsFactors=FALSE)

# Create select new input files for SWAT+

# Create vectors for outputs
wtd_sd <- vector(mode="double", length=n_sims)
denit_sd <- vector(mode="double", length=n_sims)
no3_sd <- vector(mode="double", length=n_sims)

i <- 1 # row index
while(i < n_sims+1) {
  
  file_basin[3,11] <- (as.numeric(file_basin[3,11])*matrix_values[i,1]) # nperco
  file_basin[3,23] <- (as.numeric(file_basin[3,23])*matrix_values[i,2]) # cdn, denit_exp
  file_basin[3,24] <- (as.numeric(file_basin[3,24])*matrix_values[i,3]) # sdnco, denit_frac
  
  a <- 3
  while (a <= nrow(file_channel)) {
    file_channel[a,4] <- (as.numeric(file_channel[a,4])*matrix_values[i,4]) # stream depth
    file_channel[a,22] <- (as.numeric(file_channel[a,22])*matrix_values[i,5]) # headcut height
    a <- a+1
  }
  
  b<-3
  while (b <= nrow(file_topo)) {
    file_topo[b,2] <- (as.numeric(file_topo[b,2])*matrix_values[i,6]) # slp
    file_topo[b,3] <- (as.numeric(file_topo[b,3])*matrix_values[i,7]) # slp_len
    file_topo[b,4] <- (as.numeric(file_topo[b,4])*matrix_values[i,8]) # lat_len
    b <- b+1
  }
  
  c<- 3
  while (c<=nrow(file_plant)) {
    file_plant[c,16] <- (as.numeric(file_plant[c,16])*matrix_values[i,9]) # max root depth by plant type
    c <- c+1
  }
  
  d<-3
  for (d in c(3,5,8,12,16,20,25,29,33,37,41,45)) {
    file_soil[d,4] <- (as.numeric(file_soil[d,4])*matrix_values[i,10]) # soil profile total depth = max root depth
    d <- d+1
  }
  
  e <- 4
  for (e in c(4,6:7,9:11,13:15,17:19,21:24,26:28,30:32,34:36,38:40,42:44,46)) {
    file_soil[e,1] <- (as.numeric(file_soil[e,1])*matrix_values[i,10]) # soil layer bottom depth
    file_soil[e,2] <- (as.numeric(file_soil[e,2])*matrix_values[i,11]) # soil layer bulk density
    file_soil[e,3] <- (as.numeric(file_soil[e,3])*matrix_values[i,12]) # soil layer awc
    file_soil[e,4] <- (as.numeric(file_soil[e,4])*matrix_values[i,13]) # soil layer k
    file_soil[e,5] <- (as.numeric(file_soil[e,5])*matrix_values[i,14]) # soil layer carbon
    file_soil[e,14] <- (as.numeric(file_soil[e,14])*matrix_values[i,15]) # soil layer pH
    e <- e+1
  }
  
  f <- 3
  while (f <= nrow(file_field)) {
    file_field[f,3] <- (as.numeric(file_field[f,3])*matrix_values[i,16]) # field width
    file_field[f,2] <- (as.numeric(file_field[f,2])*matrix_values[i,17]) # field length
    f <- f+1
  }
  
  g <- 3
  while (g <= nrow(file_aqu)) {
    file_aqu[g,5] <- (as.numeric(file_aqu[g,5])*matrix_values[i,18]) # depth to bottom of aquifer
    file_aqu[g,6] <- (as.numeric(file_aqu[g,6])*matrix_values[i,19]) # depth to initial water table
    file_aqu[g,7] <- (as.numeric(file_aqu[g,7])*matrix_values[i,20]) # initial nitrate in shallow aquifer
    file_aqu[g,11] <- (as.numeric(file_aqu[g,11])*matrix_values[i,21]) # baseflow rate max
    file_aqu[g,17] <- (as.numeric(file_aqu[g,17])*matrix_values[i,22]) # min WTD for return flow
    g <- g+1
  }
  
  file_cio[3,3] <- paste0("parameters_SensAnalysis_", mytime, "_", i,".bsn")
  file_cio[6,8] <- paste0("hyd-sed-lte_SensAnalysis_", mytime, "_", i,".cha")
  file_cio[17,3] <- paste0("topography_SensAnalysis_", mytime, "_", i,".hyd")
  file_cio[19,2] <- paste0("plants_SensAnalysis_", mytime, "_", i,".plt")
  file_cio[24,2] <- paste0("soils_SensAnalysis_", mytime, "_", i,".sol")
  file_cio[17,4] <- paste0("field_SensAnalysis_", mytime, "_", i,".fld")
  file_cio[13,3] <- paste0("aquifer_SensAnalysis_", mytime, "_", i,".aqu")
  
  # Perform checks to make sure values don't lie outside predetermined range
  # Don't have to check nperco, cdn, sdnco, hc_ht, slp_len, flo_min because singular values were used and accounted for in the ranges
  # awc, field_wid, field_len, dep_bot, dep_wt, no3, bf_max have limits that are 0 or negotiable
  
  # channel: stream depth s
  x <- 3
  
  while (x <= nrow(file_channel)) {
    if (file_channel[x,4]< 0.1) {
      file_channel[x,4] <- 0.1
    }
    if (file_channel[x,4] > 20) {
      file_channel[x,4] <- 20
    }
    
    x <- x+1
  }
  
  # topo: slope[2] and lat_len[4]
  x <- 3
  
  while (x <= nrow(file_topo)) {
    if (file_topo[x,2] < 0.001) {
      file_topo[x,2] <- 0.001
    }
    if (file_topo[x,2] > 0.2) {
      file_topo[x,2] <- 0.2
    }
    
    if (file_topo[x,4] < 10) {
      file_topo[x,4] <- 10
    }
    if (file_topo[x,4] > 1000) {
      file_topo[x,4] <- 1000
    }
    
    x <- x+1
  }
  
  # plants: rdmx
  x <- 3 
  
  while (x <= nrow(file_plant)) {
    if (file_plant[x,16] >0 | file_plant[x,16] < 0.15) {
      file_plant[x,16] <- 0.15
    }
    if (file_plant[x,16] > 10.2) {
      file_plant[x,16] <- 10.2 # units should be in mm but are actually in m (?)
    }
    
    x <- x+1
  }
  
  # soils: zmx; min value in files is 25.4*5 is max multiplier
  x <- 3
  
  for (x in c(3,5,8,12,16,20,25,29,33,37,41,45)) {
    if (file_soil[x,1] > 127 | file_soil[x,1]< 150) {
      file_soil[x,1] <- 150
    }
    if (file_soil[x,1] > 10200) {
      file_soil[x,1] <- 102000
    }
    x <- x+1
  }
  
  # soils: bulk density[2; min is 0.01*2], k[4], cbn[5], ph[14]
  x <- 4
  
  for (x in c(4,6:7,9:11,13:15,17:19,21:24,26:28,30:32,34:36,38:40,42:44,46)) {
    
    # bulk density
    if (file_soil[x,2] > 0.02 | file_soil[x,2] < 0.9) {
      file_soil[x,2] <- 0.9
    }
    if (file_soil[x,2] > 2.5) {
      file_soil[x,2] <- 2.5
    }
    
    # k
    if (file_soil[x,4] < 0.01) {
      file_soil[x,4] <- 0.01
    }
    if (file_soil[x,4] > 2000) {
      file_soil[x,4] <- 2000
    }
    
    # cbn
    if (file_soil[x,5] > 0 | file_soil[x,5] < 0.05) {
      file_soil[x,5] <- 0.05
    }
    if (file_soil[x,5] > 10) {
      file_soil[x,5] <- 10
    }
    
    # pH
    if (file_soil[x,14] > 0 | file_soil[x,14] < 3) {
      file_soil[x,14] <- 3
    }
    if (file_soil[x,14] > 10) {
      file_soil[x,14] <- 10
    }
    
    x <- x+1
  }
  
  k <- 3
  
  # Make sure no parameters break their mathematical relations to other parameters
  # Has to be done for each HRU by reading those files in but then the value changes defined in the matrix
  while (k <= nrow(file_channel)) {
    
    # Check if stream depth exceeds headcut height
    # If so, make depth = 0.9*headcut height
    if (file_channel[k,4] >= file_channel[k,22]) {
      file_channel[k,4] <- (0.9*as.numeric(file_channel[k,22]))
    }
    k <- k+1
  }
  
  l <- 3
  # Dep_bot must exceed dep_wt
  # If not, make dep_bot 1.5x dep_wt
  while (l <= nrow(file_aqu)) {
    
    if (file_aqu[l,5] <= file_aqu[l,6]) {
      
      file_aqu[l,5] <- as.numeric(file_aqu[l,6])*1.5
    }
    
    l <- l+1
  }
  
  
  # No need to check if depth to bottom of aquifer (not necessary) and initial water table depth (already exceeds in base model)
  # exceeds soil layer depth*number of layers

  # Write the files
  # Create a new directory for this iteration i
  dir.create(paste0(path,"\\LREW\\SensAnalysis_",
                    mytime, "_", i))
  # Copy all the TxtInOut files from the original LREW model
  TxtInOut <- list.files(paste0(path,"\\LREW_TxtInOut2"), full.names=TRUE)
  file.copy(TxtInOut, paste0(path, "\\LREW\\SensAnalysis_",
                             mytime, "_", i))
  # Delete the files in that new folder that will be replaced
  unlink(paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\parameters.bsn"))
  file.remove(paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\hyd-sed-lte.cha"))
  file.remove(paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\topography.hyd"))
  file.remove(paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\plants.plt"))
  file.remove(paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\soils.sol"))
  file.remove(paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\field.fld"))
  file.remove(paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\aquifer.aqu"))
  file.remove(paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\file.cio"))
  # Write the new files to the new directory
  write.table(file_basin, 
              file= paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\parameters_SensAnalysis_",
                           mytime, "_", i,".bsn"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_channel, 
              file= paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\hyd-sed-lte_SensAnalysis_",
                           mytime, "_", i,".cha"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_topo, 
              file= paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\topography_SensAnalysis_",
                           mytime, "_", i,".hyd"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_plant, 
              file= paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\plants_SensAnalysis_",
                           mytime, "_", i,".plt"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_soil, 
              file= paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\soils_SensAnalysis_",
                           mytime, "_", i,".sol"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_field, 
              file= paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\field_SensAnalysis_",
                           mytime, "_", i,".fld"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_aqu, 
              file= paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\aquifer_SensAnalysis_",
                           mytime, "_", i,".aqu"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_cio, 
              file= paste0(path,"\\LREW\\SensAnalysis_", mytime, "_", i, "\\file.cio"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  
  # Perform the SWAT+ simulation with the new files
  wtd<-define_output(file="basin_aqu", variable="dep_wt", unit=1249)
  denit<-define_output(file="basin_nb", variable="denit", unit=1)
  no3_out<-define_output(file="basin_sd_cha", variable="no3_out", unit=1)
  
  simulation <- run_swatplus(paste0(path,"\\LREW\\SensAnalysis_",mytime, "_", i), 
                             output=list(dep_wt = define_output(file="basin_aqu", variable="dep_wt", unit=1249),
                                         denit=define_output(file="basin_nb", variable="denit", unit=1),
                                         no3_out=define_output(file="basin_sd_cha", variable="no3_out", unit=1)))
  
  wtd_sd[i] <- sd(simulation$dep_wt)
  denit_sd[i] <- sd(simulation$denit)
  no3_sd[i] <- sd(simulation$no3_out)
  
  i <- i+1
}

swatplus_run3 <- run_swatplus(paste0(path,"\\LREW_TxtInOut2"), 
                              output=list(wtd = define_output(file="basin_aqu", variable="dep_wt", unit=1249),
                                          denit=define_output(file="basin_nb", variable="denit", unit=1),
                                          no3=define_output(file="basin_sd_cha", variable="no3_out", unit=1)))
