# Simulation_Correlation
# Generates model inputs with correlations and simulates SWAT+

# Call external libraries needed in this script
library(EnvStats) # For random number generation from truncated distribution
library(SWATplusR) # For running SWATplus and associated functions
library(dplyr) # For using the SWATplus output
options(tibble.width=Inf)
options(tibble.length=Inf)
library(DBI)
library(RSQLite)
library(stringr)

path <- "Y:\\DanielBuhr\\Sensitivity_Analysis_Parameter_Ranges\\SWAT+"
setwd(path)

mytime <- format(base::Sys.time(), "%m%d%Y_%H%M") # adds file nomenclature for date (mmmddYYYY_HrMin)

# Create a new directory for this run
dir.create(paste0(path, "\\SensAnalysis_", mytime))
filepath <- paste0(path, "\\SensAnalysis_", mytime)
# Copy all the TxtInOut files from the original LREW model
TxtInOut <- list.files(paste0(path,"\\LREW_TxtInOut2", full.names=TRUE))
Filter_TxtInOut <- TxtInOut[!grepl('\\.sqlite', TxtInOut)]
file.copy(Filter_TxtInOut, filepath)

# Create vectors for outputs
wtd_sd_3 <- vector(mode="double", length=n_sims)
denit_sd_3 <- vector(mode="double", length=n_sims)
no3_sd_3 <- vector(mode="double", length=n_sims)
n_sims <- 510 # Set high and trim down to 500 after errors are removed


# Generate parameter set
par_set_3 <- tibble("nperco.bsn|change = pctchg" = runif(n_sims, -99,400),
                    "cdn.bsn|change = pctchg" = runif(n_sims, -99, 100),
                    "sdnco.bsn|change = pctchg" = runif(n_sims,-99,100),
                    "z.sol|change = pctchg" = rlnormTrunc(n_sims, meanlog=3.98, sdlog=0.29, min=1, max=200),
                    "bd.sol|change = pctchg" = rnormTrunc(n_sims, mean=30, sd=35, min=-40, max=100),
                    "awc.sol|change = pctchg" = rnormTrunc(n_sims, mean=200, sd=150, min=-99, max=500),
                    "k.sol|change = pctchg" = rnormTrunc(n_sims, mean=50, sd=75, min=-99, max=200),
                    "cbn.sol|change = pctchg" = runif(n_sims, min=-86, max=500),
                    "clay.sol|change = pctchg" = runif(n_sims, min=-99, max=100),
                    "ph.sol|change = pctchg" = rnormTrunc(n_sims, mean=30, sd=35, min=-40, max=100),
                    "d.rte|change = pctchg" = rlnormTrunc(n_sims, meanlog=4.31, sdlog=1.41, min=40, max=2100),
                    "dep_bot.aqu|change = absval" = rnormTrunc(n_sims, mean=6.5, sd=1.75, min=3, max=10),
                    "bf_max.aqu|change = absval" = rnormTrunc(n_sims, mean=1, sd=0.5, min=0, max=2),
                    "flo_min.aqu|change = absval" = rnormTrunc(n_sims, mean=5, sd=2.475, min=0.1, max=10),
                    "slope.hru|change = pctchg" = rlnormTrunc(n_sims, meanlog=4.14, sdlog=0.6, min=4, max=200),
                    "slope_len.hru|change = absval" = rlnormTrunc(n_sims, meanlog=3.62, sdlog=1.40, min=10, max=1000),
                    "lat_len.hru|change = pctchg" =rlnormTrunc(n_sims, meanlog=4.61, sdlog=0.43, min=10, max=200),
                    "field_wid.hru|change = absval" = rlnormTrunc(n_sims, meanlog=4.20, sdlog=0.90, min=50, max=500),
                    "field_len.hru|change=absval" = rlnormTrunc(n_sims, meanlog=6.12, sdlog=0.43, min=100, max=1000)
)

# Translate log-transform values back to percentages to allow neg values
y <- 1
while (y <= n_sims) {
  par_set_3[[y,4]] <- (par_set_3[[y,4]]-100) # z
  par_set_3[[y,11]] <- (par_set_3[[y,11]]-100) # d
  par_set_3[[y,15]] <- (par_set_3[[y,15]]-100) # slope
  par_set_3[[y,17]] <- (par_set_3[[y,17]]-100) # lat len
  y <- y+1
}

# Account for some correlation
z <- 1
while (z <= n_sims) {
  
  # Clay percentage to dictate bulk density and hyd conductivity
  # Inverse relationships-- increase in clay % decreases bd and k
  if (par_set_3[[z,9]]>=50) {
    par_set_3[[z,5]] <- rnormTrunc(1, mean=30, sd=35, min=-40, max=-20) # bd
    par_set_3[[z,7]] <- rnormTrunc(1, mean=50, sd=75, min=-99, max=-50) # k
  }
  if (par_set_3[[z,9]]>0 & par_set_3[[z,9]]<50) {
    par_set_3[[z,5]] <- rnormTrunc(1, mean=30, sd=35, min=-20, max=0) # bd
    par_set_3[[z,7]] <- rnormTrunc(1, mean=50, sd=75, min=-50, max=0) # k
  }
  if (par_set_3[[z,9]]<0 & par_set_3[[z,9]]>= -50) {
    par_set_3[[z,5]] <- rnormTrunc(1, mean=30, sd=35, min=0, max=50)
    par_set_3[[z,7]] <- rnormTrunc(1, mean=50, sd=75, min=0, max=100) # k
  }
  if (par_set_3[[z,9]]< -50 & par_set_3[[z,9]]> -100) {
    par_set_3[[z,5]] <- rnormTrunc(1, mean=30, sd=35, min=50, max=100)
    par_set_3[[z,7]] <- rnormTrunc(1, mean=50, sd=75, min=100, max=200) # k
  }
  
  # Slope, slope_len, and lat_len
  # Slope and slope_len are inversely related; slope_len and lat_len are directly related
  # Make correlations relative to mean slope_len of 100 CHANGED IN ThIS RUN
  if (par_set_3[[z,16]] > 100 & par_set_3[[z,16]] < 347.5) {
    par_set_3[[z,15]] <- (rlnormTrunc(1, meanlog=4.14, sdlog=0.6, min=50, max=100)-100) # slope
    par_set_3[[z,17]] <- (rlnormTrunc(1, meanlog=4.61, sdlog=0.43, min=100, max=150)-100) # lat_len
  }
  if (par_set_3[[z,16]] >= 347.5) {
    par_set_3[[z,15]] <- (rlnormTrunc(1, meanlog=4.14, sdlog=0.6, min=4, max=50)-100) # slope
    par_set_3[[z,17]] <- (rlnormTrunc(1, meanlog=4.61, sdlog=0.43, min=150, max=200)-100) # lat_len
  }
  if (par_set_3[[z,16]] <= 100) {
    par_set_3[[z,15]] <- (rlnormTrunc(1, meanlog=4.14, sdlog=0.6, min=100, max=200)-100) # slope
    par_set_3[[z,17]] <- (rlnormTrunc(1, meanlog=4.61, sdlog=0.43, min=10, max=100)-100) # lat_len
  }
  
  z <- z+1
}

# Run SWAT+ within R
threads <-5

swatplus_run_3 <- run_swatplus(paste0(path, "\\SensAnalysis_", mytime), 
                               output=list(wtd = define_output(file="basin_aqu", variable="dep_wt", unit=1249),
                                           denit=define_output(file="basin_nb", variable="denit", unit=1),
                                           no3=define_output(file="basin_sd_cha", variable="no3_out", unit=1)),
                               save_path = filepath,
                               parameter=par_set_3,
                               add_parameter= TRUE, # changed 
                               n_thread = threads) 


# Extract outputs

# If all simulations run properly
wtd_3<- as.data.frame(swatplus_run_3$simulation$wtd) #Output in m
no3_3 <- as.data.frame(swatplus_run_3$simulation$no3) # Output in kgN
denit_3 <- as.data.frame(swatplus_run_3$simulation$denit) # Output in kg/ha


i <- 1 # simulation index
while (i <= n_sims) {
  if (i < 10) {
    run <- (paste0("run_0", i))
  }
  if (i >= 10) {
    run <- (paste0("run_",i))
  }
  
  wtd_sd_3[i] <- sd(wtd_3[,i+1])
  no3_sd_3[i] <- sd(no3_3[,i+1])
  denit_sd_3[i] <- sd(denit_3[,i+1])
  
  i <- i+1
}


# When there is an error in at least one simulation, causing outputs to not be generated
t <- 1
while (t <= threads) {
  
  con<-dbConnect(SQLite(),paste0(filepath, "\\sim_thread_",t,".sqlite"))
  tables <- as.data.frame(dbListTables(con))
  tables 
  i <- 1
  n <- nrow(tables)
  n
  
  while (i <=n) {
    table_df <-read.table(text = as.character(tables[,1]), sep ='_', col.names = c('Name', 'Iter'))
    num <- as.numeric(as.character(table_df[i,2])) # some values are stat# that will produce warnings
    dataset <- dbReadTable(con, as.character(tables[i,1]))
    
    if (grepl('denit$$from$$run_', as.character(tables[i,1]), fixed=TRUE)) {
      denit_sd_3[num] <- sd(dataset[,1])
    }
    if (grepl('no3$$from$$run_', as.character(tables[i,1]), fixed=TRUE)) {
      no3_sd_3[num] <- sd(dataset[,1])
    }
    if (grepl('wtd$$from$$run_', as.character(tables[i,1]), fixed=TRUE)) {
      wtd_sd_3[num] <- sd(dataset[,1])
    }
    i <- i+1
  }
  t <- t+1
}

# Create a dummy variable as a baseline of model sensitivity
dummy_3 <- matrix(nrow = n_sims, ncol = 1)
dummy_3 <- runif(n_sims, 0, 100)
parameters <- read.csv(paste0(path, "\\Parameters_2.csv"), header=TRUE)

par_set_mat_3 <- as.matrix(par_set_3)
colnames(par_set_mat_3) <- parameters[,2]
inputs_3 <-cbind(par_set_mat_3, dummy_3)