# Plots

library("RColorBrewer")

#
# Create 4-panel plot for Plischke, log+1

#These are your sensitivity outputs for each of your model inputs
WTD <- read.table(paste0("Sensitivity_Output_", mytime, "_WTD_rev2_log1.txt"), header = TRUE)
subdisNO3 <- read.table(paste0("Sensitivity_Output_", mytime, "_subdisNO3_rev2_log1.txt"), header = TRUE)
denitr1 <- read.table(paste0("Sensitivity_Output_", mytime, "_denitr1_rev2_log1.txt"), header = TRUE)
TN2 <- read.table(paste0("Sensitivity_Output_", mytime, "_TN2_rev2_log1.txt"), header = TRUE)
j <- 1

#Set input variable names for plot. Getting them from the column names but can also
#write them in manually
names <- colnames(WTD[1:(ncol(data[[j]])-1)])
names <- gsub("_", " ", names)

#Get values of 1 to the number of input variables
y <- 1:(ncol(WTD)-1)

#Names for each sub-plot
output_names <- c("Water Table Depth", "Subsurface Dissolved Nitrate", "Total Denitrification", "Total Nitrogen")
#The "png" function prints a .png image of your plot. You can change the file name and
#the height and width
png(paste0("Full Sensitivity plot summary_", mytime, "_col3_log1_rev2.png"), type = "cairo", units = "in",
    height = 6.5, width = 10.5, res = 500)

#sets margins. mfrow = c(1,4) means 1 row and 4 columns of plots
#You may need to adjust the left outer margin (the second value in oma)
#to fit your input variable names
par(mfrow = c(1,4), mar = c(2, 1, 1.5, 0.5), oma = c(2,10.5,0.5,0)) 

#loads the plot functions R script - change the path name
source("Y:/DanielBuhr/Sensitivity_Analysis_Parameter_Ranges/Plot Functions.R") 

#combines the data (sensitivity outputs) into a list
data <- list(WTD, subdisNO3, denitr1, TN)
xmax <- max(sapply(data, max))

#Create list of colors for plot
palette <- brewer.pal(4, "Dark2")

#Loops through list, for each output, creates a plot
for (j in 1:4){
  
  #Get the median of the dummy value - plots this as a line
  dummy_val <- data[[j]][3, "dummy"]
  
  #Creates plot
  plot(NA, xlim = c(0, xmax), ylim = c(1, max(y)), yaxt = "n", 
       xlab = "", ylab = "", main = output_names[j], cex.axis=1.2, cex.main=1.21)
  
  #Adds dummy variable line
  abline(v = dummy_val, lty = 2, col = "gray60", lwd = 2)
  
  #Adds error bars
  arrows(x0 = as.numeric(data[[j]][1,1:(ncol(data[[j]])-1)]), y0 = y, 
         x1 = as.numeric(data[[j]][5,1:(ncol(data[[j]])-1)]),
         y1 = y, length = 0)
  
  #Adds points
  #points(data[[j]][3,1:(ncol(data[[j]])-1)], y, pch = 21, bg = "gray60", cex = 1.5)
  if (j==1) { # WTD
    #Highly sensitive
    points(data[[j]][3,12], y[12], pch = 21, bg = palette[2], cex = 1.7)
    
    #Slightly sensitive
    points(data[[j]][3,c(1,5)], y[c(1,5)], pch = 22, bg = palette[1], cex = 1.7)
    points(data[[j]][3,c(13,14)], y[c(13,14)], pch = 22, bg = palette[2], cex = 1.7)
    
    
    #Not sensitive
    points(data[[j]][3,c(2:4,6:7)], y[c(2:4,6:7)], pch = 24, bg = palette[1], cex = 1.7)
    points(data[[j]][3,c(8:11, 15:20)], y[c(8:11, 15:20)], pch = 24, bg = palette[2], cex = 1.7)
    points(data[[j]][3,21:35], y[21:35], pch = 24, bg = palette[3], cex = 1.7)
    points(data[[j]][3,36:47], y[36:47], pch = 24, bg = palette[4], cex = 1.7)
  }
  
  if (j==2) {
    #Highly sensitive
    points(data[[j]][3,17:18], y[17:18], pch = 21, bg = palette[2], cex = 1.7)
    
    #Slightly sensitive
    points(data[[j]][3,5], y[5], pch = 22, bg = palette[1], cex = 1.7)
    points(data[[j]][3,19], y[19], pch = 22, bg = palette[2], cex = 1.7)
    
    #Not sensitive
    points(data[[j]][3,c(1:4, 6:7)], y[c(1:4, 6:7)], pch = 24, bg = palette[1], cex = 1.7)
    points(data[[j]][3,c(8:16, 20)], y[c(8:16, 20)], pch = 24, bg = palette[2], cex = 1.7)
    points(data[[j]][3,21:35], y[21:35], pch = 24, bg = palette[3], cex = 1.7)
    points(data[[j]][3,36:47], y[36:47], pch = 24, bg = palette[4], cex = 1.7)
  }
  
  if (j==3) { # Denitro
    #Highly sensitive
    points(data[[j]][3,12], y[12], pch = 21, bg = palette[2], cex = 1.7)
    points(data[[j]][3,41], y[41], pch = 21, bg = palette[4], cex = 1.7)
    
    #Slightly sensitive
    points(data[[j]][3,1:5], y[1:5], pch = 22, bg = palette[1], cex = 1.7)
    points(data[[j]][3,c(8:10, 13, 17:19)], y[c(8:10, 13, 17:19)], pch = 22, bg = palette[2], cex = 1.7)
    points(data[[j]][3,c(23,26,28,30,32,34)], y[c(23,26,28,30,32,34)], pch = 22, bg = palette[3], cex = 1.7)
    points(data[[j]][3,c(37,47)], y[c(37,47)], pch = 22, bg = palette[4], cex = 1.7)
    
    #Not sensitive
    points(data[[j]][3,6:7], y[6:7], pch = 24, bg = palette[1], cex = 1.7)
    points(data[[j]][3,c(11,14:16,20)], y[c(11,14:16,20)], pch = 24, bg = palette[2], cex = 1.7)
    points(data[[j]][3,c(21:22, 24:25,27,29,31,33,35)], y[c(21:22, 24:25,27,29,31,33,35)], pch = 24, bg = palette[3], cex = 1.7)
    points(data[[j]][3,c(36, 38:40, 42:46)], y[c(36, 38:40, 42:46)], pch = 24, bg = palette[4], cex = 1.7)
  }
  
  if (j==4) { TN2
    #Highly sensitive
    points(data[[j]][3,c(1,5)], y[c(1,5)], pch = 21, bg = palette[1], cex = 1.7)
    
    #Slightly sensitive
    points(data[[j]][3,c(9,12,14,17)], y[c(9,12,14,17)], pch = 22, bg = palette[2], cex = 1.7)
    points(data[[j]][3,c(29,34)], y[c(29,34)], pch = 22, bg = palette[3], cex = 1.7)
    
    #Not sensitive
    points(data[[j]][3,c(2:4,6:7)], y[c(2:4,6:7)], pch = 24, bg = palette[1], cex = 1.7)
    points(data[[j]][3,c(8,10:11,13,15:16,18:20)], y[c(8,10:11,13,15:16,18:20)], pch = 24, bg = palette[2], cex = 1.7)
    points(data[[j]][3,c(21:28,30:33,35)], y[c(21:28,30:33,35)], pch = 24, bg = palette[3], cex = 1.7)
    points(data[[j]][3,36:47], y[36:47], pch = 24, bg = palette[4], cex = 1.7)
  }
  
  #Adds axes with labels (first plot only)
  if (j == 1){
    axis(side = 2, at = y[1:7], labels = names[1:7], las = 1, cex.axis = 1,
         mgp = c(3, 0.8, 0), col.axis=palette[1])
    axis(side = 2, at = y[8:20], labels = names[8:20], las = 1, cex.axis = 1,
         mgp = c(3, 0.8, 0), col.axis=palette[2])
    axis(side = 2, at = y[21:35], labels = names[21:35], las = 1, cex.axis = 1,
         mgp = c(3, 0.8, 0), col.axis=palette[3])
    axis(side = 2, at = y[36:47], labels = names[36:47], las = 1, cex.axis = 1,
         mgp = c(3, 0.8, 0), col.axis=palette[4])
  }else{
    axis(side = 2, at = y, labels = FALSE, las = 1)
  }
  
  #Adds (a), (b), etc. labels to plots
  add_label(-0.08, -0.03, paste0("(", letters[j], ")"), cex=1.3)
  
  if (j==4) {
    legend(x="topright", legend=c("Buffer", "Soil", "Vegetation", "Rate"), pch=21, bty="n", pt.bg= palette, cex=1.2, pt.cex=1.4, title="Parameter Type")
    legend(x=0.25, y=40, legend=c("Highly", "Slightly", "Not"), bty="n", pch=c(21,22,24), pt.bg= palette[1], cex=1.2, pt.cex=1.4)
    text(x=0.325, y=40.1, "Sensitivity", cex=1.2)
  }
}
mtext("Sensitivity Index", side = 1, outer = TRUE, line = 0.5)
dev.off()

#
# Repeat 4-panel plot for Plischke, log+1; select parameters only
# Choose select parameters to include
condensed <- c(1:5,8:10,12:15,17:20, 45, 47, 23,25,28,30,32,35,41,48)

WTD_condensed <- WTD[,condensed]
subdisNO3_condensed <- subdisNO3[,condensed]
denitr1_condensed <- denitr1[,condensed]
TN2_condensed <- TN2[,condensed]

j <- 1
#Set input variable names for plot. Getting them from the column names but can also
#write them in manually
names <- colnames(WTD_condensed[1:(ncol(data[[j]])-1)])
names <- gsub("_", " ", names)

#Get values of 1 to the number of input variables
y <- 1:(ncol(WTD_condensed)-1)

#Names for each sub-plot
output_names <- c("Water Table Depth", "Subsurface Dissolved Nitrate", "Total Denitrification", "Total Nitrogen")
#The "png" function prints a .png image of your plot. You can change the file name and
#the height and width
png(paste0("Sensitivity plot summary_rev2_", mytime, "_col3_log1_rev2.png"), type = "cairo", units = "in",
    height = 6.5, width = 10.5, res = 500)

#sets margins. mfrow = c(1,4) means 1 row and 4 columns of plots
#You may need to adjust the left outer margin (the second value in oma)
#to fit your input variable names
par(mfrow = c(1,4), mar = c(2, 1, 1.5, 0.5), oma = c(2,10.5,0.5,0)) 

#loads the plot functions R script - change the path name
source("Y:/DanielBuhr/Sensitivity_Analysis_Parameter_Ranges/Plot Functions.R") 

#combines the data (sensitivity outputs) into a list
#data <- list(WTD, subdisNO3, NO3GW1, denitr1)
data <- list(WTD_condensed, subdisNO3_condensed, denitr1_condensed, TN_condensed)
xmax <- max(sapply(data, max))

#Create list of colors for plot
palette <- brewer.pal(4, "Dark2")

#Loops through list, for each output, creates a plot
for (j in 1:4){
  
  #Get the median of the dummy value - plots this as a line
  dummy_val <- data[[j]][3, "dummy"]
  
  #Creates plot
  plot(NA, xlim = c(0, xmax), ylim = c(1, max(y)), yaxt = "n", 
       xlab = "", ylab = "", main = output_names[j], cex.axis=1.2, cex.main=1.21)
  
  #Adds dummy variable line
  abline(v = dummy_val, lty = 2, col = "gray60", lwd = 2)
  
  #Adds error bars
  arrows(x0 = as.numeric(data[[j]][1,1:(ncol(data[[j]])-1)]), y0 = y, 
         x1 = as.numeric(data[[j]][5,1:(ncol(data[[j]])-1)]),
         y1 = y, length = 0)
  
  #Adds points
  #Manually distinguished between highly, slightly, not significant based on result output
  if (j==1) { # WTD plot
    # Highly significant
    points(data[[j]][3,9], y[9], pch = 21, bg = palette[2], cex = 1.8)
    
    # Slightly significant
    points(data[[j]][3,c(1,5)], y[c(1,5)], pch = 22, bg = palette[1], cex = 1.8)
    points(data[[j]][3,c(10,11)], y[c(10,11)], pch = 22, bg = palette[2], cex = 1.8)
    
    # Not significant
    points(data[[j]][3,c(2:4)], y[2:4], pch = 24, bg = palette[1], cex = 1.8)
    points(data[[j]][3,c(6:8, 12:18)], y[c(6:8, 12:18)], pch = 24, bg = palette[2], cex = 1.8)
    points(data[[j]][3,19:24], y[19:24], pch = 24, bg = palette[3], cex = 1.8)
    points(data[[j]][3,25], y[25], pch = 24, bg = palette[4], cex = 1.8)
    
  }
  
  if (j==2) {# Subsurface dissolved NO3 plot
    # Highly significant
    points(data[[j]][3,13:14], y[13:14], pch = 21, bg = palette[2], cex = 1.8)
    
    # Slightly significant
    points(data[[j]][3,5], y[5], pch = 22, bg = palette[1], cex = 1.8)
    points(data[[j]][3,15], y[15], pch = 22, bg = palette[2], cex = 1.8)
    
    # Not significant
    points(data[[j]][3,1:4], y[1:4], pch = 24, bg = palette[1], cex = 1.8)
    points(data[[j]][3,c(6:12, 16:18)], y[c(6:12, 16:18)], pch = 24, bg = palette[2], cex = 1.8)
    points(data[[j]][3,19:24], y[19:24], pch = 24, bg = palette[3], cex = 1.8)
    points(data[[j]][3,25], y[25], pch = 24, bg = palette[4], cex = 1.8)
  }
  
  if (j==3) {# Denitrification plot
    # Highly significant
    points(data[[j]][3,9], y[9], pch = 21, bg = palette[2], cex = 1.8)
    points(data[[j]][3,25], y[25], pch = 21, bg = palette[4], cex = 1.8)
    
    # Slightly significant
    points(data[[j]][3,1:5], y[1:5], pch = 22, bg = palette[1], cex = 1.8)
    points(data[[j]][3,c(6:8, 10, 13:15, 18)], y[c(6:8, 10, 13:15, 18)], pch = 22, bg = palette[2], cex = 1.8)
    points(data[[j]][3,c(19,21:23)], y[c(19,21:23)], pch = 22, bg = palette[3], cex = 1.8)
    
    # Not significant
    points(data[[j]][3,c(11,12,16,17)], y[c(11,12,16,17)], pch = 24, bg = palette[2], cex = 1.8)
    points(data[[j]][3,c(20,24)], y[c(20,24)], pch = 24, bg = palette[3], cex = 1.8)
    
  }
  if (j==4) {# TN2 plot
    # Highly significant
    points(data[[j]][3,c(1,5)], y[c(1,5)], pch = 21, bg = palette[1], cex = 1.8)
    
    # Slightly significant
    points(data[[j]][3,c(7,9,11,13)], y[c(7,9,11,13)], pch = 22, bg = palette[2], cex = 1.8)
    
    # Not significant
    points(data[[j]][3,2:4], y[2:4], pch = 24, bg = palette[1], cex = 1.8)
    points(data[[j]][3,c(6,8,10,12,14:18)], y[c(6,8,10,12,14:18)], pch = 24, bg = palette[2], cex = 1.8)
    points(data[[j]][3,19:24], y[19:24], pch = 24, bg = palette[3], cex = 1.8)
    points(data[[j]][3,25], y[25], pch = 24, bg = palette[4], cex = 1.8)
  }

  #Adds axes with labels (first plot only)
  if (j == 1){
    axis(side = 2, at = y[1:5], labels = names[1:5], las = 1, cex.axis = 1.4,
         mgp = c(3, 0.8, 0), col.axis=palette[1])
    axis(side = 2, at = y[6:18], labels = names[6:18], las = 1, cex.axis = 1.4,
         mgp = c(3, 0.8, 0), col.axis=palette[2])
    axis(side = 2, at = y[19:24], labels = names[19:24], las = 1, cex.axis = 1.4,
         mgp = c(3, 0.8, 0), col.axis=palette[3])
    axis(side = 2, at = y[25], labels = names[25], las = 1, cex.axis = 1.4,
         mgp = c(3, 0.8, 0), col.axis=palette[4])
  }else{
    axis(side = 2, at = y, labels = FALSE, las = 1)
  }
  
  #Adds (a), (b), etc. labels to plots
  add_label(-0.08, -0.03, paste0("(", letters[j], ")"), cex=1.3)
  
  if (j==4) {
    legend(x="topright", legend=c("Buffer", "Soil", "Vegetation", "Rate"), pch=21, bty="n", pt.bg= palette, cex=1.2, pt.cex=1.4, title="Parameter Type")
    legend(x=0.25, y=21.3, legend=c("Highly", "Slightly", "Not"), bty="n", pch=c(21,22,24), pt.bg= palette[1], cex=1.2, pt.cex=1.4)
    text(x=0.325, y=21.4, "Sensitivity", cex=1.2)
  }
}
mtext("Sensitivity Index", side = 1, outer = TRUE, line = 0.5)
dev.off()

#
# Create plots to show sensitivity for most sensitive parameters across various time steps
# Generate matrices for each time-varying analyses that will be combined outside the loop
WTD_sens_full_summary_rev2 <- data.frame(matrix(,nrow=(ncol(WTD_sens_log1_500_rev2[["boot"]])-1), ncol=6))
denit_sens_full_summary_rev2 <- data.frame(matrix(,nrow=(ncol(denitr1_sens_log1_500_sum[["boot"]])-1), ncol=6))
TN2_sens_full_summary_rev2 <- data.frame(matrix(,nrow=(ncol(TN2_sens_log1_500_sum[["boot"]])-1), ncol=6))
WTD_sens_yr_summary_rev2 <- data.frame(matrix(,nrow=(n_yr*(ncol(WTD_sens_log1_500_rev2[["boot"]])-1)), ncol=6))
denit_sens_yr_summary_rev2 <- data.frame(matrix(,nrow=(n_yr*(ncol(WTD_sens_log1_500_rev2[["boot"]])-1)), ncol=6))
TN2_sens_yr_summary_rev2 <- data.frame(matrix(,nrow=(n_yr*(ncol(TN2_sens_log1_500_sum[["boot"]])-1)), ncol=6))
WTD_sens_seas_summary_rev2 <- data.frame(matrix(,nrow=(n_seas_rev3*(ncol(WTD_sens_log1_500_rev2[["boot"]])-1)), ncol=6))
denit_sens_seas_summary_rev2 <- data.frame(matrix(,nrow=(n_seas_rev3*(ncol(WTD_sens_log1_500_rev2[["boot"]])-1)), ncol=6))
TN2_sens_seas_summary_rev2 <- data.frame(matrix(,nrow=(n_seas_rev3*(ncol(TN2_sens_log1_500_sum[["boot"]])-1)), ncol=6))
WTD_sens_30_summary_rev2 <- data.frame(matrix(,nrow=(n_30*(ncol(WTD_sens_log1_500_rev2[["boot"]])-1)), ncol=6))
denit_sens_30_summary_rev2 <- data.frame(matrix(,nrow=(n_30*(ncol(WTD_sens_log1_500_rev2[["boot"]])-1)), ncol=6))
TN2_sens_30_summary_rev2 <- data.frame(matrix(,nrow=(n_30*(ncol(TN2_sens_log1_500_sum[["boot"]])-1)), ncol=6))

# Loop to fill those matrices
# Columns are: parameter name, Window type, window #, Sensitivity, sensitivity relative to dummy max

for (i in 1:(ncol(sens_log1_WTD_30_rev2[[1]])-1)) { # Each parameter
  
  # Full sim
  # Parameter names, column 1
  WTD_sens_full_summary_rev2[i, 1] <- colnames(WTD_sens_log1_500_rev2[["boot"]])[i]
  denit_sens_full_summary_rev2[i, 1] <- colnames(denitr1_sens_log1_500_sum[["boot"]])[i]
  TN2_sens_full_summary_rev2[i, 1] <- colnames(TN2_sens_log1_500_sum[["boot"]])[i]
  
  # Window type, column 2
  WTD_sens_full_summary_rev2[i, 2] <- "Full"
  denit_sens_full_summary_rev2[i, 2] <- "Full"
  TN2_sens_full_summary_rev2[i, 2] <- "Full"
  
  # Window number, column 3
  WTD_sens_full_summary_rev2[i, 3] <- 1
  denit_sens_full_summary_rev2[i, 3] <- 1
  TN2_sens_full_summary_rev2[i, 3] <- 1
  
  # Parameter Min Sensitivity, column 4
  WTD_sens_full_summary_rev2[i, 4] <- WTD_sens_log1_500_rev2[["boot"]][5,i]
  denit_sens_full_summary_rev2[i, 4] <- denitr1_sens_log1_500_sum[["boot"]][5,i]
  TN2_sens_full_summary_rev2[i, 4] <- TN2_sens_log1_500_sum[["boot"]][5,i]
  
  # Min Sensitivity relative to dummy, max; column 5
  WTD_sens_full_summary_rev2[i, 5] <- WTD_sens_log1_500_rev2[["boot"]][5,i]-WTD_sens_log1_500_rev2[["boot"]][236]
  denit_sens_full_summary_rev2[i, 5] <- denitr1_sens_log1_500_sum[["boot"]][5,i]-denitr1_sens_log1_500_sum[["boot"]][236]
  TN2_sens_full_summary_rev2[i, 5] <- TN2_sens_log1_500_sum[["boot"]][5,i]-TN2_sens_log1_500_sum[["boot"]][236]
  
  # Test for overlap in interquartile sensitivity; column 6
  WTD_sens_full_summary_rev2[i, 6] <- WTD_sens_log1_500_rev2[["boot"]][4,i]-WTD_sens_log1_500_rev2[["boot"]][237]
  denit_sens_full_summary_rev2[i, 6] <- denitr1_sens_log1_500_sum[["boot"]][4,i]-denitr1_sens_log1_500_sum[["boot"]][237]
  TN2_sens_full_summary_rev2[i, 6] <- TN2_sens_log1_500_sum[["boot"]][4,i]-TN2_sens_log1_500_sum[["boot"]][237]
  
  # Year analyses
  for (j in 1:n_yr) { # Each window
    
    # Parameter names, column 1
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 1] <- colnames(sens_log1_WTD_yr_rev2[[1]])[i]
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 1] <- colnames(sens_log1_denitr_yr_rev2[[1]])[i]
    TN2_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 1] <- colnames(sens_log1_TN2_yr_rev2[[1]])[i]
    
    # Window type, column 2
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 2] <- "Year"
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 2] <- "Year"
    TN2_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 2] <- "Year"
    
    # Window number, column 3
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 3] <- j
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 3] <- j
    TN2_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 3] <- j
    
    # Parameter Min Sensitivity, column 4
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 4] <- sens_log1_WTD_yr_rev2[[j]][5,i]
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 4] <- sens_log1_denitr_yr_rev2[[j]][5,i]
    TN2_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 4] <- sens_log1_TN2_yr_rev2[[j]][5,i]
    
    # Min Sensitivity relative to dummy, max; column 5
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 5] <- sens_log1_WTD_yr_rev2[[j]][5,i]-sens_log1_WTD_yr_rev2[[j]][236]
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 5] <- sens_log1_denitr_yr_rev2[[j]][5,i]-sens_log1_denitr_yr_rev2[[j]][236]
    TN2_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 5] <- sens_log1_TN2_yr_rev2[[j]][5,i]-sens_log1_TN2_yr_rev2[[j]][236]
    
    # Test for overlap in interquartile sensitivity; column 6
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 6] <- sens_log1_WTD_yr_rev2[[j]][4,i]-sens_log1_WTD_yr_rev2[[j]][237]
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 6] <- sens_log1_denitr_yr_rev2[[j]][4,i]-sens_log1_denitr_yr_rev2[[j]][237]
    TN2_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 6] <- sens_log1_TN2_yr_rev2[[j]][4,i]-sens_log1_TN2_yr_rev2[[j]][237]
  }
  
  # Seasonal analyses
  for (j in 1:n_seas_rev2) { # Each window
    
    # Parameter names, column 1
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 1] <- colnames(sens_log1_WTD_seas_rev2[[1]])[i]
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 1] <- colnames(sens_log1_denitr_seas_rev2[[1]])[i]
    TN2_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 1] <- colnames(sens_log1_TN2_seas_rev2[[1]])[i]
    
    # Window type, column 2
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 2] <- "Season"
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 2] <- "Season"
    TN2_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 2] <- "Season"
    
    # Window number, column 3
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 3] <- j
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 3] <- j
    TN2_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 3] <- j
    
    # Parameter Min Sensitivity, column 4
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 4] <- sens_log1_WTD_seas_rev2[[j]][5,i]
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 4] <- sens_log1_denitr_seas_rev2[[j]][5,i]
    TN2_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 4] <- sens_log1_TN2_seas_rev2[[j]][5,i]
    
    # Min Sensitivity relative to dummy, max; column 5
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 5] <- sens_log1_WTD_seas_rev2[[j]][5,i]-sens_log1_WTD_seas_rev2[[j]][236]
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 5] <- sens_log1_denitr_seas_rev2[[j]][5,i]-sens_log1_denitr_seas_rev2[[j]][236]
    TN2_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 5] <- sens_log1_TN2_seas_rev2[[j]][5,i]-sens_log1_TN2_seas_rev2[[j]][236]
    
    # Test for overlap in interquartile sensitivity; column 6
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 6] <- sens_log1_WTD_seas_rev2[[j]][4,i]-sens_log1_WTD_seas_rev2[[j]][237]
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 6] <- sens_log1_denitr_seas_rev2[[j]][4,i]-sens_log1_denitr_seas_rev2[[j]][237]
    TN2_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 6] <- sens_log1_TN2_seas_rev2[[j]][4,i]-sens_log1_TN2_seas_rev2[[j]][237]
  }
  
  # 30-day analyses
  for (j in 1:n_30) { # Each window
    
    # Parameter names, column 1
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 1] <- colnames(sens_log1_WTD_30_rev2[[1]])[i]
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 1] <- colnames(sens_log1_denitr_30_rev2[[1]])[i]
    TN2_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 1] <- colnames(sens_log1_TN2_30_rev2[[1]])[i]
    
    # Window type, column 2
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 2] <- 30
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 2] <- 30
    TN2_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 2] <- 30
    
    # Window number, column 3
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 3] <- j
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 3] <- j
    TN2_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 3] <- j
    
    # Parameter Min Sensitivity, column 4
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 4] <- sens_log1_WTD_30_rev2[[j]][5,i]
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 4] <- sens_log1_denitr_30_rev2[[j]][5,i]
    TN2_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 4] <- sens_log1_TN2_30_rev2[[j]][5,i]
    
    # Sensitivity relative to dummy, max; column 5
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 5] <- sens_log1_WTD_30_rev2[[j]][5,i]-sens_log1_WTD_30_rev2[[j]][236]
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 5] <- sens_log1_denitr_30_rev2[[j]][5,i]-sens_log1_denitr_30_rev2[[j]][236]
    TN2_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 5] <- sens_log1_TN2_30_rev2[[j]][5,i]-sens_log1_TN2_30_rev2[[j]][236]
    
    # Test for overlap in interquartile sensitivity; column 6
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 6] <- sens_log1_WTD_30_rev2[[j]][4,i]-sens_log1_WTD_30_rev2[[j]][237]
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 6] <- sens_log1_denitr_30_rev2[[j]][4,i]-sens_log1_denitr_30_rev2[[j]][237]
    TN2_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 6] <- sens_log1_TN2_30_rev2[[j]][4,i]-sens_log1_TN2_30_rev2[[j]][237]
  }
}

# Add color info
cRamp <- function(x, palette, alpha = 1){
  
  range <- (x-min(x))/diff(range(c(x, max(x))))
  
  cols <- suppressWarnings(colorRamp(RColorBrewer::brewer.pal(11, palette))(range))
  cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
  cols <- adjustcolor(cols, alpha.f = alpha)
  
  return(cols)
}

WTD_sens_yr_summary_rev2$Color <- cRamp(c(WTD_sens_yr_summary_rev2$X4,0,0.3), "Greens")[1:nrow(WTD_sens_yr_summary_rev2)]
WTD_sens_seas_summary_rev2$Color<- cRamp(c(WTD_sens_seas_summary_rev2$X4,0,0.3), "Oranges")[1:nrow(WTD_sens_seas_summary_rev2)]
WTD_sens_30_summary_rev2$Color <- cRamp(c(WTD_sens_30_summary_rev2$X4,0,0.3), "Purples")[1:nrow(WTD_sens_30_summary_rev2)]
denit_sens_yr_summary_rev2$Color <- cRamp(c(denit_sens_yr_summary_rev2$X4,0,0.3), "Greens")[1:nrow(denit_sens_yr_summary_rev2)]
denit_sens_seas_summary_rev2$Color<- cRamp(c(denit_sens_seas_summary_rev2$X4,0,0.3), "Oranges")[1:nrow(denit_sens_seas_summary_rev2)]
denit_sens_30_summary_rev2$Color <- cRamp(c(denit_sens_30_summary_rev2$X4,0,0.3), "Purples")[1:nrow(denit_sens_30_summary_rev2)]
TN2_sens_yr_summary_rev2$Color <- cRamp(c(TN2_sens_yr_summary_rev2$X4,0,0.5), "Greens")[1:nrow(TN2_sens_yr_summary_rev2)]
TN2_sens_seas_summary_rev2$Color<- cRamp(c(TN2_sens_seas_summary_rev2$X4,0,0.5), "Oranges")[1:nrow(TN2_sens_seas_summary_rev2)]
TN2_sens_30_summary_rev2$Color <- cRamp(c(TN2_sens_30_summary_rev2$X4,0,0.5), "Purples")[1:nrow(TN2_sens_30_summary_rev2)]


# Generate empty matrices with parameter sensitivity
WTD_sens_summary_rev2 <- as.data.frame(rbind(WTD_sens_yr_summary_rev2, WTD_sens_seas_summary_rev2, WTD_sens_30_summary_rev2))
denit_sens_summary_rev2 <- rbind(denit_sens_yr_summary_rev2, denit_sens_seas_summary_rev2, denit_sens_30_summary_rev2)
TN2_sens_summary_rev2 <- rbind(TN2_sens_yr_summary_rev2, TN2_sens_seas_summary_rev2, TN2_sens_30_summary_rev2)

colnames(WTD_sens_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight", "Color")
colnames(denit_sens_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight", "Color")
colnames(TN2_sens_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight", "Color")

# Filter (manually) only the parameters that are highly sensitive in multiple time steps
parameters <- colnames(WTD_sens_log1_500_rev2[["boot"]])
WTD_sens_params <- parameters[c(1, 5, 14, 17, 18)] # DA, slope, FC, permeability, % clay
denit_sens_params <- parameters[c(1, 5, 12, 41)] # DA, slope, layer thickness, denitro K
TN2_sens_params <- parameters[c(1, 5, 17, 21)] # DA, slope, permeability, rainfall interception

WTD_sens_subset_rev2 <- WTD_sens_summary_rev2[WTD_sens_summary_rev2$Parameter %in% WTD_sens_params,]
denit_sens_subset_rev2 <- denit_sens_summary_rev2[denit_sens_summary_rev2$Parameter %in% denit_sens_params,]
TN2_sens_subset_rev2 <- TN2_sens_summary_rev2[TN2_sens_summary_rev2$Parameter %in% TN2_sens_params,]

# Change "full" results to have asterisk in label
colnames(WTD_sens_full_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight")
colnames(denit_sens_full_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight")
colnames(TN2_sens_full_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight")
WTD_sens_full_subset_rev2 <- WTD_sens_full_summary_rev2[WTD_sens_full_summary_rev2$Parameter %in% WTD_sens_params,]
denit_sens_full_subset_rev2 <- denit_sens_full_summary_rev2[denit_sens_full_summary_rev2$Parameter %in% denit_sens_params,]
TN2_sens_full_subset_rev2 <- TN2_sens_full_summary_rev2[TN2_sens_full_summary_rev2$Parameter %in% TN2_sens_params,]

# Generate coordinates to plot rectangles for each window
WTD_xleft <- vector(mode="numeric", length=nrow(WTD_sens_subset_rev2))
WTD_xright <- vector(mode="numeric", length=nrow(WTD_sens_subset_rev2))
WTD_ybott <- vector(mode="numeric", length=nrow(WTD_sens_subset_rev2))
WTD_ytop <- vector(mode="numeric", length=nrow(WTD_sens_subset_rev2))

for (i in 1:nrow(WTD_sens_subset_rev2)) {
  
  # x value
  if (WTD_sens_subset_rev2$WindowType[i] == "Year") {
    WTD_xleft[i] <- yrs[WTD_sens_subset_rev2$Window[i]]
    WTD_xright[i] <- yrs[WTD_sens_subset_rev2$Window[i]+1]-1
    
    # Y values
    for (j in 1:length(WTD_sens_params)) {
      if (WTD_sens_subset_rev2$Parameter[i]== WTD_sens_params[j]) {
        WTD_ybott[i] <- (5*j)-4
        WTD_ytop[i] <- (5*j)-3.04
      }
    }
  }
  
  else if (WTD_sens_subset_rev2$WindowType[i] == "Season") {
    WTD_xleft[i] <- seasons_rev3[WTD_sens_subset_rev2$Window[i]]
    WTD_xright[i] <- seasons_rev3[WTD_sens_subset_rev2$Window[i]+1]-1
    
    # Y values
    for (j in 1:length(WTD_sens_params)) {
      if (WTD_sens_subset_rev2$Parameter[i]== WTD_sens_params[j]) {
        WTD_ybott[i] <- (5*j)-3
        WTD_ytop[i] <- (5*j)-2.04
      }
    }
  }
  
  else if (WTD_sens_subset_rev2$WindowType[i] == "30") {
    WTD_xleft[i] <- (WTD_sens_subset_rev2$Window[i]*30)-29
    WTD_xright[i] <- (WTD_sens_subset_rev2$Window[i]*30)
    
    # Y values
    for (j in 1:length(WTD_sens_params)) {
      if (WTD_sens_subset_rev2$Parameter[i]== WTD_sens_params[j]) {
        WTD_ybott[i] <- (5*j)-2
        WTD_ytop[i] <- (5*j)-1.04
      }
    }
  }
}
WTD_sens_subset_rev2_2 <- cbind(WTD_sens_subset_rev2, WTD_xleft, WTD_ybott, WTD_xright, WTD_ytop)


# Denit
denit_xleft <- vector(mode="numeric", length=nrow(denit_sens_subset_rev2))
denit_xright <- vector(mode="numeric", length=nrow(denit_sens_subset_rev2))
denit_ybott <- vector(mode="numeric", length=nrow(denit_sens_subset_rev2))
denit_ytop <- vector(mode="numeric", length=nrow(denit_sens_subset_rev2))

for (i in 1:nrow(denit_sens_subset_rev2)) {
  
  # x values
  if (denit_sens_subset_rev2$WindowType[i] == "Year") {
    denit_xleft[i] <- yrs[denit_sens_subset_rev2$Window[i]]
    denit_xright[i] <- yrs[denit_sens_subset_rev2$Window[i]+1]-1
    
    # Y values
    for (j in 1:length(denit_sens_params)) {
      if (denit_sens_subset_rev2$Parameter[i]== denit_sens_params[j]) {
        denit_ybott[i] <- (5*j)-4
        denit_ytop[i] <- (5*j)-3.04
      }
    }
  }
  
  else if (denit_sens_subset_rev2$WindowType[i] == "Season") {
    denit_xleft[i] <- seasons_rev3[denit_sens_subset_rev2$Window[i]]
    denit_xright[i] <- seasons_rev3[denit_sens_subset_rev2$Window[i]+1]-1
    
    # Y values
    for (j in 1:length(denit_sens_params)) {
      if (denit_sens_subset_rev2$Parameter[i]== denit_sens_params[j]) {
        denit_ybott[i] <- (5*j)-3
        denit_ytop[i] <- (5*j)-2.04
      }
    }
  }
  
  else if (denit_sens_subset_rev2$WindowType[i] == "30") {
    denit_xleft[i] <- (denit_sens_subset_rev2$Window[i]*30)-29
    denit_xright[i] <- (denit_sens_subset_rev2$Window[i]*30)
    
    # Y values
    for (j in 1:length(denit_sens_params)) {
      if (denit_sens_subset_rev2$Parameter[i]== denit_sens_params[j]) {
        denit_ybott[i] <- (5*j)-2
        denit_ytop[i] <- (5*j)-1.04
      }
    }
  }
}
denit_sens_subset_rev2_2 <- cbind(denit_sens_subset_rev2, denit_xleft, denit_ybott, denit_xright, denit_ytop)

# TN2
TN2_xleft <- vector(mode="numeric", length=nrow(TN2_sens_subset_rev2))
TN2_xright <- vector(mode="numeric", length=nrow(TN2_sens_subset_rev2))
TN2_ybott <- vector(mode="numeric", length=nrow(TN2_sens_subset_rev2))
TN2_ytop <- vector(mode="numeric", length=nrow(TN2_sens_subset_rev2))

for (i in 1:nrow(TN2_sens_subset_rev2)) {
  
  # x values
  if (TN2_sens_subset_rev2$WindowType[i] == "Year") {
    TN2_xleft[i] <- yrs[TN2_sens_subset_rev2$Window[i]]
    TN2_xright[i] <- yrs[TN2_sens_subset_rev2$Window[i]+1]-1
    
    # Y values
    for (j in 1:length(TN2_sens_params)) {
      if (TN2_sens_subset_rev2$Parameter[i]== TN2_sens_params[j]) {
        TN2_ybott[i] <- (5*j)-4
        TN2_ytop[i] <- (5*j)-3.04
      }
    }
  }
  
  else if (TN2_sens_subset_rev2$WindowType[i] == "Season") {
    TN2_xleft[i] <- seasons_rev3[TN2_sens_subset_rev2$Window[i]]
    TN2_xright[i] <- seasons_rev3[TN2_sens_subset_rev2$Window[i]+1]-1
    
    # Y values
    for (j in 1:length(TN2_sens_params)) {
      if (TN2_sens_subset_rev2$Parameter[i]== TN2_sens_params[j]) {
        TN2_ybott[i] <- (5*j)-3
        TN2_ytop[i] <- (5*j)-2.04
      }
    }
  }
  
  else if (TN2_sens_subset_rev2$WindowType[i] == "30") {
    TN2_xleft[i] <- (TN2_sens_subset_rev2$Window[i]*30)-29
    TN2_xright[i] <- (TN2_sens_subset_rev2$Window[i]*30)
    
    # Y values
    for (j in 1:length(TN2_sens_params)) {
      if (TN2_sens_subset_rev2$Parameter[i]== TN2_sens_params[j]) {
        TN2_ybott[i] <- (5*j)-2
        TN2_ytop[i] <- (5*j)-1.04
      }
    }
  }
}
TN2_sens_subset_rev2_2 <- cbind(TN2_sens_subset_rev2, TN2_xleft, TN2_ybott, TN2_xright, TN2_ytop)

# Put boxes around each row in a parameter
boxes_ybott <- 1:(length(WTD_sens_params)*5)
boxes_ybott <- boxes_ybott[(boxes_ybott %% 5) != 0]
for (i in 1:length(boxes_ybott)) {
  if(((boxes_ybott[i]+1) %% 5) == 0) {
    boxes_ybott<- boxes_ybott[-c(i)]
  }
}

#
# Plots
palette_green <- brewer.pal(4, "Greens")
palette_orange <- brewer.pal(4, "Oranges")
palette_purple <- brewer.pal(4, "Purples")

#
# WTD
for (i in 1:nrow(WTD_sens_full_subset_rev2)) {
  if (WTD_sens_full_subset_rev2$RelativeSensitivity_High[i] >0) {
    WTD_sens_params[i] <- paste0(WTD_sens_params[i], "*")
  }
}
WTD_sens_params <- gsub("_", " ", WTD_sens_params)

WTD_sens_params[3] <- "Field\ncapacity"

par(mar=c(5,4,4,2)+0.1)
# Version 6; with line plot of WTD on top
legendCol <- cRamp(c(0.3,0.2,0.1,0), "Oranges")
legend_image <- as.raster(matrix(legendCol, ncol=1))
png(paste0("REMM Sensitivity plot comps_", mytime, "_WTD_rev6.png"), type = "cairo", units = "in",
    height = 6.5, width = 10.5, res = 500)
par(oma = c(1,1.25,0.5,1))
par(mfrow=c(1,1))
plot(c(1,nrow(subset_rev2_WTD_output_all)), c(-2, length(WTD_sens_params)*6-3), type="n", ylab="Parameter", yaxt="n", xaxt="n",cex.axis=0.8,xlab="Simulation Time", main="Water Table Depth Sensitivity")
for (i in 1:nrow(WTD_sens_subset_rev2)) {
  # Non-sensitive parameters have empty rectangles
  if (WTD_sens_subset_rev2_2$RelativeSensitivity_High[i] > 0) {
    if (WTD_sens_subset_rev2$WindowType[i] == "Year") {
      rect(WTD_xleft[i], WTD_ybott[i], WTD_xright[i], WTD_ytop[i], col=WTD_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
    
    else if (WTD_sens_subset_rev2$WindowType[i] == "Season") {
      rect(WTD_xleft[i], WTD_ybott[i], WTD_xright[i], WTD_ytop[i], col=WTD_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
    
    else if (WTD_sens_subset_rev2$WindowType[i] == "30") {
      rect(WTD_xleft[i], WTD_ybott[i], WTD_xright[i], WTD_ytop[i], col=WTD_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
  }
  
  else if (WTD_sens_subset_rev2_2$RelativeSensitivity_Slight[i] >0) {
    if (WTD_sens_subset_rev2$WindowType[i] == "Year") {
      rect(WTD_xleft[i], WTD_ybott[i], WTD_xright[i], WTD_ytop[i], col=WTD_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
    
    else if (WTD_sens_subset_rev2$WindowType[i] == "Season") {
      rect(WTD_xleft[i], WTD_ybott[i], WTD_xright[i], WTD_ytop[i], col=WTD_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
    
    else if (WTD_sens_subset_rev2$WindowType[i] == "30") {
      rect(WTD_xleft[i], WTD_ybott[i], WTD_xright[i], WTD_ytop[i], col=WTD_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
  }
  # Sensitive parameters are colored
  else {
    rect(WTD_xleft[i], WTD_ybott[i], WTD_xright[i], WTD_ytop[i], col=WTD_sens_subset_rev2_2$Color[i], border=NA,lwd=0.1)
    
  }
}
axis(side = 2, at = c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5), labels = WTD_sens_params[1:7], tick=FALSE, las = 1, cex.axis = 0.7,
     mgp = c(3, 0.1, 0))
axis(side = 1, at = seasons_rev3, labels=c("W", rep(c("Spr", "Smr", "F", "W"), times=4)),tick=TRUE, las = 1, cex.axis = 0.75,
     mgp = c(3, 0.25, 0)) # Seasons
axis(side=1, at=yrs[2:5]-183, labels=c("Year 1", "Year 2", "Year 3", "Year 4"), tick=FALSE, las=1)
legend(x="bottomright",legend=c("Annual", "Seasonal", "30-day"), bty="n",fill= c(palette_green[3], palette_orange[3], palette_purple[3]),cex=0.6, pt.cex=0.5, border=NA,title="Window Size")
par(new=TRUE)
plot(c(1,nrow(subset_rev2_WTD_output_all)), c(-2, length(WTD_sens_params)*6-3), type='n', axes=F, xlab='',ylab='')
rasterImage(legend_image, 1260,-2.5,1350,-0.4)
lbsq=seq.int(-2.5,-0.4, l=3)
mtext(c(0,0.15,0.3),4,-4.5,at=lbsq,las=2,cex=0.5)
text(1320,0.5,"Sensitivity Index", cex=0.6)
#par(mar = c(0.5, 4, 2, 0))
par(new=TRUE)
plot(1:1461, WTD_output_all_avg/1000, type="l", axes=FALSE, ylim=c(10,min(WTD_output_all_avg/1000)), xlab="", ylab="")
axis(side=4, at=c(0,1), ylab="Depth", cex.axis=0.8,mgp = c(3, 0.35, 0))
text(1595,0.5,"Depth (m)",srt=270, xpd=NA, cex=0.9)
dev.off()

# Denit
for (i in 1:nrow(denit_sens_full_subset_rev2)) {
  if (denit_sens_full_subset_rev2$RelativeSensitivity_High[i] >0) {
    denit_sens_params[i] <- paste0(denit_sens_params[i], "*")
  }
}
denit_sens_params <- gsub("_", "\n", denit_sens_params)

par(mar=c(5,4,4,2)+0.1)
# Version 6; with line plot of denit on top
legendCol <- cRamp(c(0.3,0.2,0.1,0), "Oranges")
legend_image <- as.raster(matrix(legendCol, ncol=1))
png(paste0("REMM Sensitivity plot comps_", mytime, "_denit_rev6.png"), type = "cairo", units = "in",
    height = 6.5, width = 10.5, res = 500)
par(oma = c(1,1.25,0.5,1))
plot(c(1,nrow(subset_rev2_denitr1_output_all)), c(-2, length(denit_sens_params)*6-2), type="n", ylab="Parameter", yaxt="n", xaxt="n",cex.axis=0.8,xlab="Simulation Time", main= "Denitrification Sensitivity")
for (i in 1:nrow(denit_sens_subset_rev2)) {
  # Non-sensitive parameters have empty rectangles
  if (denit_sens_subset_rev2_2$RelativeSensitivity_High[i] > 0) {
    if (denit_sens_subset_rev2$WindowType[i] == "Year") {
      rect(denit_xleft[i], denit_ybott[i], denit_xright[i], denit_ytop[i], col=denit_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
    
    else if (denit_sens_subset_rev2$WindowType[i] == "Season") {
      rect(denit_xleft[i], denit_ybott[i], denit_xright[i], denit_ytop[i], col=denit_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
    
    else if (denit_sens_subset_rev2$WindowType[i] == "30") {
      rect(denit_xleft[i], denit_ybott[i], denit_xright[i], denit_ytop[i], col=denit_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
  }
  else if (denit_sens_subset_rev2_2$RelativeSensitivity_Slight[i] > 0) {
    if (denit_sens_subset_rev2$WindowType[i] == "Year") {
      rect(denit_xleft[i], denit_ybott[i], denit_xright[i], denit_ytop[i], col=denit_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
    
    else if (denit_sens_subset_rev2$WindowType[i] == "Season") {
      rect(denit_xleft[i], denit_ybott[i], denit_xright[i], denit_ytop[i], col=denit_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
    
    else if (denit_sens_subset_rev2$WindowType[i] == "30") {
      rect(denit_xleft[i], denit_ybott[i], denit_xright[i], denit_ytop[i], col=denit_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
  }
  # Sensitive parameters are colored
  else {
    rect(denit_xleft[i], denit_ybott[i], denit_xright[i], denit_ytop[i], col=denit_sens_subset_rev2_2$Color[i], border=NA,lwd=0.1)
    
  }
}
axis(side = 2, at = c(2.5, 7.5, 12.5, 17.5), labels = denit_sens_params[1:4], tick=FALSE, las = 1, cex.axis = 0.7,
     mgp = c(3, 0.1, 0))
axis(side = 1, at = seasons_rev3, labels=c("W", rep(c("Spr", "Smr", "F", "W"), times=4)),tick=TRUE, las = 1, cex.axis = 0.75,
     mgp = c(3, 0.25, 0)) # Seasons
axis(side=1, at=yrs[2:5]-183, labels=c("Year 1", "Year 2", "Year 3", "Year 4"), tick=FALSE, las=1)
legend(x="bottomright",legend=c("Annual", "Seasonal", "30-day"), bty="n",fill=c(palette_green[3], palette_orange[3], palette_purple[3]),cex=0.6, pt.cex=0.5, border=NA,title="Window Size")
par(new=TRUE)
plot(c(1,nrow(subset_rev2_denitr1_output_all)), c(-2, length(denit_sens_params)*6-2), type='n', axes=F, xlab='',ylab='')
rasterImage(legend_image, 1260,-2.5,1350,-0.4)
lbsq=seq.int(-2.5,-0.4, l=3)
mtext(c(0,0.15,0.3),4,-4.5,at=lbsq,las=2,cex=0.5)
text(1320,0.5,"Sensitivity Index", cex=0.6)#par(mar = c(0.5, 4, 2, 0))
par(new=TRUE)
plot(1:1461, denitr1_output_all_avg, type="l", axes=FALSE, ylim=c(-8,max(denitr1_output_all_avg)+0.3), xlab="", ylab="")
axis(side=4, at=c(0,1), ylab="Denitrification", cex.axis=0.8,mgp = c(3, 0.35, 0))
text(1595,0.5,"Denitrification (kg/ha)",srt=270, xpd=NA, cex=0.9)
dev.off()

# TN2
for (i in 1:nrow(TN2_sens_full_subset_rev2)) {
  if (TN2_sens_full_subset_rev2$RelativeSensitivity_High[i] >0) {
    TN2_sens_params[i] <- paste0(TN2_sens_params[i], "*")
  }
}
TN2_sens_params <- gsub("_", "\n", TN2_sens_params)

par(mar=c(5,4,4,2)+0.1)
# Version 6; with line plot of TN on top
legendCol <- cRamp(c(0.7,0.5,0.3,0.1,0), "Oranges")
legend_image <- as.raster(matrix(legendCol, ncol=1))
png(paste0("REMM Sensitivity plot comps_", mytime, "_TN2_rev6.png"), type = "cairo", units = "in",
    height = 6.5, width = 10.5, res = 500)
par(oma = c(1,1.25,0.5,1))
plot(c(1,nrow(subset_rev2_TN2_output_all)), c(-2, length(TN2_sens_params)*6-2), type="n", ylab="Parameter", yaxt="n", xaxt="n",cex.axis=0.8,xlab="Simulation Time", main= "Total Nitrogen Sensitivity")
for (i in 1:nrow(TN2_sens_subset_rev2)) {
  # Non-sensitive parameters have empty rectangles
  if (TN2_sens_subset_rev2_2$RelativeSensitivity_High[i] > 0) {
    if (TN2_sens_subset_rev2$WindowType[i] == "Year") {
      rect(TN2_xleft[i], TN2_ybott[i], TN2_xright[i], TN2_ytop[i], col=TN2_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
    
    else if (TN2_sens_subset_rev2$WindowType[i] == "Season") {
      rect(TN2_xleft[i], TN2_ybott[i], TN2_xright[i], TN2_ytop[i],  col=TN2_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
    
    else if (TN2_sens_subset_rev2$WindowType[i] == "30") {
      rect(TN2_xleft[i], TN2_ybott[i], TN2_xright[i], TN2_ytop[i],  col=TN2_sens_subset_rev2_2$Color[i],lwd=0.3, border=TRUE)
    }
  }
  else if (TN2_sens_subset_rev2_2$RelativeSensitivity_Slight[i] > 0) {
    if (TN2_sens_subset_rev2$WindowType[i] == "Year") {
      rect(TN2_xleft[i], TN2_ybott[i], TN2_xright[i], TN2_ytop[i], col=TN2_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
    
    else if (TN2_sens_subset_rev2$WindowType[i] == "Season") {
      rect(TN2_xleft[i], TN2_ybott[i], TN2_xright[i], TN2_ytop[i],  col=TN2_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
    
    else if (TN2_sens_subset_rev2$WindowType[i] == "30") {
      rect(TN2_xleft[i], TN2_ybott[i], TN2_xright[i], TN2_ytop[i],  col=TN2_sens_subset_rev2_2$Color[i],lwd=0.1, border=NA)
    }
  }
  # Sensitive parameters are colored
  else {
    rect(TN2_xleft[i], TN2_ybott[i], TN2_xright[i], TN2_ytop[i],  col=TN2_sens_subset_rev2_2$Color[i],border=NA,lwd=0.1)
    
  }
}

axis(side = 2, at = c(2.5, 7.5, 12.5, 17.5), labels = TN2_sens_params[1:4], tick=FALSE, las = 1, cex.axis = 0.7,
     mgp = c(3, 0.1, 0))
axis(side = 1, at = seasons_rev3, labels=c("W", rep(c("Spr", "Smr", "F", "W"), times=4)),tick=TRUE, las = 1, cex.axis = 0.75,
     mgp = c(3, 0.25, 0)) # Seasons
axis(side=1, at=yrs[2:5]-183, labels=c("Year 1", "Year 2", "Year 3", "Year 4"), tick=FALSE, las=1)
legend(x="bottomright",legend=c("Annual", "Seasonal", "30-day"), bty="n",fill=c(palette_green[3], palette_orange[3], palette_purple[3]),cex=0.6, pt.cex=0.5, border=NA,title="Window Size")
par(new=TRUE)
plot(c(1,nrow(subset_rev2_TN2_output_all)), c(-2, length(TN2_sens_params)*6-2), type='n', axes=F, xlab='',ylab='')
rasterImage(legend_image, 1260,-2.5,1350,-0.7)
lbsq=seq.int(-2.5,-0.7, l=3)
mtext(c(0,0.35,0.7),4,-4.5,at=lbsq,las=2,cex=0.5)
text(1320,0.1,"Sensitivity Index", cex=0.6)#par(mar = c(0.5, 4, 2, 0))
par(new=TRUE)
plot(1:1461, TN2_output_all_avg, type="l", axes=FALSE, ylim=c(-1600,max(TN2_output_all_avg)-50), xlab="", ylab="")
axis(side=4, at=c(0,100, 200), ylab="TN2", cex.axis=0.8,mgp = c(3, 0.35, 0))
text(1595,0,"Total Nitrogen (kg/ha)",srt=270, xpd=NA, cex=0.9)
dev.off()

#
# CVD Plot
png(paste0("CVD_plot_log1_", mytime, "_rev2.png"), type = "cairo", units = "in",
    height = 8, width = 8, res = 500)


par(mfrow=c(2,2)) # rows and columns of plots
par(oma=c(3, 3, 1, 1)) #
par(mar=c(3, 2, 0.5, 2)) # margins
par(mgp=c(0.5, 0.5, 0)) # axis offsets

plot(log1_WTD_CVD_rev2[, 1], log1_WTD_CVD_rev2[, 2], type="p", xlim=c(0, 0.8), ylim=c(0, 0.8), pch=19, ylab="", xlab="", cex = 1, col=col_sel)
abline(v=0.1, col="gray", lty=2)
abline(h=0.1, col="gray", lty=2)
mtext("Water Table Depth", 3, line=0, cex=0.75)
mtext("(a)", 3, at=(0), cex=0.6)
text(0.38, 0.21, "Layer thickness", srt=0, xpd=NA)
text(0.24, 0.13, "Drainage area", srt=0, xpd=NA)

plot(log1_subdisNO3_CVD_rev2[, 1], log1_subdisNO3_CVD_rev2[, 2], type="p", xlim=c(0, 0.8), ylim=c(0, 0.8), pch=19, ylab="", xlab="", cex = 1, col=col_sel)
abline(v=0.1, col="gray", lty=2)
abline(h=0.1, col="gray", lty=2)
mtext("Subsurface Dissolved Nitrate", 3, line=0, cex=0.75)
mtext("(b)", 3, at=(0), cex=0.6)
text(0.72, 0.52, "Permeability", srt=0, xpd=NA)
text(0.55, 0.6, "% clay", srt=0, xpd=NA)

plot(log1_denitr1_CVD_rev2[, 1], log1_denitr1_CVD_rev2[, 2], type="p", xlim=c(0, 0.8), ylim=c(0, 0.8), pch=19, ylab="", xlab="", cex = 1, col=col_sel)
abline(v=0.1, col="gray", lty=2)
abline(h=0.1, col="gray", lty=2)
mtext("Denitrification", 3, line=0, cex=0.75)
mtext("(c)", 3, at=(0), cex=0.6)
text(0.6, 0.19, "Denitrification K", srt=0, xpd=NA)
text(0.24, 0.195, "Layer thickness", srt=0, xpd=NA)
text(0.24, 0.12, "Drainage area", srt=0, xpd=NA)

plot(log1_TN2_CVD_rev2[, 1], log1_TN2_CVD_rev2[, 2], type="p", xlim=c(0, 0.8), ylim=c(0, 0.8), pch=19, ylab="", xlab="", cex = 1, col=col_sel)
abline(v=0.1, col="gray", lty=2)
abline(h=0.1, col="gray", lty=2)
mtext("Total Nitrogen", 3, line=0, cex=0.75)
mtext("(d)", 3, at=(0), cex=0.6)
text(0.7, 0.26, "Drainage area", srt=0, xpd=NA)
text(0.6, 0.16, "Slope", srt=0, xpd=NA)
text(0.2, 0.22, "Zone width", srt=0, xpd=NA)

mtext(expression("Main effect "*italic("S"[i])), 1, outer=TRUE, line=-1.5, cex=0.75)
mtext(expression("Interaction "*italic("I"[i])), 2, outer=TRUE, cex=0.75)

par(fig = c(0, 1, 0.01, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("bottom", names[condensed], col=col_sel[condensed], pch=19, cex=0.6, ncol=7)
dev.off()