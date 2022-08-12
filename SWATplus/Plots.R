# Plots
# Creates plots found in Buhr et al.

# Needed for color plots
library("RColorBrewer")

#
# Create 4-panel Plischke plot
#These are your sensitivity outputs for each of your model inputs
wtd_output_log1_3 <- read.table(paste0("Sensitivity_Output_", mytime, "_log1_wtd_sd.txt"), header = TRUE)
no3_output_log1_3 <- read.table(paste0("Sensitivity_Output_", mytime, "_log1_no3_sum.txt"), header = TRUE)
denit_output_log1_3 <- read.table(paste0("Sensitivity_Output_", mytime, "_log1_denit_sum.txt"), header = TRUE)

#Set input variable names for plot. Getting them from the column names but can also
#write them in manually
names <- colnames(wtd_output_log1_3[1:(ncol(data[[j]])-1)])
names <- gsub("_", " ", names)

#Get values of 1 to the number of input variables
y <- 1:(ncol(wtd_output_log1_3)-1)

#Names for each sub-plot
output_names <- c("Water Table Depth", "Riverine Nitrate Load", "Total Denitrification")
#The "png" function prints a .png image of your plot. You can change the file name and
#the height and width
png(paste0("Sensitivity plot summary_", mytime, "_2021col_log1_rev2.png"), type = "cairo", units = "in",
    height = 6.5, width = 10.5, res = 500)

#sets margins. mfrow = c(1,4) means 1 row and 4 columns of plots
#You may need to adjust the left outer margin (the second value in oma)
#to fit your input variable names
par(mfrow = c(1,3), mar = c(2, 1, 1.5, 0.5), oma = c(2,13,0.5,0)) 

#loads the plot functions R script - change the path name
source("Y:/DanielBuhr/Sensitivity_Analysis_Parameter_Ranges/Plot Functions.R") 

#combines the data (sensitivity outputs) into a list
#data <- list(WTD, subdisNO3, NO3GW1, denitr1)
data <- list(wtd_output_log1_3, no3_output_log1_3, denit_output_log1_3)
xmax <- max(sapply(data, max))

palette <- brewer.pal(4, "Dark2")

#Loops through list, for each output, creates a plot
for (j in 1:3){
  
  #Get the median of the dummy value - plots this as a line
  dummy_val <- data[[j]][3, "dummy_3"]
  
  #Creates plot
  plot(NA, xlim = c(0, xmax), ylim = c(1, max(y)), yaxt = "n", 
       xlab = "", ylab = "", main = output_names[j], cex.axis=1.2, cex.main=1.3)
  
  #Adds dummy variable line
  abline(v = dummy_val, lty = 2, col = "gray60", lwd = 2)
  
  #Adds error bars
  arrows(x0 = as.numeric(data[[j]][1,1:(ncol(data[[j]])-1)]), y0 = y, 
         x1 = as.numeric(data[[j]][5,1:(ncol(data[[j]])-1)]),
         y1 = y, length = 0)
  
  #Adds points
  if (j==1) { # WTD
    #Highly sensitive
    points(data[[j]][3,c(5,7,9)], y[c(5,7,9)], pch = 21, bg = palette[2], cex = 1.8)
    points(data[[j]][3,c(15,17)], y[c(15,17)], pch = 21, bg = palette[4], cex = 1.8)
    
    # Slightly sensitive
    points(data[[j]][3,1:2], y[1:2], pch = 22, bg = palette[1], cex = 1.8)
    points(data[[j]][3,c(4,8)], y[c(4,8)], pch = 22, bg = palette[2], cex = 1.8)
    points(data[[j]][3,11:14], y[11:14], pch = 22, bg = palette[3], cex = 1.8)
    points(data[[j]][3,c(16,19)], y[c(16,19)], pch = 22, bg = palette[4], cex = 1.8)
    
    #Not sensitive
    points(data[[j]][3,3], y[3], pch = 24, bg = palette[1], cex = 1.8)
    points(data[[j]][3,c(6,10)], y[c(6,10)], pch = 24, bg = palette[2], cex = 1.8)
    points(data[[j]][3,18], y[18], pch = 24, bg = palette[4], cex = 1.8)
  }
  
  if (j==2){
    #Highly sensitive
    points(data[[j]][3,3], y[3], pch = 21, bg = palette[1], cex = 1.8)
    
    #Not sensitive
    points(data[[j]][3,1:2], y[1:2], pch = 24, bg = palette[1], cex = 1.8)
    points(data[[j]][3,4:10], y[4:10], pch = 24, bg = palette[2], cex = 1.8)
    points(data[[j]][3,11:14], y[11:14], pch = 24, bg = palette[3], cex = 1.8)
    points(data[[j]][3,15:19], y[15:19], pch = 24, bg = palette[4], cex = 1.8)
  }
  
  if (j==3) {
    #Highly sensitive
    points(data[[j]][3,3], y[3], pch = 21, bg = palette[1], cex = 1.8)
    
    
    #Not sensitive
    points(data[[j]][3,1:2], y[1:2], pch = 24, bg = palette[1], cex = 1.8)
    points(data[[j]][3,4:10], y[4:10], pch = 24, bg = palette[2], cex = 1.8)
    points(data[[j]][3,11:14], y[11:14], pch = 24, bg = palette[3], cex = 1.8)
    points(data[[j]][3,15:19], y[15:19], pch = 24, bg = palette[4], cex = 1.8)
  }
  
  #Adds axes with labels (first plot only)
  
  if (j == 1){
    axis(side = 2, at = y[1:3], labels = names[1:3], las = 1, cex.axis = 1.3,
         mgp = c(3, 0.8, 0), col.axis=palette[1])
    axis(side = 2, at = y[4:10], labels = names[4:10], las = 1, cex.axis = 1.3,
         mgp = c(3, 0.8, 0), col.axis=palette[2])
    axis(side = 2, at = y[11:14], labels = names[11:14], las = 1, cex.axis = 1.3,
         mgp = c(3, 0.8, 0), col.axis=palette[3])
    axis(side = 2, at = y[15:19], labels = names[15:19], las = 1, cex.axis = 1.3,
         mgp = c(3, 0.8, 0), col.axis=palette[4])
  }else{
    axis(side = 2, at = y, labels = FALSE, las = 1)
  }
  
  
  #Adds (a), (b), etc. labels to plots
  add_label(-0.08, -0.03, paste0("(", letters[j], ")"), cex=1.3)
  if (j==3) {
    legend(x="topright", legend=c("Basin", "Soil", "Aquifer/Routing", "HRU"), bty="n",pch=21, pt.bg= palette, cex=1.2, pt.cex=1.4, title="Parameter Type")
    legend(x=0.335, y=16, legend=c("Highly", "Slightly", "Not"), bty="n", pch=c(21,22,24), pt.bg= palette[1], cex=1.2, pt.cex=1.4)
    text(x=0.44, y=16.1, "Sensitivity", cex=1.2)}
}
mtext("Sensitivity Index", side = 1, outer = TRUE, line = 0.5)
dev.off()

#
# Create plots to show sensitivity for most sensitive parameters across various time steps

# Generate matrices for each time-varying analyses that will be combined outside the loop
WTD_sens_full_summary_rev2 <- data.frame(matrix(,nrow=(ncol(log1_wtd_sens_3[["boot"]])-1), ncol=6))
denit_sens_full_summary_rev2 <- data.frame(matrix(,nrow=(ncol(log1_denit_sens_3[["boot"]])-1), ncol=6))
WTD_sens_yr_summary_rev2 <- data.frame(matrix(,nrow=(n_yr*(ncol(log1_wtd_sens_3[["boot"]])-1)), ncol=6))
denit_sens_yr_summary_rev2 <- data.frame(matrix(,nrow=(n_yr*(ncol(log1_wtd_sens_3[["boot"]])-1)), ncol=6))
WTD_sens_seas_summary_rev2 <- data.frame(matrix(,nrow=(n_seas*(ncol(log1_wtd_sens_3[["boot"]])-1)), ncol=6))
denit_sens_seas_summary_rev2 <- data.frame(matrix(,nrow=(n_seas*(ncol(log1_wtd_sens_3[["boot"]])-1)), ncol=6))
WTD_sens_30_summary_rev2 <- data.frame(matrix(,nrow=(n_30*(ncol(log1_wtd_sens_3[["boot"]])-1)), ncol=6))
denit_sens_30_summary_rev2 <- data.frame(matrix(,nrow=(n_30*(ncol(log1_wtd_sens_3[["boot"]])-1)), ncol=6))

# Loop to fill those matrices
# Columns are: parameter name, Window type, window #, Sensitivity, sensitivity relative to dummy max

for (i in 1:(ncol(sens_log1_WTD_30[[1]])-1)) { # Each parameter
  
  # Full sim
  # Parameter names, column 1
  WTD_sens_full_summary_rev2[i, 1] <- colnames(log1_wtd_sens_3[["boot"]])[i]
  denit_sens_full_summary_rev2[i, 1] <- colnames(log1_denit_sens_3[["boot"]])[i]
  
  # Window type, column 2
  WTD_sens_full_summary_rev2[i, 2] <- "Full"
  denit_sens_full_summary_rev2[i, 2] <- "Full"
  
  # Window number, column 3
  WTD_sens_full_summary_rev2[i, 3] <- 1
  denit_sens_full_summary_rev2[i, 3] <- 1
  
  # Parameter Min Sensitivity, column 4
  WTD_sens_full_summary_rev2[i, 4] <- log1_wtd_sens_3[["boot"]][5,i]
  denit_sens_full_summary_rev2[i, 4] <- log1_denit_sens_3[["boot"]][5,i]
  
  # Min Sensitivity relative to dummy, max; column 5
  WTD_sens_full_summary_rev2[i, 5] <- log1_wtd_sens_3[["boot"]][5,i]-log1_wtd_sens_3[["boot"]][96]
  denit_sens_full_summary_rev2[i, 5] <- log1_denit_sens_3[["boot"]][5,i]-log1_denit_sens_3[["boot"]][96]
  
  #test for overlap in interquartile sensitivity; Column 6
  WTD_sens_full_summary_rev2[i, 6] <- log1_wtd_sens_3[["boot"]][4,i]-log1_wtd_sens_3[["boot"]][97]
  denit_sens_full_summary_rev2[i, 6] <- log1_denit_sens_3[["boot"]][4,i]-log1_denit_sens_3[["boot"]][97]
  
  # Year analyses
  for (j in 1:n_yr) { # Each window
    
    # Parameter names, column 1
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 1] <- colnames(sens_log1_WTD_yr[[1]])[i]
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 1] <- colnames(sens_log1_denit_yr[[1]])[i]
    
    # Window type, column 2
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 2] <- "Year"
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 2] <- "Year"
    
    # Window number, column 3
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 3] <- j
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 3] <- j
    
    # Parameter Min Sensitivity, column 4
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 4] <- sens_log1_WTD_yr[[j]][5,i]
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 4] <- sens_log1_denit_yr[[j]][5,i]
    
    # Sensitivity relative to dummy, max; column 5
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 5] <- sens_log1_WTD_yr[[j]][5,i]-sens_log1_WTD_yr[[j]][96]
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 5] <- sens_log1_denit_yr[[j]][5,i]-sens_log1_denit_yr[[j]][96]
    
    #test for overlap in interquartile sensitivity; Column 6
    WTD_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 6] <- sens_log1_WTD_yr[[j]][4,i]-sens_log1_WTD_yr[[j]][97]
    denit_sens_yr_summary_rev2[(((19*n_yr)-(n_yr*(20-i)))+j), 6] <- sens_log1_denit_yr[[j]][4,i]-sens_log1_denit_yr[[j]][97]
  }
  
  # Seasonal analyses
  for (j in 1:n_seas_rev2) { # Each window
    
    # Parameter names, column 1
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 1] <- colnames(sens_log1_WTD_seas_rev2[[1]])[i]
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 1] <- colnames(sens_log1_denit_seas_rev2[[1]])[i]
    
    # Window type, column 2
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 2] <- "Season"
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 2] <- "Season"
    
    # Window number, column 3
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 3] <- j
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 3] <- j
    
    # Parameter Min Sensitivity, column 4
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 4] <- sens_log1_WTD_seas_rev2[[j]][5,i]
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 4] <- sens_log1_denit_seas_rev2[[j]][5,i]
    
    # Min Sensitivity relative to dummy, max; column 5
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 5] <- sens_log1_WTD_seas_rev2[[j]][5,i]-sens_log1_WTD_seas_rev2[[j]][96]
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 5] <- sens_log1_denit_seas_rev2[[j]][5,i]-sens_log1_denit_seas_rev2[[j]][96]
    
    #test for overlap in interquartile sensitivity; Column 6
    WTD_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 6] <- sens_log1_WTD_seas_rev2[[j]][4,i]-sens_log1_WTD_seas_rev2[[j]][97]
    denit_sens_seas_summary_rev2[(((19*n_seas_rev2)-(n_seas_rev2*(20-i)))+j), 6] <- sens_log1_denit_seas_rev2[[j]][4,i]-sens_log1_denit_seas_rev2[[j]][97]
  }
  
  # 30-day analyses
  for (j in 1:n_30) { # Each window
    
    # Parameter names, column 1
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 1] <- colnames(sens_log1_WTD_30[[1]])[i]
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 1] <- colnames(sens_log1_denit_30[[1]])[i]
    
    # Window type, column 2
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 2] <- 30
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 2] <- 30
    
    # Window number, column 3
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 3] <- j
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 3] <- j
    
    # Parameter Min Sensitivity, column 4
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 4] <- sens_log1_WTD_30[[j]][5,i]
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 4] <- sens_log1_denit_30[[j]][5,i]
    
    # Min Sensitivity relative to dummy, max; column 5
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 5] <- sens_log1_WTD_30[[j]][5,i]-sens_log1_WTD_30[[j]][96]
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 5] <- sens_log1_denit_30[[j]][5,i]-sens_log1_denit_30[[j]][96]
    
    #test for overlap in interquartile sensitivity; Column 6
    WTD_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 6] <- sens_log1_WTD_30[[j]][4,i]-sens_log1_WTD_30[[j]][97]
    denit_sens_30_summary_rev2[(((19*n_30)-(n_30*(20-i)))+j), 6] <- sens_log1_denit_30[[j]][4,i]-sens_log1_denit_30[[j]][97]
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

WTD_sens_yr_summary_rev2$Color <- cRamp(c(WTD_sens_yr_summary_rev2$X4,0,0.5), "Greens")[1:nrow(WTD_sens_yr_summary_rev2)]
WTD_sens_seas_summary_rev2$Color<- cRamp(c(WTD_sens_seas_summary_rev2$X4,0,0.5), "Oranges")[1:nrow(WTD_sens_seas_summary_rev2)]
WTD_sens_30_summary_rev2$Color <- cRamp(c(WTD_sens_30_summary_rev2$X4,0,0.5), "Purples")[1:nrow(WTD_sens_30_summary_rev2)]
denit_sens_yr_summary_rev2$Color <- cRamp(c(denit_sens_yr_summary_rev2$X4,0,0.75), "Greens")[1:nrow(denit_sens_yr_summary_rev2)]
denit_sens_seas_summary_rev2$Color<- cRamp(c(denit_sens_seas_summary_rev2$X4,0,0.75), "Oranges")[1:nrow(denit_sens_seas_summary_rev2)]
denit_sens_30_summary_rev2$Color <- cRamp(c(denit_sens_30_summary_rev2$X4,0,0.75), "Purples")[1:nrow(denit_sens_30_summary_rev2)]

# Generate empty matrices with parameter sensitivity
WTD_sens_summary_rev2 <- as.data.frame(rbind(WTD_sens_yr_summary_rev2, WTD_sens_seas_summary_rev2, WTD_sens_30_summary_rev2))
denit_sens_summary_rev2 <- rbind(denit_sens_yr_summary_rev2, denit_sens_seas_summary_rev2, denit_sens_30_summary_rev2)

colnames(WTD_sens_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight", "Color")
colnames(denit_sens_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight", "Color")

# Filter only the parameters that are highly sensitive in multiple time steps
parameters <- colnames(log1_wtd_sens_3[["boot"]])
WTD_sens_params <- parameters[c(4, 5, 7, 9, 15, 16, 17)]
denit_sens_params <- parameters[3]

WTD_sens_subset_rev2 <- WTD_sens_summary_rev2[WTD_sens_summary_rev2$Parameter %in% WTD_sens_params,]
denit_sens_subset_rev2 <- denit_sens_summary_rev2[denit_sens_summary_rev2$Parameter %in% denit_sens_params,]

# Change "full" results to have asterisk in label
colnames(WTD_sens_full_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight")
colnames(denit_sens_full_summary_rev2) <- c("Parameter", "WindowType", "Window", "Sensitivity", "RelativeSensitivity_High", "RelativeSensitivity_Slight")
WTD_sens_full_subset_rev2 <- WTD_sens_full_summary_rev2[WTD_sens_full_summary_rev2$Parameter %in% WTD_sens_params,]
denit_sens_full_subset_rev2 <- denit_sens_full_summary_rev2[denit_sens_full_summary_rev2$Parameter %in% denit_sens_params,]

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
    WTD_xleft[i] <- seasons[WTD_sens_subset_rev2$Window[i]]
    WTD_xright[i] <- seasons[WTD_sens_subset_rev2$Window[i]+1]-1
    
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
    denit_xleft[i] <- seasons[denit_sens_subset_rev2$Window[i]]
    denit_xright[i] <- seasons[denit_sens_subset_rev2$Window[i]+1]-1
    
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

# Put boxes around each row in a parameter
boxes_ybott <- 1:(length(WTD_sens_params)*5)
boxes_ybott <- boxes_ybott[(boxes_ybott %% 5) != 0]
for (i in 1:length(boxes_ybott)) {
  if(((boxes_ybott[i]+1) %% 5) == 0) {
    boxes_ybott<- boxes_ybott[-c(i)]
  }
}
for (i in 1:(length(WTD_sens_params)*4)) {
  rect(1, boxes_ybott[i], 1461, boxes_ybott[i]+1, lwd=0.1)
}

#
# Plots
palette_green <- brewer.pal(4, "Greens")
palette_orange <- brewer.pal(4, "Oranges")
palette_purple <- brewer.pal(4, "Purples")

# WTD

wtd_3_avg <- rowMeans(wtd_3[,2:ncol(wtd_3)])


for (i in 1:nrow(WTD_sens_full_subset_rev2)) {
  if (WTD_sens_full_subset_rev2$RelativeSensitivity_High[i] >0) {
    WTD_sens_params[i] <- paste0(WTD_sens_params[i], "*")
  }
}
WTD_sens_params <- gsub("_", " ", WTD_sens_params)

WTD_sens_params[3] <- "Hydraulic\nconductivity*"
WTD_sens_params[1] <- "Soil layer\nthickness"
WTD_sens_params[4] <- "Clay\ncontent*"

par(mar=c(5,4,4,2)+0.1)
# Version 6; with line plot of WTD on top
legendCol <- cRamp(c(0.5,0), "Purples")
legend_image <- as.raster(matrix(legendCol, ncol=1))
png(paste0("Sensitivity plot comps_", mytime, "_WTD_rev6.png"), type = "cairo", units = "in",
    height = 6.5, width = 10.5, res = 500)
#par(mar = c(5, 4, 0, 0))
par(oma = c(1,1.25,0.5,1))
plot(c(1,nrow(wtd_3)), c(-3.5, length(WTD_sens_params)*6-3), type="n", ylab="Parameter", yaxt="n", xaxt="n",cex.axis=0.8,xlab="Simulation Time", main="Water Table Depth Sensitivity")
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
  
  else if(WTD_sens_subset_rev2_2$RelativeSensitivity_Slight[i] > 0) {
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
    rect(WTD_xleft[i], WTD_ybott[i], WTD_xright[i], WTD_ytop[i], col=WTD_sens_subset_rev2_2$Color[i],border=NA,lwd=0.1)
    
  }
}

axis(side = 2, at = c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5), labels = WTD_sens_params[1:7], tick=FALSE, las = 1, cex.axis = 0.7,
     mgp = c(3, 0.1, 0))
axis(side = 1, at = seasons_rev2, labels=c("W", rep(c("Spr", "Smr", "F", "W"), times=4)),tick=TRUE, las = 1, cex.axis = 0.75,
     mgp = c(3, 0.25, 0)) # Seasons
axis(side=1, at=yrs[2:5]-183, labels=c("Year 1", "Year 2", "Year 3", "Year 4"), tick=FALSE, las=1)
legend(x="bottomright",legend=c("Annual", "Seasonal", "30-day"), bty="n",fill= c(palette_green[3], palette_orange[3], palette_purple[3]),cex=0.6, pt.cex=0.5, border=NA, title="Window Size")
par(new=TRUE)
plot(c(1,nrow(wtd_3)), c(-3.5, length(WTD_sens_params)*6-3), type='n', axes=F, xlab='', ylab='')
rasterImage(legend_image, 1270,-4,1350,-0.75)
lbsq <- seq.int(-4,-0.75, l=3)
mtext(c(0,0.25,0.5),4,-4.5,at=lbsq,las=2,cex=0.5)
text(1320,0.15,"Sensitivity Index", cex=0.6)
par(new=TRUE)
plot(1:1461, wtd_3_avg, type="l", axes=FALSE, ylim=c(8, 2.7), xlab="", ylab="")
axis(side=4, at=c(2.5,3), cex.axis=0.8,mgp = c(3, 0.4, 0))
text(1590,2.8,"Depth (m)",srt=270, xpd=NA, cex=0.9)
dev.off()

# Denit
denit_3_avg <- rowMeans(denit_3[,2:ncol(denit_3)])

for (i in 1:nrow(denit_sens_full_subset_rev2)) {
  if (denit_sens_full_subset_rev2$RelativeSensitivity_High[i] >0) {
    denit_sens_params[i] <- paste0(denit_sens_params[i], "*")
  }
}
denit_sens_params <- gsub("_", " ", denit_sens_params)
denit_sens_params[1] <- "Denitrification\nsat threshold*"


# Version 6; with line plot of denit on top
legendCol <- cRamp(c(0.5,0.25,0), "Purples")
legend_image <- as.raster(matrix(legendCol, ncol=1))
png(paste0("Sensitivity plot comps_", mytime, "_denit_rev6.png"), type = "cairo", units = "in",
    height = 6.5, width = 10.5, res = 500)
par(oma = c(1,1.25,0.5,1))
plot(c(1,nrow(denit_3)), c(0.5, length(denit_sens_params)*5), type="n", ylab="", yaxt="n", xaxt="n",cex.axis=0.8,xlab="Simulation Time", main="Denitrification Sensitivity")
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
  
  else if(denit_sens_subset_rev2_2$RelativeSensitivity_Slight[i] > 0) {
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
    rect(denit_xleft[i], denit_ybott[i], denit_xright[i], denit_ytop[i], col=denit_sens_subset_rev2_2$Color[i],border=NA,lwd=0.1)
    
  }
}

axis(side = 2, at = c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5), labels = denit_sens_params[1:7], tick=FALSE, las = 1, cex.axis = 0.7,
     mgp = c(3, 0.1, 0))
axis(side = 1, at = seasons_rev2, labels=c("W", rep(c("Spr", "Smr", "F", "W"), times=4)),tick=TRUE, las = 1, cex.axis = 0.75,
     mgp = c(3, 0.25, 0)) # Seasons
axis(side=1, at=yrs[2:5]-183, labels=c("Year 1", "Year 2", "Year 3", "Year 4"), tick=FALSE, las=1)
legend(x="bottomright",legend=c("Annual", "Seasonal", "30-day"), bty="n",fill= c(palette_green[3], palette_orange[3], palette_purple[3]),cex=0.6, pt.cex=0.5, border=NA, title="Window Size")
par(new=TRUE)
plot(c(1,nrow(denit_3)), c(0.5, length(denit_sens_params)*5), type='n', axes=F, xlab='', ylab='')
rasterImage(legend_image, 1270,0.45,1350,0.75)
lbsq <- seq.int(0.45,0.75, l=3)
mtext(c(0,0.25,0.5),4,-4.5,at=lbsq,las=2,cex=0.5)
text(1320,0.89,"Sensitivity Index", cex=0.6)
par(new=TRUE)
plot(1:1461, denit_3_avg, type="l", axes=FALSE, ylim=c(-50, 10), xlab="", ylab="")
axis(side=4, at=c(0,10), cex.axis=0.8,mgp = c(3, 0.4, 0))
text(1595,5,"Denitrification (kg/ha)",srt=270, xpd=NA, cex=0.9)
dev.off()

# Subsurface dissolved nitrate does not need to be plotted 

#
# CVD Plot
png(paste0("CVD_plot_", mytime, "_log1_rev2.png"), type = "cairo", units = "in",
    height = 8, width = 8, res = 500)

par(mfrow=c(3,1)) # rows and columns of plots
par(oma=c(3, 3, 1, 1)) #
par(mar=c(3, 2, 0.5, 2)) # margins
par(mgp=c(0.5, 0.5, 0)) # axis offsets

plot(WTD_CVD_log1[, 1], WTD_CVD_log1[, 2], type="p", xlim=c(0, 1), ylim=c(0, 0.9), pch=19, ylab="", xlab="", cex = 1, col=col_sel)
abline(v=0.1, col="gray", lty=2)
abline(h=0.1, col="gray", lty=2)
mtext("Water Table Depth", 3, line=0, cex=0.75)
mtext("(a)", 3, at=(0), cex=0.6)

plot(no3_CVD_log1[, 1], no3_CVD_log1[, 2], type="p", xlim=c(0, 1), ylim=c(0, 0.9), pch=19, ylab="", xlab="", cex = 1, col=col_sel)
abline(v=0.1, col="gray", lty=2)
abline(h=0.1, col="gray", lty=2)
mtext("Riverine Nitrate Load", 3, line=0, cex=0.75)
mtext("(b)", 3, at=(0), cex=0.6)

plot(denit_CVD_log1[, 1], denit_CVD_log1[, 2], type="p", xlim=c(0, 1), ylim=c(0, 0.9), pch=19, ylab="", xlab="", cex = 1, col=col_sel)
abline(v=0.1, col="gray", lty=2)
abline(h=0.1, col="gray", lty=2)
mtext("Denitrification", 3, line=0, cex=0.75)
mtext("(c)", 3, at=(0), cex=0.6)

mtext(expression("Main effect "*italic("S"[i])), 1, outer=TRUE, line=-2, cex=0.75)
mtext(expression("Interaction "*italic("I"[i])), 2, outer=TRUE, cex=0.75)

par(fig = c(0, 1, 0.01, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("bottom", names, col=col_sel, pch=19, cex=0.7, ncol=5)
dev.off()

#
# Testing if the number of simulations performed were adequate

a <-100
vector_a <- vector(length = 9)
vector_sums_WTD <- vector(length =9)
vector_sums_no3 <- vector(length =9)
vector_sums_denit <- vector(length =9)
iteration <- 1

for (a in seq(100,500,50)) {
  
  vector_a[iteration] <- a
  
  # Log transform (with natural log) all outputs to better visualize trends
  log1_output_WTD <- log(wtd_sd_3[1:a]+1)
  log1_output_no3 <- log(no3_sd_3[1:a]+1)
  log1_output_denit <- log(denit_sd_3[1:a]+1)
  
  # Re-run the sensitivity analysis with the log transformed outputs, and nBOOT = 0
  WTD_sens_log1 <- run_sensitivity(inputs_3[1:a,], log1_output_WTD, nBOOT=0, plot = FALSE)
  vector_sums_WTD[iteration] <- sum(WTD_sens_log1)
  
  no3_sens_log1 <- run_sensitivity(inputs_3[1:a,], log1_output_no3, nBOOT=0, plot = FALSE)
  vector_sums_no3[iteration] <- sum(no3_sens_log1)
  
  denit_sens_log1 <- run_sensitivity(inputs_3[1:a,], log1_output_denit, nBOOT = 0, plot = FALSE)
  vector_sums_denit[iteration] <- sum(denit_sens_log1)
  
  iteration <- iteration+1    
}

z<-1
for (z in 1:(length(vector_sums_WTD)-1)) {
  WTD_diffs[z] <- abs(vector_sums_WTD[z+1]-vector_sums_WTD[z])
  no3_diffs[z] <- abs(vector_sums_no3[z+1]-vector_sums_no3[z])
  denit_diffs[z] <- abs(vector_sums_denit[z+1]-vector_sums_denit[z])
  
  z <- z+1
}

png(paste0("SimsTest_", mytime, "_all.png"), type = "cairo", units = "in",
    height = 6.5, width = 8.5, res = 500)
par(mfrow=c(3,1), mai=c(0.1,0.5,0.4,0.1), oma=c(3, 3, 1, 1))
plot(vector_a[1:8], WTD_diffs[1:8], xlab="Simulations", ylab="d(WTD Sensitivity)", xlim= c(0,500), ylim=c(0,3), pch=16, xaxt="n")
axis(1, at=seq(0,500,100), labels=FALSE)
abline(h=0, lty=3)
mtext("(a)", 3, at=(0), cex=0.6)
plot(vector_a[1:8], no3_diffs[1:8], xlab="Simulations", ylab="d(Diss NO3 Sensitivity)",xlim= c(0,500), ylim=c(0,3), pch=16, xaxt="n")
axis(1, at=seq(0,500,100), labels=FALSE)
abline(h=0, lty=3)
mtext("(b)", 3, at=(0), cex=0.6)
plot(vector_a[1:8], denit_diffs[1:8], xlab="Simulations", ylab="d(Denitrification Sensitivity)",xlim= c(0,500),ylim=c(0,3), pch=16)
abline(h=0, lty=3)
text(250, -1, "Simulations", str=90, xpd=NA)
mtext("(c)", 3, at=(0), cex=0.6)
dev.off()