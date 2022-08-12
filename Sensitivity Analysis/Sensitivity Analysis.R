# Sensitivity analysis
# Implement the Plischke et al. sensitivity analysis method
# Code developed by Rod Lammers

library(Rcpp)
library(KernSmooth)
source("Plot Functions.R")
sourceCpp("Sensitivity_Function.cpp")

#' Implementation of the Plischke et al. density based sensitivity analysi smethod
#'
#' @param input A matrix or data.frame of model inputs
#' @param output A vector of model output
#' @param nBOOT The number of bootstrap replicates to run to correct for bias. 
#'              Default is 0 (provides uncorrected estimates) but a good choice is ~500.
#' @param plot Logical. Should the conditional and unconditional probability density estimates 
#'              be plotted for each input variable. Defaults to `FALSE`.
#' 
#' @details If the probability density  plot of your output is highly skewed, you may want to use log-transformed
#'          values to give more reliable estimates of sensitivity.
#'          
#' @return If `nBOOT` = 0, a vector of estimated sensitivity indices for each input variable.
#'         If `nBOOT` > 0, a list with the uncorrected sensitivity indices and the min, max, 1st and 3rd quartiles,
#'         and median of the bias corrected sensitivity indices.
#'
#' @export
#'
#'
run_sensitivity <- function(input, output, nBOOT = 0, plot = FALSE){
  
  #Initial parameters
  sampleN <- nrow(input)
  L <- 100
  M <- 50
  nPAR <- ncol(input)
  
  #Density plot of full output
  densityTOT <- bkde(output, gridsize = 512, range.x = c(min(output), max(output)))
  
  par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
  d <- numeric()
  dBoot <- matrix(NA, ncol = nPAR, nrow = nBOOT)
  boot_output <- matrix(NA, ncol = nPAR, nrow = 5)
  density2 <- matrix(c(densityTOT$x, densityTOT$y), ncol = 2, byrow = FALSE)

  col <- cRamp_legend(M, "Viridis")
  
  # create progress bar
  if (nBOOT > 0){
    pb <- txtProgressBar(min = 0, max = nPAR * nBOOT, style = 3)
  }
  
  for (count in 1:nPAR){
    
    #Get sample data for parameter and sort
    sample <- data.frame(input[,count], output)
    sample2 <- sample[order(sample[,1]), 2]
    
    
    Results <- Calc_Sort_C(sample2, density2, sampleN, M)
    d[count] <- Results$d
    
    if (plot){
      max_y <- max(Results$lines)
      plot(densityTOT,
           main = colnames(input)[count],
           ylim = c(0, max_y),
           ylab = "Density",
           xlab = paste("output given", colnames(input)[count]))
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray")
      for (i in 1:nrow(Results$lines)) {
        lines(densityTOT$x, Results$lines[i,], col = col[i])
      }
      lines(densityTOT, lwd = 3)
      legend("topleft", legend = c("Low", "High"), lwd = 2, col = c(col[1], col[M]), bty = "n")
    }
    
    #BOOTSTRAP
    if (nBOOT > 0){
      for (i in 1:nBOOT){
        #Resample with replacement
        Boot<-sample[sample(nrow(sample),size=sampleN,replace=TRUE),]
        Boot <- Boot[order(Boot[, 1]), 2]
        
        densityTOTboot <- density(Boot, from = min(Boot), to = max(Boot))
        densityboot2 <- matrix(c(densityTOTboot$x, densityTOTboot$y), ncol = 2,
                               byrow = FALSE)
        
        Boot.Output <- Calc_Sort_C(Boot, densityboot2,sampleN, M)
        dBoot[i, count] <- Boot.Output$d
        i <- i+1
        
        setTxtProgressBar(pb, (count - 1) * nBOOT + i)
      }
      
    }
    
    

    
  }
  
    
  if (nBOOT > 0){
    close(pb)
  }
  
  if (nBOOT == 0){
    names(d) <- colnames(input)
    return(d)
  }else{
    boot_output <- get_stats(dBoot, d)
    colnames(boot_output) <- colnames(input)
    names(d) <- colnames(input)
    return(list(d = d, boot = boot_output))
  }
  
}

#############################
get_stats <- function(boot, d) {
  stats <- apply(boot, 2, summary)
  #Drop mean
  stats <- stats[-4,]
  adj <- t(apply(stats, 1, function(x, d) {
    2 * d - x
  }, as.numeric(d)))
  colnames(adj) <- names(d)
  
  return(adj)
}

#############################
