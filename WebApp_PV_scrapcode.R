######################################################################
## Scrap code while updating the practitioner version of the webapp
##
## 03/16/2026
######################################################################

library(shiny)
library(shinythemes)
library(ICSNP)     ## for Hotellings T2 test
library(heplots)   ## for visualizing data ellipses
library(dplyr)     ## %>% transformations
library(geometry)  ## for 'cart2sph' function
library(MASS)      ## for mvrnorm() 
library(ggplot2)
library(tidyr)
library(tools)     ## for determining file extension
library(patchwork) ## for plotting panels together
library(Cairo) ## for saving pdf with ascii characters (e.g., 'theta')

##**USE THIS WHEN ON MY COMPUTER
source("C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/R/utils.R")


##
## Purpose: create large table of length errors for Lengths A-F
##
## 
## Function that takes as input:
##  Dtape   File input by user with tape measure info
##  Zc      Cart. coordinates from 'test' data
##  Zc_names  List of target names, whose position in the vector align with the targets' data position in Zc
##
##  Output
lengthErrors <- function(Dtape, Zc, Zc_names){
  ## First, check that the target names match
  if(sum((1-1*(Dtape[,2] %in% Zc_names)) + (1-1*(Dtape[,3] %in% Zc_names)))!=0){
    stop(print("Target names in Reference Length data need to match those in TLS Test Data"))
  } else{
    ## If target names are ok, go ahead and cycle through each length ID 
    for(l in 1:length(Dtape[,1])){
      ## Determine which letter
      row_l <- Dtape[l,]
      ## Determine the numeric position of the targets that define the length
      targets_l <- which(Zc_names %in% row_l[2:3])
      # apply the lengthError_table function and save the full dataframe
      LengthErrors_l <- data.frame(
        ID = row_l[,1], 
        lengthError_table(Zc = Zc, i = targets_l[1], j = targets_l[2], ref_length = as.numeric(row_l[4]))
      )
      statement_l <- data.frame(
        ID = row_l[,1], 
        statement = paste0("Length ", row_l[,1], " is defined by ", Zc_names[targets_l[1]], " and ", 
                           Zc_names[targets_l[2]], ". The reported reference measurement of Length ", 
                           row_l[,1], " is ", row_l[4], " mm.")
      )
      if(l==1) {
        AllLengthErrors <- LengthErrors_l
        AllStatements <- statement_l
      } else {
        AllLengthErrors <- rbind(AllLengthErrors, LengthErrors_l)
        AllStatements <- rbind(AllStatements, statement_l)
      }
    }
    return(list(table = AllLengthErrors, statements = AllStatements))
  }
}

lengthError_table <- function(Zc, i, j, ref_length) {
  n_pos <- ncol(Zc)/3
  
  lengthHat <- NA
  for(p in 1:n_pos){
    cols_p <- (3*p-2):(3*p)
    lengthHat[p] <- dist(Zc[c(i,j), cols_p])
  }
  ## calculate the error (difference b/t observed length and the reference length)
  errorHat <- lengthHat - ref_length
  ## put it all in a data frame
  errorTab <- data.frame(
    ReferenceLength = ref_length,
    Position = paste("Position", 1:n_pos), 
    Length = round(lengthHat,4),
    Error = round(errorHat,3), 
    PctError = round(errorHat/ref_length*100,2)
  )
  ## and return the dataframe
  return(errorTab)
}

##
## Function that will plot the Part I length errors
##
partIerrorplot <- function(dat, threshold= NA, plot_as = c('mm', 'pct')) {
  plot_as <- match.arg(plot_as)
  
  ## if we're plotting the errors in mm, then the 'y' value will be "Error"
  if(plot_as=='mm') {
    errorplot <- ggplot(dat, aes(x = ReferenceLength, y = Error, color = as.factor(ID))) +
      geom_point() +
      scale_y_continuous(name = "Error (mm)") +
      scale_x_continuous(name = "Reference length (mm)") + 
      geom_hline(yintercept = 0, color = "grey20", linetype = "dotted") +
      ## make the legend title better
      guides(color=guide_legend(title="Length ID"))
    ## if the user has input a threshold, add that error threshold to the plot
    if((1-is.na(threshold)) & is.numeric(threshold)) {
      errorplot <- errorplot + 
        geom_hline(yintercept = threshold, color = "grey20", linetype = "dashed") +
        geom_hline(yintercept = -threshold, color = "grey20", linetype = "dashed")
    }
    ## if we're plotting the errors as a pct of the reference length, then the 'y' value will be "PctError"
  } else {
    ## calculate what the error is as a percentage of the reference length
    errorplot <- ggplot(dat, aes(x = ReferenceLength, y = PctError, color = as.factor(ID))) +
      geom_point() +
      scale_y_continuous(name = "Error percentage of reference length (%)") +
      scale_x_continuous(name = "Reference length (mm)") + 
      geom_hline(yintercept = 0, color = "grey20", linetype = "dotted") +
      ## make the legend title better
      guides(color=guide_legend(title="Length ID"))
    ## if the user has input a threshold, add that error threshold to the plot
    if((1-is.na(threshold)) & is.numeric(threshold)) {
      errorplot <- errorplot + 
        geom_hline(yintercept = threshold, color = "grey20", linetype = "dashed") +
        geom_hline(yintercept = -threshold, color = "grey20", linetype = "dashed")
    }
    
  }
  return(errorplot)
}

lengthError_statement <- function(table, t_val, report_as = c('mm', 'pct')) {
  report_as <- match.arg(report_as)
  ## if we're reporting in mm, do stuff in 'mm'
  if(report_as =='mm') {
    ## sort the table based on largest absolute error
    table_sorted <- table %>% arrange(desc(abs(Error)))
    ## Subset the table, keeping only the values whose absolute error is bigger than the threshold
    table_subset <- table_sorted[abs(table_sorted$Error) > t_val,]
    ## If there are no abs errors that are larger than the threshold, return a statement saying so
    if(nrow(table_subset)==0){
      statement <- paste("All absolute length errors are smaller than", t_val, "mm.")
    
    } else{
      statement <- paste(nrow(table_subset), "out of the", nrow(table), "absolute length errors are larger than", 
                        t_val, "mm.")
    }
  } else { ## otherwise, we're reporting as a percentage..so do all the same things, except for absolute percentage
    table_sorted <- table %>% arrange(desc(abs(PctError)))
    table_subset <- table_sorted[abs(table_sorted$PctError) > t_val,]
    if(nrow(table_subset)==0){
      statement <- paste("All length errors are smaller than", t_val, "% of the reference length.")
      
    } else{
      statement <- paste(nrow(table_subset), "out of the", nrow(table), "length errors are larger than", 
                         t_val, "% of the reference length.")
    }
    }
  return(list(table = table_subset, statement = statement))
}



##******************************************************************
##**PART I - ALLOW 'THRESHOLD' TO BE A % OF THE REFERENCE LENGTH**
##

## import the TLS data
Zc_1tmp <- read.csv("C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PostCalibration/RTC360_Post_mm.csv")

Zc <- Zc_1tmp[,-1]
Zc_names <- Zc_1tmp[,1]

## import the reference length data
Dtape <- read.csv("C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/TapeData/RTC360_TapeData.csv")

lengthErrors <- lengthErrors(Dtape = Dtape, Zc = Zc_1tmp[,-1], Zc_names = Zc_1tmp[,1])



lengthError_statement(lengthErrors$table, t_val = 2, report_as = "pct")
## probably here, add a new column to 'lengthErrors' table that calculates the error as a percentage of the reference length
## ...which might mean adding a 'ReferenceLength' to the table?

windows()
ggplot(lengthErrors, aes(x = ReferenceLength, y = ErrorPct, color = as.factor(ID))) + 
  geom_point() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey20") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "grey20")

## if no 'report as' is given, it will plot errors in mm
windows(6,4)
partIerrorplot(dat = lengthErrors$table, threshold = 2)
## if no threshold is given, no error lines will be plotted
partIerrorplot(dat = lengthErrors$table, plot_as = "pct")
## and now, report as percentage and give a threshold
partIerrorplot(dat = lengthErrors$table, plot_as = "pct", threshold = 0.5)


##**************************************************************************************
##**PART II - WRITE CUSTOM DATA ELLIPSE FUNCTION (so the residual points are plotted)*

##
## READ IN TWO DATA SETS
Zc_1tmp <- read.csv("C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PostCalibration/RTC360_Post_mm.csv")
Zc_2tmp <- read.csv("C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PreCalibration/RTC360_Pre_mm.csv")

## remove the 'TargetID' column
Zc_1 <- Zc_1tmp[,-1]
Zc_2 <- Zc_2tmp[,-1]

## convert cartesian coordinates to spherical residuals
R1 <- Zc_to_R(Zc_1)
R2 <- Zc_to_R(Zc_2)

## transform the residuals from wide to long
X1 <- RtoX(R1)
X2 <- RtoX(R2)

## combine, adding in a name column
Xcomb <- rbind(data.frame(X1, dataset = 'PostCalibration'), 
               data.frame(X2, dataset = 'PreCalibration'))


## THIS FUNCTION WILL EITHER MAKE ONE PLOT (IF THE XDAT HAVE ONLY theta and phi, or the three-panel plot
## analigous to the covellipse() function). But it will include the residual points
##
## **data input must be in the form 
##      theta phi r datasetname
my_ellipse_plot <- function(Xdat, r = 2, col_list = c("#1F77B4", "#FF7F0E")) {
  ## rename the last column to 'dataset' (this is the dataset ID)
  colnames(Xdat)[ncol(Xdat)] <- "dataset"
  radius <- r
  nsegments <- 100
  angles <- (0:nsegments) * 2 * pi/nsegments
  unit.circle <- cbind(cos(angles), sin(angles))
  
  ## subset the two sets of data
  Xtmp1 <- subset(Xdat, Xdat[,ncol(Xdat)]==unique(Xdat[,ncol(Xdat)])[1])
  Xtmp2 <- subset(Xdat, Xdat[,ncol(Xdat)]==unique(Xdat[,ncol(Xdat)])[2])
  
  ## calculate the two covariance matrices
  sigmaHat1 <- cov(Xtmp1[,-ncol(Xtmp1)])
  sigmaHat2 <- cov(Xtmp2[,-ncol(Xtmp2)])
  
  ## (IF THE DIMENSION OF 'Xdat' is 3, then we're only making one plot)
  ## We always make this plot...but if this is the only plot, then we need a legend
  ellipse1_1 <- data.frame(radius*(unit.circle %*% chol(sigmaHat1)[c(1,2), c(1,2)]))
  ellipse1_2 <- data.frame(radius*(unit.circle %*% chol(sigmaHat2)[c(1,2), c(1,2)]))
  
  if(ncol(Xdat)==(2+1)) {
    p1 <- ggplot(Xdat, aes(x = phi, y = theta, color = dataset)) + geom_point(pch = 20, size = 2) +
      labs(x = "\u03c6 residual (arcseconds)", y = "\u03b8 residual (arcseconds)") +
      scale_color_manual(values = col_list)+
      guides(color=guide_legend(title="Data")) +
      theme(legend.position = "top") +
      geom_path(data = ellipse1_1, color = col_list[1]) +
      geom_path(data = ellipse1_2, color = col_list[2]) +
      theme(text = element_text(size = 18))
    
    ellipsePlot <- p1
  } else{
    ## same first plot, but suppress the legend
    p1 <- ggplot(Xdat, aes(x = phi, y = theta, color = dataset)) + geom_point(pch = 20, size = 2) +
      labs(x = "\u03c6 residual (arcseconds)", y = "\u03b8 residual (arcseconds)") +
      scale_color_manual(values = col_list)+
      guides(color = "none") +
      geom_path(data = ellipse1_1, color = col_list[1]) +
      geom_path(data = ellipse1_2, color = col_list[2]) +
      theme(text = element_text(size = 18))
    ## and create the other two plots
    ##
    ## Panel 2: Theta vs. r (rows/columns 1&3)
    ellipse2_1 <- data.frame(radius*(unit.circle %*% chol(sigmaHat1)[c(1,3), c(1,3)]))
    ellipse2_2 <- data.frame(radius*(unit.circle %*% chol(sigmaHat2)[c(1,3), c(1,3)]))
    
    p2 <- ggplot(Xdat, aes(x = r, y = theta, color = dataset)) + geom_point(pch = 20, size = 2) +
      labs(x = "r residual (mm)", y = "\u03b8 residual (arcseconds)") +
      scale_color_manual(values = col_list) +
      guides(color = "none") +
      geom_path(data = ellipse2_1, color = col_list[1]) +
      geom_path(data = ellipse2_2, color = col_list[2]) +
      theme(text = element_text(size = 18))
    
    ## Panel 3: Phi vs. r (rows/columns 2&3)
    ellipse3_1 <- data.frame(radius*(unit.circle %*% chol(sigmaHat1)[c(2,3), c(2,3)]))
    ellipse3_2 <- data.frame(radius*(unit.circle %*% chol(sigmaHat2)[c(2,3), c(2,3)]))
    
    p3 <- ggplot(Xdat, aes(x = r, y = phi, color = dataset)) + geom_point(pch = 20, size = 2) +
      labs(x = "r residual (mm)", y = "\u03c6 residual (arcseconds)") +
      scale_color_manual(values = col_list) +
      guides(color=guide_legend(title="Data", ncol = 1)) +
      geom_path(data = ellipse3_1, color = col_list[1]) +
      geom_path(data = ellipse3_2, color = col_list[2]) +
      theme(legend.position=c(-0.85, .85)) +
      theme(text = element_text(size = 18))
    
    ellipsePlot <- (p1 + p2) / (plot_spacer() + p3 )
  }
  return(ellipsePlot)
}

## plot just the angular residuals
windows()
my_ellipse_plot(Xcomb[,-3], col_list = c('purple', 'orange'))

windows()
my_ellipse_plot(Xcomb)
