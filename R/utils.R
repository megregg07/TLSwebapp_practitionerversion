##
## UTILITY FILE FOR THE PRACTITIONER VERSION OF WEBAPP
## UPDATED 03/16/2026 
## (new version of the app that takes a single file instead of four files)
##

##
## FUNCITON THAT TAKES AS INPUT 'R' MATRIX, AND RETURNS A 'PRETTY' VERSION OF R 
## (with target names etc., for the purpose of making plots to check for problem targets)
##

create_pretty_R <- function(M, data_name) {
  ## determine how many targets and positions there are
  nT <- nrow(M)
  nP <- ncol(M)/3
  
  M_pretty <- data.frame(
    data = data_name, 
    target = paste0("Target_", str_pad(1:nT, width = 2, side = "left", "0")), 
    theta = as.numeric(as.matrix(M[,c(seq(1,ncol(M), by = 3))])), ## columns 1,4,7,10 when there are four positions
    phi = as.numeric(as.matrix(M[,c(seq(2,ncol(M), by = 3))])),   ## columns 2,5,8,11 when there are four positions
    r = as.numeric(as.matrix(M[,c(seq(3,ncol(M), by = 3))])),     ## columns 3,6,9,12 when there are four positions
    position = rep(paste0("position",1:nP), each = 20)
  )
  ## convert angular residuals from radians to arcseconds
  M_pretty <- M_pretty %>%
    mutate(theta = theta*((3600*180)/pi), 
           phi = phi*((3600*180)/pi))
  
  return(M_pretty)
}

##
## Function that takes as input the 'M_pretty' data frame and makes one plot, faceted,
## one for each of theta, phi, and r
##
plot_my_residuals <- function(Mp) {
  dname <- Mp$data[1]
  
  my_plot <- Mp %>%
    pivot_longer(cols = theta:r, names_to = "variable", values_to = "residual") %>%
    mutate(variable = factor(variable, levels = c('theta', 'phi', 'r'))) %>%
    ggplot(aes(x = as.factor(target), y = residual)) +
    facet_wrap(vars(variable), nrow = 3, scales = "free_y") + geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Target") + ggtitle(dname)
  
  return(my_plot)
}

##
## DATA ELLIPSE FUNCTION
## Either make one plot (if the Xdat data only has theta/phi/dataset columns), 
## or make the three-panel plot analogous to the covellipse() function. 
## But these data ellipses include the residual points
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


##
## Function that takes as input Zc and returns
## the number of targets and number of TLS positions
##
TLS_getTandP <- function(dat) {
  ## determine number of targets
  nT <- nrow(dat)
  ## determine the number of TLS positions
  nP <- ncol(dat)/3
  return(list(nTargets = nT, nPositions = nP))
}

##
## function to check/convert tape data to mm, if necessary
##
Dtape_ToMM <- function(dat, units = c('mm', 'cm', 'inches')) {
  units <- match.arg(units)
  if(units=='inches') {
    return(dat %>%
             dplyr::mutate(Length = Length*25.4))
  } else if (units=='cm') {
    return(dat %>% 
             dplyr::mutate(Length = Length*10))
  } else{
    return(dat)
  }
}


##
## Function that takes as input the Zc file (WITH target names) and converts to mm, if necessary
## 
convertZc_tomm <- function(Zc_input, units = c("mm", "meters", "inches", "feet")){
  ## determine the units
  units <- match.arg(units)
  ## split data and target list (first column)
  Zc <- Zc_input[,-1]
  target_list <- Zc_input[,1]
  ## check units, and convert to mm if necessary
  ## ...
  Zc <- switch(units, 
               mm = Zc, 
               meters = Zc*1000, 
               inches = Zc*25.4, 
               feet = Zc*304.8)
  return(list(Zc = Zc, target_list = target_list))
}


##
## Purpose: calculate the T2T lengths from TLS coordinates
##
## Inputs: 
##   Zc         - nx(3*p) matrix of (xyz) coordinates
##   i,j        - numberic values indicating the rows (targets) of Zc that define the length
##   ref_length - the reference length for the length
##
## Output: 
##   errorTab   - Px3 table with rows corresponding to Position, 
##                giving the TLS length estimate (for each position) and the calculated length error 

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

##
## function that takes as input the table of length errors (6 lengths per position) 
## and a threshold value, and returns which (absolute) errors exceeded the threshold +
## a statement
##

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
      statement <- paste("All absolute length errors are smaller than the specified threshold of", t_val, "mm.")
      
    } else{
      statement <- paste(nrow(table_subset), "out of the", nrow(table), "absolute length errors are larger than the specified 
                         threshold of", 
                         t_val, "mm.")
    }
  } else { ## otherwise, we're reporting as a percentage..so do all the same things, except for absolute percentage
    table_sorted <- table %>% arrange(desc(abs(PctError)))
    table_subset <- table_sorted[abs(table_sorted$PctError) > t_val,]
    if(nrow(table_subset)==0){
      statement <- paste("All length errors are smaller than the specified threshold of", t_val, "% of the reference length.")
      
    } else{
      statement <- paste(nrow(table_subset), "out of the", nrow(table), "length errors are larger than the specified threshold of", 
                         t_val, "% of the reference length.")
    }
  }
  return(list(table = table_subset, statement = statement))
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

##
## Function that performs rigid body transformation
## X = 6x1 vector of parameters
## A = data to be moved
## flag = indicator (if 1, transform A to base frame; if 2, transform to frame A)
## 
RigidBodyTransform <- function(X,A, flag) {
  n = nrow(A)
  cx = X[1]
  cy = X[2]
  cz = X[3]
  alpha = -X[4]
  beta = -X[5]
  gamma = -X[6]
  
  ## hardcode the RxRyRz matrix multiplication result
  htm = matrix(c(
    cos(beta)*cos(gamma), cos(beta)*sin(gamma), -sin(beta), 
    sin(alpha)*sin(beta)*cos(gamma) - cos(alpha)*sin(gamma), sin(alpha)*sin(beta)*sin(gamma) + cos(alpha)*cos(gamma), sin(alpha)*cos(beta), 
    cos(alpha)*sin(beta)*cos(gamma) + sin(alpha)*sin(gamma), cos(alpha)*sin(beta)*sin(gamma) - sin(alpha)*cos(gamma), cos(alpha)*cos(beta)
  ), nrow = 3, byrow = T)
  htm = rbind(cbind(htm, c(cx, cy, cz)), c(0,0,0,1))
  ## add in a column of 1s to the data input
  A_aug = cbind(as.matrix(A), rep(1,n))
  if(flag==1) {
    A_relocated = t(htm%*%t(A_aug))
  } else {
    A_relocated = t(ginv(htm)%*%t(A_aug))
  }
  
  return(A_relocated[,1:3])
}


##
## Function to be input into 'optim' to obtain the rigid body transformation
## multi-position parameters
RigidBodyTransformWrapper_multi <- function(X, Zc, n, m){
  ## take the input vector X and add in 6 zeros (the first set of data will not be moved)
  X <- c(rep(0,6), X)
  ## storage for transformed data
  ZcT_tmp <- matrix(NA, nrow = n, ncol = 3*m)
  ## cycle through the positions and apply RigidBodyTransform (transform the positions 2-4 data to position 1)
  for(i in 1:m) {
    params_i <- X[(6*i-5):(6*i)]
    ZcT_tmp[,((3*i)-2):(3*i)] <- RigidBodyTransform(params_i, Zc[,((3*i)-2):(3*i)], 1)
  }
  ## calculate composite coordinates by averaging
  composite <- cbind(apply(ZcT_tmp[,seq(1, 3*m, by = 3)],1,mean, na.rm = T), 
                     apply(ZcT_tmp[,seq(1, 3*m, by = 3)+1],1,mean, na.rm = T), 
                     apply(ZcT_tmp[,seq(1, 3*m, by = 3)+2],1,mean, na.rm = T))
  
  for(i in 1:m){
    cols_i <- ((3*i)-2):(3*i)
    error <- ZcT_tmp[,cols_i] - composite
    if(i==1){
      Fvec <- as.numeric(error)
    } else {
      Fvec <- c(Fvec, as.numeric(error))
    }
  }
  
  sum(Fvec^2, na.rm = T)
}


## 
## Function to be input into 'optim' to obtain the RBT parameters 
## for a single transformation
##
RigidBodyTransformWrapper_single <- function(par, data_tbm, base_data, flag) {
  moved_data <- RigidBodyTransform(X = par, A = data_tbm, flag = flag)
  errors <- moved_data - base_data
  sum(as.numeric(as.matrix(errors))^2, na.rm = T)
}

##
## Function to get intial starting parameters 
## for multi-position RBT 'optim'
##
get_initial_params <- function(Zc) {
  nP <- ncol(Zc)/3
  
  dat_p1 <- Zc[,1:3]
  
  for(p in 2:nP) {
    dat_p <- Zc[,((3*p)-2):(3*p)]
    params_p <- optim(par = c(0,0,0,0,0,0), method = 'BFGS', control = c(reltol = 1e-15),
                      fn = RigidBodyTransformWrapper_single, data_tbm = dat_p, base_data = dat_p1, 
                      flag = 1)$par
    if(p==2) {
      paramsHat <- params_p
    } else{
      paramsHat <- c(paramsHat, params_p)
    }
  }
  return(paramsHat)
}

##
## Revised 'Zc_to_R' function that performs the multi-position least-squares
## optimization using 'optim', rather than the one-at-a-time SVD transformation
##
Zc_to_R <- function(Zc) {
  ## determine the number of positions
  nP <- ncol(Zc)/3
  ## determine the number of targets
  nT <- nrow(Zc)
  
  
  ## STEP 1: 
  ##      Get initial starting parameters (the one-at-a-time estimates)
  ##      using the 'get_initial_params()' function
  X0 <- get_initial_params(Zc)
  
  ## STEP 2: 
  ##       Obtain the multi-position parameter estimates
  paramsHat <- optim(par = X0, method = 'BFGS', control = c(reltol = 1e-15),
                     fn = RigidBodyTransformWrapper_multi, Zc = Zc, n = nT, 
                     m = nP)$par
  ## add in 0s to represent no transformation of the position 1 data
  paramsHat <- c(rep(0,6), paramsHat)
  
  ## STEP 3:
  ##      Obtain ZcT by applying the parameters obtained in Step 1 to the Zc data
  ##      Build up ZcT by starting with position 1 data (which remains unmoved)
  ZcT <- Zc[,1:3] ## keep Position 1 data as is
  ## for every other position, convert to the Position 1 frame of reference and save as a new set of columns in ZcT
  for(p in 2:nP) {
    pos_p <- Zc[,(3*p-2):(3*p)]  ## get data from the p'th position
    params_p <- paramsHat[(6*p-5):(6*p)]  ## for the p'th position, obtain the parameters from the paramsHat vector
    ZcT[,(3*p-2):(3*p)] <- RigidBodyTransform(X = params_p, pos_p, flag = 1)
  }
  
  ## STEP 4: 
  ##      Obtain the composite coordinates by averaging across the four positions in ZcT
  composite <- cbind(apply(ZcT[,seq(1, 3*nP, by = 3)],1,mean, na.rm = T), 
                     apply(ZcT[,seq(1, 3*nP, by = 3)+1],1,mean, na.rm = T), 
                     apply(ZcT[,seq(1, 3*nP, by = 3)+2],1,mean, na.rm = T))
  
  ## STEP 5: 
  ##      Translate the composite coordinates back to the individual frames of reference
  ##      using the optimal parameters obtained in Step 1
  Kc <- data.frame(matrix(data= '', nrow=nT , ncol= nP*3))
  for(p in 1:nP) {
    cols <- (3*p-2):(3*p)
    if(p==1) {
      Kc[,cols] <- composite 
    } else {
      ## perform Rigid Body Transformation to the composite coordinates in the opposite direction
      params_p <- paramsHat[(6*p-5):(6*p)]  ## for the p'th position, obtain the parameters from the paramsHat vector
      Kc[,cols] <- RigidBodyTransform(X = params_p, composite, flag = 2)
    }
  }
  Kc <- as.matrix(Kc)
  
  
  ## STEP 6: 
  ##      CONVERT THE ORIGINAL CARTESIAN COORDINATES (individual frames of reference) 
  ##      AND THE COMPOSITE COORDINATES (expanded to the individual frames of reference)
  ##      TO SPHERICAL COORDINATES
  ##
  ## convert Zc to Zp
  ## convert Kc to Kp
  Zp <- data.frame(matrix(data= '', nrow=nT , ncol= nP*3))
  Kp <- data.frame(matrix(data= '', nrow=nT , ncol= nP*3))
  for(p in 1:nP) {
    cols <- (3*p-2):(3*p)
    Zp[,cols] <- cart2sph(as.matrix(Zc[,cols]))
    Kp[,cols] <- cart2sph(as.matrix(Kc[,cols]))
  }
  
  ## STEP 6: CALCULATE THE SPHERICAL RESIDUALS (finally...)
  R <- Zp - Kp
  
  ## Return the matrix of spherical residuals
  return(R)
}


##**************************
## data processing function
##   input:  R  data separated by position (e.g., Tx(3*P))
##   output: X  data in long form, ignoring target/position (e.g., (TxP)x3)
##
## ASSUMING TRIVARIATE DATA 

RtoX <- function(Rmat) {
  nP <- ncol(Rmat)/3
  colseq <- seq(1, nP*3, by = 3) ## denotes the 1st, 4th, 7th, etc. columns
  Xmat <- data.frame(
    theta = as.numeric(as.matrix(Rmat[,colseq]))*((3600*180)/pi),  ## converts radians to arcseconds
    phi = as.numeric(as.matrix(Rmat[,colseq+1]))*((3600*180)/pi),  ## converts radians to arcseconds
    r = as.numeric(as.matrix(Rmat[,colseq+2]))                     ## range residuals already in mm
  )
  return(Xmat)
}

###################################################
## FUNCTION APPLYING R-OMNIBUS TEST
## Testing equality of variance for 
##   bivariate or trivariate data
##

## Function to calculate Z values - this will be applied to the median-centered data 
## within the 'Romni_fun' functions -- used within 'TLS_cov_check()' 
Zfun <- function(dmat) {
  ## established empty data frame
  Z <- data.frame(matrix(NA, nrow = nrow(dmat), ncol = ncol(dmat)*(ncol(dmat)+1)/2))
  ## cycle through multiplying column 1 times columns 1-k, column 2 times columns 2-k, etc. 
  Zi <- 1
  for (k in 1:ncol(dmat)) {
    Xp <- dmat[,k]
    for(kp in k:ncol(dmat)) {
      Xpk <- dmat[,kp]
      Z[,Zi] <- Xp*Xpk
      Zi <- Zi+1
    }
  }
  return(Z)
}

TLS_cov_check <- function(dmat1, dmat2, conf.level = 0.95) {
  ## check that the two matrices have the same number of variables
  if(ncol(dmat1)!=ncol(dmat2)) {stop('data sets must have the same number of variables')}
  ## check that there are only 2 or 3 variables 
  ## (we don't need this to work for > 3 variables)
  continue <- 1*(ncol(dmat1)%in% c(2,3))
  if(!continue) {stop('data must be bivariate or trivariate')}
  ## check that the confidence level is numeric and a value between 0 and 1
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || 
                               conf.level < 0 || conf.level > 1)) 
    stop("'conf.level' must be a single number between 0 and 1")
  
  ## Step 0: remove any missing values
  na1 <- which(is.na(dmat1[,1]))
  na2 <- which(is.na(dmat2[,1]))
  if(length(na1)>0) {
    dmat1 <- dmat1[-which(is.na(dmat1[,1])),]
  }
  if(length(na2)>0) {
    dmat2 <- dmat2[-which(is.na(dmat2[,1])),]
  }
  
  
  ## Step 1: median-center the data
  M1 <- apply(dmat1, 2, median)
  M2 <- apply(dmat2, 2, median)
  D1_prime <- dmat1 - data.frame(matrix(rep(M1, nrow(dmat1)), ncol = ncol(dmat1), byrow = T))
  D2_prime <- dmat2 - data.frame(matrix(rep(M2, nrow(dmat2)), ncol = ncol(dmat2), byrow = T))
  
  ## Step 2: Calculate the Z values using 'Zfun'
  Z1 <- Zfun(D1_prime)
  Z2 <- Zfun(D2_prime)
  
  ## Step 3: calculate the W values
  W1 <- apply(Z1, 2, function(x) sign(x)*sqrt(abs(x)))
  W2 <- apply(Z2, 2, function(x) sign(x)*sqrt(abs(x)))
  
  ######################
  ## R-omnibus test
  ## Step 4: apply the Hotelling T2 test to the transformed data, and save the p-value
  alpha <- 1 - conf.level
  HT2 <- HotellingsT2(W1, W2)
  statistic <- HT2$statistic
  pval <- HT2$p.value
  parameter <- HT2$parameter
  
  method <- "Robust Omnibus Test - Hotelling's two sample T2-test"
  RESULT <- ifelse(pval > alpha, 'PASSES', 'FAILS')
  conclusion <- paste('The instrument', RESULT, 'at the', conf.level, 'confidence level.')
  interpretation <- paste("There is", ifelse(RESULT=="PASSES", 'insufficient', 'sufficient'), 
  "evidence to conclude that the instrument's precision has significantly changed.")
  
  #rval <- list(method = method, statistic = statistic, parameter = parameter, 
  #             p.value = pval, conclusion = conclusion, 
  #             data.name = conclusion)
  #class(rval) <- "htest"
  rval <- list(results = HT2, 
               conclusion = conclusion, 
               interpretation = interpretation)
  rval
  #HT2
}

