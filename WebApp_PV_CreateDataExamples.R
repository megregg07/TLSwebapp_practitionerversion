###########################################
## 03/13/2026
##
## CREATE EXAMPLE DATA FOR WEBAPP - PRACTITONER VERSION
###########################################



##**still working with 4 files...need to figure out dynamic number of files later**

##
## Function that takes as input the four individual position files, and the reported TLS units,
## and returns as output the Zc data structure, converted to mm if necessary
## 
##**MAJOR CHANGE: THE TARGET NAMES ARE RETAINED**
##
four_to_one <- function(f1, f2, f3, f4, units = c("mm", "meters", "inches", "feet")){
  ## determine the units
  units <- match.arg(units)
  ## ensure the files have the target ID column as "Target"
  colnames(f1) <- colnames(f2) <- colnames(f3) <- colnames(f4) <- c("TargetID", "X", "Y", "Z")
  
  data_cbind <- merge(merge(merge(f1, f2, by = "TargetID", suffixes = c('.p1', '.p2')), 
                            f3, by = "TargetID"), 
                      f4, by = "TargetID", suffixes = c('.p3', '.p4'))
  ## split data and target list (first column)
  Zc <- data_cbind[,-1]
  target_list <- data_cbind[,1]
  ## check units, and convert to mm if necessary
  ## ...
  Zc <- switch(units, 
               mm = Zc, 
               meters = Zc*1000, 
               inches = Zc*25.4, 
               feet = Zc*304.8)
  my_file <- cbind(target_list, Zc)
  colnames(my_file) <- c("TargetID", rep(c('X','Y','Z'),4))
  return(my_file)
}

##
##**PRECALIBRATION:**
## READ IN THE INDIVIDUAL FILES
##
f1 <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PreCalibration/individual files/RTC360_Pre_p1.csv')
f2 <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PreCalibration/individual files/RTC360_Pre_p2.csv')
f3 <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PreCalibration/individual files/RTC360_Pre_p3.csv')
f4 <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PreCalibration/individual files/RTC360_Pre_p4.csv')

mydata_pre <- four_to_one(f1, f2, f3, f4, units = 'mm')
## save this file
write.csv(mydata_pre,'C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PreCalibration/RTC360_Pre_mm.csv', 
          row.names = F)

##
##**POSTCALIBRATION:**
## READ IN THE INDIVIDUAL FILES
##
f5 <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PostCalibration/individual files/RTC360_Post_p1.csv')
f6 <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PostCalibration/individual files/RTC360_Post_p2.csv')
f7 <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PostCalibration/individual files/RTC360_Post_p3.csv')
f8 <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PostCalibration/individual files/RTC360_Post_p4.csv')

mydata_post <- four_to_one(f5, f6, f7, f8, units = 'mm')
## save this file
write.csv(mydata_post,'C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PostCalibration/RTC360_Post_mm.csv', 
          row.names = F)

aa <- read.csv('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/PostCalibration/RTC360_Post_mm.csv', row.names = NULL)
#######################
##
## CREATE EXAMPLE DATA
## Create and save the four files (for each timepoint)
## the user will presumably start with 
##     TargetID x y z
##    [              ]
##    [              ]
##
#######################
##*********************
##* PRE-CALIBRATION
##*********************
p1 <- read.table('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/data/MeasurementWeek/Leica/RTC360/PreCalibration/Position_1_Reordered.txt')
p2 <- read.table('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/data/MeasurementWeek/Leica/RTC360/PreCalibration/Position_2_Reordered.txt')
p3 <- read.table('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/data/MeasurementWeek/Leica/RTC360/PreCalibration/Position_3_Reordered.txt')
p4 <- read.table('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/data/MeasurementWeek/Leica/RTC360/PreCalibration/Position_4_Reordered.txt')

##*********************
##* POST-CALIBRATION
##*********************
p5 <- read.table('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/data/MeasurementWeek/Leica/RTC360/PostCalibration/Position_1_Reordered.txt')
p6 <- read.table('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/data/MeasurementWeek/Leica/RTC360/PostCalibration/Position_2_Reordered.txt')
p7 <- read.table('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/data/MeasurementWeek/Leica/RTC360/PostCalibration/Position_3_Reordered.txt')
p8 <- read.table('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/data/MeasurementWeek/Leica/RTC360/PostCalibration/Position_4_Reordered.txt')


## Cycle through each file and add in a "Target" column
for(f in 1:8) {
  dat_f <- get(paste0("p", f))
  tdat <- data.frame(
    Target = paste0("Target_", 1:nrow(dat_f)), 
    ## CONVERT HERE IF DESIRED...original files are in mm. Keep it that way for now. 
    ##dat_f/25.4  ## convert to inches
    dat_f
  )
  colnames(tdat)[2:4] <- c("X", "Y", "Z")
  ## write the file to the appropriate place...as a .csv
  if(f < 5) {
    folder <- "Pre"
    n <- f
  } else {
    folder <- "Post"
    n <- f-4
  }
  ## determine the file name/location
  fn <- paste0("RTC360_", folder, "_p", n, ".csv")
  loc <- paste0('C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/ExampleData/', 
                folder, "Calibration/individual files/", fn)
  ## and save the file
  write.csv(tdat, loc, row.names = FALSE)
}
