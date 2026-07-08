README FILE FOR TLS WEBAPP - EXAMPLE DATA 1

## 
## TOY SCENARIO
## Instrument Model RTC360, serial number ending in 111, is tested in December 2025.  
## The following data are collected:
##   1a. 'RTC360_2025December_mm.csv'
##       These are the TLS Cartesian coordinates (in mm) collected from four positions. 
##   1b. 'TapeData_2025December.csv'
##       Reference lengths (in mm) collected by tape at the time of the December IPA.
##
## The organization has a policy that TLSs will undergo periodic evaluation every four months, and 
## has a dedicated testing setup. The target array remains the same, but for the April 2026 IPA 
## a different set of six reference lengths were used
## In accordance with this policy, the same instrument is tested again in April 2026. 
## At this time, the following data are collected:
##   2a. 'RTC360_2026April_mm.csv'
##       These are the TLS Cartesian coordinates (in mm) collected from the Regional Office testing setup.
##   2b. 'TapeData_2026April.csv'
##       Reference lengths (in mm) collected by tape at the time of the April IPA.
##
## NOTE:
## The organization has a dedicated testing setup, so the number of targets between the two TLS data
## sets is the same and the naming scheme remains constant. However, between December 2025 and April 2026
## the six specific lengths that define the reference lengths were changed to make the tape measuring process
## easier. Therefore, the targets that define the six specific length have changed between 
## 'TapeData_2025December.csv' and 'TapeData_2026April.csv'.
##
## PURPOSE OF THIS EXAMPLE:
## 1. Simple illustration where there is no change in the instrument precision
## 2. Same test setup results in same number of targets, with the same naming scheme
##


##
## HOW THESE DATA WERE COLLECTED
## 
'RTC360_2025December_mm.csv'    Based on Ken's Precalibration data from 2025 NIST Testing Day
                                (TargetID column added)
'TapeData_2025December.csv'     The actual reference lengths collected by Ken


'RTC360_2026April_mm.csv'	Ken's Postcalibration data from 2025 NIST Testing data
				(TargetID column added)
'TapeData_2026April.csv'  	Created by hand by picking six lengths from the Position 1 data