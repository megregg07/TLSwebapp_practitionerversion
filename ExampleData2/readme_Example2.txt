README FILE FOR TLS WEBAPP - EXAMPLE DATA 2

## 
## TOY SCENARIO
## Instrument Model LSS, serial number ending in 999, is tested at the Home Office in January 2026. 
## The following data are collected:
##   1a. 'LSS999_Jan2026.csv'
##       This are the TLS Cartesian coordinates (in mm) collected from four positions in the testing environment. 
##   1b. 'RefLengths_Jan2026.csv'
##       These are the reference lengths (in mm) collected by tape at the January 2026 testing. 
##
## Five months later, in May, the instrument is shipped to the Regional Office. Upon arrival, the state of the shipping 
## container had some exterior damage. The IPA is run again to test the instrument's operational status.
## The following data are collected:
##   2a. 'LSS999_May2026.csv'
##       TLS Cartesian coordinates (in mm) collected from the Regional Office testing setup.
##   2b. 'RefLengths_May2026.csv'
##       Reference lengths (in mm) collected by tape from the Regional Office testing setup.
##
## NOTES:
## The testing environment at the Regional Office is different than at the Home Office. 
## Specifically, the Regional Office used a target array with 18 targets, while the Home Office target array 
## is 20 targets. Additionally, the printer was having issues, so the target numbering from the May data collection 
## is not sequential. To highlight that the two IPAs were performed in different locations, the target naming schemes 
## in the two sets of TLS data are different. 
## 
## PURPOSE OF THIS EXAMPLE:
## 1. Illustrate a significant Part II hypothesis test
##    (and demonstrating the MCS, depending on length error spec value)
## 2. Illustrate how target names in the two sets of TLS data can be different
## 3. Illustrate how the number of targets may be different in the two sets of TLS data
##


##
## HOW THESE DATA WERE COLLECTED
## 
'LSS999_Jan2026.csv' 		Created from 'Set_1_LeicaScanStation_LSLab_3_17_2026.txt'
                                (just added in a TargetID column)
'RefLengths_Jan2026.csv'        Created by picking six lengths from the Position 1 data


'LSS999_May2026.csv'		Created from 'Set_10_LeicaScanStation_Office_3_24_2026.txt'
				(added in a Target ID column and removed two targets)
'RefLengths_May2026.csv'  	Created by hand by picking six lengths from the Position 1 data
