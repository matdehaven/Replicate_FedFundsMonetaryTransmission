##
##  Main Script
##  Executes all other scripts in order to be run
##
##  Matthew DeHaven
##  2022 11 14
##

## Step 1:  Gather data
source("./code/pull_FRED_data.R")

source("./code/Recrate_BernankeBlinderVAR.R")

source("./code/Update_BernankeBlinderVAR.R")

source("./code/Update_BernankeBlinderVAR_shadow.R")