# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 0_Data_Processing_Run_All.R
# Authors: Tristan Hanon
# Date: August 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Use renv::restore() to install packages recorded in lockfile:
renv::restore()

# Establish project root:
here::i_am("R CODE/Data-Processing/0_Data_Processing_Run_All.R")
library(here)


##### Run Data Processing Scripts ##############################################

# 1. Import all data:
source(
  here("R CODE", "Data-Processing", "1_Data_Import.R")
)

# 2. Process milk shipments data:
source(
  here("R CODE", "Data-Processing", "2_Process_Milk_Shipments.R")
)

# 3. Process milk component values and volumes:
source(
  here("R CODE", "Data-Processing", "3_Process_Component_Values_Volumes.R")
)

# 4. Process Commodity Flow Survey data:
source(
  here("R CODE", "Data-Processing", "4_Process_CFS_Data.R")
)

# 5. Estimate silage use regressions:
source(
  here("R CODE", "Data-Processing", "5_Silage_Regressions.R")
)

# 6. Process feed use data:
source(
  here("R CODE", "Data-Processing", "6_Process_Feed_Use_Data.R")
)

# 7. Process NASS crops data:
source(
  here("R CODE", "Data-Processing", "7_Process_NASS_Crops_Data.R")
)

# 8. Process cropland rent data:
source(
  here("R CODE", "Data-Processing", "8_CroplandRentQE.R")
)
