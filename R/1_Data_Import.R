# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 1_Data_Import.R
# Authors: Tristan Hanon and Pierre Merel
# Date: July 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R/1_Data_Import.R")

# Load Packages
library(here)
library(readxl)
library(tidyverse)


##### Load Lookup Tables #######################################################

# Load Lookup Table for States and FMMO Regions:
state_region_match <- read_csv(
  here("Data", "Lookup_Tables", "State_Region_Match_Table.csv")
)

# Load Lookup Table for States and Census Regions and Divisions:
census_regions_divisions_states <- read_csv(
  here("Data", "Lookup_Tables", "Census_Regions_Divisions_States.csv")
) %>%
  mutate(State = str_to_title(State))

# Load Lookup Table for HS Codes and FMMO Product Classes:
hscode_class <- read_csv(
  here("Data", "Lookup_Tables", "HSCode_Class.csv")
)

# Load Lookup Table for Feed Crop Aggregate Groups:
crop_groups <- read_csv(
  here("Data", "Lookup_Tables", "Crop_Groups.csv")
)


##### Load Milk Movement Data ##################################################

# Load 2017 Milk Movement Data
milk_by_state_raw <- read_excel(
  here("Data", "Raw", "MilkAndMilkPooledByState2017.xlsx")
)

# Load 2019 Milk Movement Data
milk_by_state_2019 <- read_excel(
  here("Data", "Raw", "MilkAndMilkPooledByState2019.xlsx")
)


##### Load Commodity Flow Survey and Related Data ##############################

cfs_raw <- read_delim(here("Data", "Raw", "Special Tab 10.txt"), delim = "|")

faf_raw <- read_csv(here("Data", "Raw", "FAF_Shipments_All_2017.csv"))


##### Load Census Population and Trade Data ####################################

census_pop <- read_csv(here("Data", "Raw", "Census_Population_2017.csv"))

census_dairy_exports_raw <- read_csv(
  here("Data", "Raw", "Census_Dairy_Product_Exports_2017.csv")
)

census_dairy_imports_raw <- read_csv(
  here("Data", "Raw", "Census_Dairy_Product_Imports_2017.csv")
)


##### Load NASS Data ###########################################################

milk_prod_raw <- read_csv(here("Data", "Raw", "NASS_Milk_Production_2017.csv"))

feed_crop_acres_raw <- read_csv(
  here("Data", "Raw", "NASS_Census_Feed_Crop_Acreage_2017.csv")
)

total_cropland_raw <- read_csv(
  here("Data", "Raw", "NASS_All_Cropland_2017.csv")
)

harvested_cropland_raw <- read_csv(
  here("Data", "Raw", "NASS_All_Cropland_Harvested_2017.csv")
)

other_crop_acres_raw <- read_csv(
  here("Data", "Raw", "NASS_Census_Other_Crop_Acreage_2017.csv")
)


# Milk Utilization by Class 2017
# They want everything in flat files, not API pulls

##### Useful Vectors/Lists #####################################################

# Vector of FMMO Region Names
fmmo_regions <- c(
  "Northeast",
  "Appalachian",
  "Florida",
  "Southeast",
  "Upper-Midwest",
  "Central",
  "Mideast",
  "California",
  "Pacific-Northwest",
  "Southwest",
  "Arizona",
  "Unregulated"
)

# Vector of Continental States (i.e., excluding Alaska and Hawaii)
continental_states <- c(
  "Alabama",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "Florida",
  "Georgia",
  "Idaho",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "Wisconsin",
  "West Virginia",
  "Wyoming",
  "District of Columbia"
)
