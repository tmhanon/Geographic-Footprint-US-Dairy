# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 1_Data_Import.R
# Authors: Tristan Hanon and Pierre Merel
# Date: July 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R CODE/Data-Processing/1_Data_Import.R")

# Load Packages
library(here)
library(tidyverse)


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

# Load Lookup Table for HS Codes and Feed Crops:
hscode_crops <- read_csv(
  here("Data", "Lookup_Tables", "HSCode_Crops.csv")
)

# Load Lookup Table for Feed Crop Aggregate Groups:
crop_groups <- read_csv(
  here("Data", "Lookup_Tables", "Crop_Groups.csv")
)

# Load Lookup Table for NASS Dairy Product Production Regions:
nass_regions <- read_csv(
  here("Data", "Lookup_Tables", "NASS_Regions_Match_Table.csv")
)

# Load Lookup Table for Dairy Product Groups:
dairy_product_groups <- read_csv(
  here("Data", "Lookup_Tables", "NASS_Dairy_Product_Match_Table.csv")
)

# Load Lookup Table for Livestock Groups:
livestock_groups <- read_csv(
  here("Data", "Lookup_Tables", "NASS_Livestock_Match_Table.csv")
)

# Load Lookup Table for Class I Price Differentials
class1_diffs <- read_csv(
  here("Data", "Lookup_Tables", "Class1_Differentials.csv")
)


##### Load Milk Movement Data ##################################################

# Load 2017 Milk Movement Data
milk_by_state_raw <- read_csv(
  here("Data", "Raw", "MilkAndMilkPooledByState2017.csv")
)

# Load 2019 Milk Movement Data
milk_by_state_2019 <- read_csv(
  here("Data", "Raw", "MilkAndMilkPooledByState2019.csv")
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

crop_exports_raw <- read_csv(
  here("Data", "Raw", "GATS_Crop_Exports_1519.csv")
)


##### Load ERS Data ############################################################

feed_grains_yearbook_raw <- read_csv(
  here("Data", "Raw", "feed-grains-yearbook-all-years.csv")
)

oil_crops_yearbook_raw <- read_csv(
  here("Data", "Raw", "OilCropsAllTables.csv")
)

bioenergy_stats_raw <- read_csv(
  here("Data", "Raw", "us-bioenergy-statistics.csv")
)

dairy_per_capita <- read_csv(
  here("Data", "Raw", "selected-dairy-products-percapita.csv")
)

comp_conversions <- read_csv(
  here("Data", "Raw", "ERS_Component_Conversions.csv")
)


##### Load NASS Data ###########################################################

milk_prod_raw <- read_csv(
  here("Data", "Raw", "NASS_Milk_Production_2017.csv")
)

dairy_products_raw <- read_csv(
  here("Data", "Raw", "NASS_Dairy_Products_2017.csv")
)

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

crop_sales_raw <- read_csv(
  here("Data", "Raw", "NASS_Crop_Sales_2017.csv")
)

total_sales_raw <- read_csv(
  here("Data", "Raw", "NASS_Total_Crop_Sales_2017.csv")
)

silage_value_raw <- read_csv(
  here("Data", "Raw", "NASS_Silage_Data_2017.csv")
)

haylage_value_raw <- read_csv(
  here("Data", "Raw", "NASS_Haylage_Data_2017.csv")
)

silage_data_1519_raw <- read_csv(
  here("Data", "Raw", "NASS_Silage_Data_1519.csv")
)

haylage_data_1519 <- read_csv(
  here("Data", "Raw", "NASS_Haylage_Data_1519.csv")
)

nass_livestock_data_raw <- read_csv(
  here("Data", "Raw", "NASS_Livestock_Data_1519.csv")
)

LandRent <- read.csv(
  here("Data", "Raw", "CroplandRentState.csv")
)

LandAcres <- read.csv(
  here("Data", "Raw", "CroplandAcresState.csv")
)


##### Load AMS Data ############################################################

# Total Milk Receipts by Region
fmmo_receipts_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_Producer_Receipts_2017.csv")
)


# Overall Component Test Values
total_fat_raw <- read_csv(
  here(
    "Data",
    "Raw",
    "Datamart-Export_FM_Producer_Components-Butterfat_2017.csv"
  )
)

total_prot_raw <- read_csv(
  here(
    "Data",
    "Raw",
    "Datamart-Export_FM_Producer_Components-Protein_2017.csv"
  )
)

total_os_raw <- read_csv(
  here(
    "Data",
    "Raw",
    "Datamart-Export_FM_Producer_Components-Other Solids_2017.csv"
  )
)


# Class I Milk Volumes
class1_milk_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassI_Util-Milk_2017.csv")
)


# Utilization Shares by Class
class1_util_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassI_Util-Utilization_2017.csv")
)

class2_util_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassII_Util-Utilization_2017.csv")
)

class3_util_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassIII_Util-Utilization_2017.csv")
)

class4_util_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassIV_Util-Utilization_2017.csv")
)


# Butterfat Shares by Class
class1_fat_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassI_Util-Butterfat_2017.csv")
)

class2_fat_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassII_Util-Butterfat_2017.csv")
)

class3_fat_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassIII_Util-Butterfat_2017.csv")
)

class4_fat_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassIV_Util-Butterfat_2017.csv")
)


# Nonfat Solids Shares for Classes I, II, and IV
class1_nfs_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassI_Util-NFS_2017.csv")
)

class2_nfs_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassII_Util-NFS_2017.csv")
)

class4_nfs_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassIV_Util-NFS_2017.csv")
)


# Protein and Other Solids Shares for Class III
class3_protein_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassIII_Util-Protein_2017.csv")
)

class3_os_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_FM_ClassIII_Util-Other Solids_2017.csv")
)


# Advanced and Class Prices
adv_prices_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_DY_Advanced_Prices_2017.csv")
)

class_prices_raw <- read_csv(
  here("Data", "Raw", "Datamart-Export_DY_Class_Prices_2017.csv")
)


##### Data for State Milk Marketing Orders #####################################

# Load 2017 California Data
ca_data_raw <- read_csv(
  here("Data", "Raw", "California_Statistics_2017.csv")
)

ca_class1_raw <- read_csv(
  here("Data", "Raw", "California_Monthly_ClassI_2017.csv")
)

# Load 2017 Western New York Data
wny_data_raw <- read_csv(
  here("Data", "Raw", "Western_New_York_Data_2017.csv")
)
