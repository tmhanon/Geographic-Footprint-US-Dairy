# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 5_Process_NASS_Crops_Data.R
# Authors: Tristan Hanon
# Date: July 2026
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R/5_Process_NASS_Crops_Data.R")

# Load Packages
library(here)
library(tidyverse)

##### Clean Acreage Data #######################################################

# Feed Crops Acreage
feed_crops_processed <- feed_crop_acres_raw %>%
  separate(`Data Item`, into = c("Commodity", "Drop"), sep = " - ") %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  mutate(
    State = str_to_title(State),
    Comm_Group = Commodity %>%
      recode_values(from = crop_groups$Commodity, to = crop_groups$Comm_Group),
    Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  ) %>%
  select(State, Region, Comm_Group, Acres = Value)

# Other Crops Acreage
other_crops_processed <- other_crop_acres_raw %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  summarise(Other_Acres_Calculated = sum(Value), .by = State) %>%
  mutate(
    State = str_to_title(State),
    Comm_Group = "Other-Crops",
    Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  ) %>%
  select(State, Region, Comm_Group, Other_Acres_Calculated)

# Total Cropland
total_cropland_processed <- total_cropland_raw %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  mutate(
    State = str_to_title(State),
    Land_Type = `Data Item` %>%
      recode_values("AG LAND, CROPLAND - ACRES" ~ "All", default = "Pastured"),
    Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  ) %>%
  select(State, Region, Land_Type, Acres = Value) %>%
  pivot_wider(names_from = Land_Type, values_from = Acres) %>%
  mutate(Total_Cropland = All - Pastured) %>%
  select(State, Region, Total_Cropland)

# Harvested Cropland
harvested_cropland_processed <- harvested_cropland_raw %>%
  mutate(State = str_to_title(State)) %>%
  select(State, Harvested_Cropland = Value)


##### Clean Crop Sales Data ####################################################

# Feed Crops Sales
crop_sales_processed <- crop_sales_raw %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  mutate(
    State = str_to_title(State),
    Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      ),
    Comm_Group = Commodity %>%
      recode_values(from = crop_groups$Commodity, to = crop_groups$Comm_Group)
  ) %>%
  filter_out(Comm_Group == "Other-Crops") %>%
  summarise(Value = sum(Value), .by = c(Region, Comm_Group)) %>%
  arrange(Region, Comm_Group)


# Silage and Haylage Values

silage_value_processed <- silage_value_raw %>%
  separate(`Data Item`, into = c("Commodity", "Measure"), sep = " - ") %>%
  separate(Measure, into = c("Measure", "Drop"), sep = ",") %>%
  separate(Commodity, into = c("Commodity", "Drop"), sep = ",") %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  select(State, Commodity, Measure, Value) %>%
  pivot_wider(names_from = c(Commodity, Measure), values_from = Value) %>%
  mutate(
    State = str_to_title(State),
    Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      ),
    Comm_Group = "Silage",
    Silage_Price = `CORN_PRICE RECEIVED` * 8,
    Corn_Value = case_when(
      is.na(CORN_PRODUCTION) ~ 0,
      TRUE ~ CORN_PRODUCTION * Silage_Price
    ),
    Sorghum_Value = case_when(
      is.na(SORGHUM_PRODUCTION) ~ 0,
      TRUE ~ SORGHUM_PRODUCTION * 0.8 * Silage_Price
    ),
    Silage_Value = Corn_Value + Sorghum_Value
  ) %>%
  summarize(Value = sum(Silage_Value), .by = c(Region, Comm_Group))

haylage_value_processed <- haylage_value_raw %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  select(State, Commodity, Value) %>%
  pivot_wider(names_from = Commodity, values_from = Value) %>%
  mutate(
    State = str_to_title(State),
    Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      ),
    Comm_Group = "Silage",
    Value = 0.4 * HAYLAGE * HAY
  ) %>%
  summarize(Value = sum(Value), .by = c(Region, Comm_Group))

# Total Crop Sales
total_sales_processed <- total_sales_raw %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  mutate(
    State = str_to_title(State),
    Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  ) %>%
  summarize(Total_Sales = sum(Value), .by = Region)

# Calculate Sales Value of Other Crops:
other_sales_processed <- crop_sales_processed %>%
  # NOTE: Hay is excluded from the sum of crop sales subtracted from total sales
  #   since the total sales data come from the 2017 Census of Agriculture and
  #   the hay production value data are survey data.
  filter_out(Comm_Group == "Hay") %>%
  summarise(Sum_Sales = sum(Value), .by = Region) %>%
  left_join(total_sales_processed, by = "Region") %>%
  mutate(
    Other_Crops = Total_Sales - Sum_Sales,
    Comm_Group = "Other-Crops"
  ) %>%
  select(Region, Comm_Group, Value = Other_Crops)


##### Aggregate Data ###########################################################

# Crop Acreage:
crop_acreage_aggregate <- feed_crops_processed %>%
  summarise(Feed_Acres = sum(Acres), .by = c(State, Region)) %>%
  left_join(total_cropland_processed, by = c("State", "Region")) %>%
  left_join(harvested_cropland_processed, by = "State") %>%
  mutate(
    Other_Acres_Total = Total_Cropland - Feed_Acres,
    Other_Acres_Harvested = Harvested_Cropland - Feed_Acres
  ) %>%
  left_join(other_crops_processed, by = c("State", "Region")) %>%
  rowwise() %>%
  mutate(Acres = max(Other_Acres_Total, Other_Acres_Calculated)) %>%
  ungroup() %>%
  select(State, Region, Comm_Group, Acres) %>%
  bind_rows(feed_crops_processed) %>%
  mutate(
    Crop = Comm_Group %>%
      replace_values(
        c("Corn", "Other-Grains") ~ "Grains",
        c("Soybeans", "Other-Oilseeds") ~ "Oilseeds"
      )
  ) %>%
  summarise(Acres = sum(Acres), .by = c(Region, Crop)) %>%
  arrange(Region, Crop)

# Value of Crop Production:
crop_value_aggregate <- bind_rows(
  crop_sales_processed,
  silage_value_processed,
  haylage_value_processed,
  other_sales_processed
) %>%
  mutate(
    Crop = Comm_Group %>%
      replace_values(
        c("Corn", "Other-Grains") ~ "Grains",
        c("Soybeans", "Other-Oilseeds") ~ "Oilseeds"
      )
  ) %>%
  summarise(Value = sum(Value), .by = c(Region, Crop)) %>%
  arrange(Region, Crop)


##### Adjust Silage for Dairy Consumption ######################################

# Load Dairy Silage Shares
# NOTE: This intermediate file was created by the script 4_Silage_Regressions.R
#   If that script was recently run, the data may already be available, but
#   this step ensures it will be loaded.
dairy_silage_share <- read_csv(
  here("Data", "Processed", "Dairy_Silage_Share.csv")
)

# Adjust Silage Acreage for Dairy Consumption:
crop_acres_adjusted <- crop_acreage_aggregate %>%
  left_join(dairy_silage_share, by = "Region") %>%
  # Calculate share of silage acreage consumed by dairy cattle:
  mutate(
    Dairy_Silage_Acres = case_when(
      Crop == "Silage" ~ Acres * Dairy_Share,
      .default = NA_real_
    ),
    Other_Silage_Acres = case_when(
      Crop == "Silage" ~ Acres * CattleFeed_Share,
      .default = NA_real_
    )
  ) %>%
  fill(Dairy_Silage_Acres, Other_Silage_Acres, .direction = "up") %>%
  # Replace silage acres and add other silage acres to other crops:
  mutate(
    Acres = case_when(
      Crop == "Silage" ~ Dairy_Silage_Acres,
      Crop == "Other-Crops" ~ Acres + Other_Silage_Acres,
      .default = Acres
    )
  ) %>%
  select(Region, Crop, Acres) %>%
  write_csv(
    here("GAMS", "CSV DATA FILES", "Crop_Areas.csv"),
    col_names = FALSE
  )

# Adjust Silage Value for Dairy Consumption:
crop_value_adjusted <- crop_value_aggregate %>%
  left_join(dairy_silage_share, by = "Region") %>%
  # Calculate value of silage consumed by dairy cattle:
  mutate(
    Dairy_Silage_Value = case_when(
      Crop == "Silage" ~ Value * Dairy_Share,
      .default = NA_real_
    ),
    Other_Silage_Value = case_when(
      Crop == "Silage" ~ Value * CattleFeed_Share,
      .default = NA_real_
    )
  ) %>%
  fill(Dairy_Silage_Value, Other_Silage_Value, .direction = "up") %>%
  # Replace silage value and add other silage consumption to other crops:
  mutate(
    Value = case_when(
      Crop == "Silage" ~ Dairy_Silage_Value,
      Crop == "Other-Crops" ~ Value + Other_Silage_Value,
      .default = Value
    )
  ) %>%
  select(Region, Crop, Value) %>%
  write_csv(
    here("GAMS", "CSV DATA FILES", "Crop_Prod_Values.csv"),
    col_names = FALSE
  )
