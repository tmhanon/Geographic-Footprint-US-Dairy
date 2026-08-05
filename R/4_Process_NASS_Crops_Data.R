# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 4_Process_NASS_Crops_Data.R
# Authors: Tristan Hanon
# Date: July 2026
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R/4_Process_NASS_Crops_Data.R")

# Load Packages
library(tidyverse)
library(here)

##### Clean Data ###############################################################

# Feed Crops Acreage
feed_crops_processed <- feed_crop_acres_raw %>%
  separate(`Data Item`, into = c("Commodity", "Drop"), sep = " - ") %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  mutate(
    State = str_to_title(State),
    Comm_Group = Commodity %>%
      recode_values(from = crop_groups$Commodity, to = crop_groups$Comm_Group),
    Origin_Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  ) %>%
  select(State, Origin_Region, Comm_Group, Value)

# Other Crops Acreage
other_crops_processed <- other_crop_acres_raw %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  summarise(Value = sum(Value), .by = State) %>%
  mutate(
    State = str_to_title(State),
    Comm_Group = "Other-Crops",
    Origin_Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  ) %>%
  select(State, Origin_Region, Comm_Group, Value)

# Total Cropland
total_cropland_processed <- total_cropland_raw %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  mutate(
    State = str_to_title(State),
    Land_Type = `Data Item` %>%
      recode_values("AG LAND, CROPLAND - ACRES" ~ "All", default = "Pastured"),
    Origin_Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  ) %>%
  select(State, Origin_Region, Land_Type, Value) %>%
  pivot_wider(names_from = Land_Type, values_from = Value) %>%
  mutate(Value = All - Pastured) %>%
  select(State, Origin_Region, Value)

# Harvested Cropland
harvested_cropland_processed <- harvested_cropland_raw %>%
  mutate(State = str_to_title(State)) %>%
  select(State, Harvested_Cropland = Value)


##### Output Data ##############################################################

feed_crops_processed %>%
  summarise(Feed_Acres = sum(Value), .by = c(State, Origin_Region)) %>%
  left_join(
    rename(total_cropland_processed, Total_Cropland = Value),
    by = c("State", "Origin_Region")
  ) %>%
  left_join(harvested_cropland_processed, by = c("State")) %>%
  mutate(
    Other_Acres_Total = Total_Cropland - Feed_Acres,
    Other_Acres_Harvested = Harvested_Cropland - Feed_Acres
  ) %>%
  left_join(
    rename(other_crops_processed, Other_Acres_Calculated = Value),
    by = c("State", "Origin_Region")
  ) %>%
  rowwise() %>%
  mutate(Value = max(Other_Acres_Total, Other_Acres_Calculated)) %>%
  ungroup() %>%
  select(State, Origin_Region, Comm_Group, Value) %>%
  bind_rows(feed_crops_processed) %>%
  mutate(
    Big_Group = Comm_Group %>%
      replace_values(
        c("Corn", "Other-Grains") ~ "Grains",
        c("Soybeans", "Other-Oilseeds") ~ "Oilseeds"
      )
  ) %>%
  summarise(Value = sum(Value), .by = c(Origin_Region, Big_Group)) %>%
  arrange(Origin_Region, Big_Group) %>%
  write_csv(
    here("GAMS", "CSV DATA FILES", "Cropland_Areas_Raw.csv"),
    col_names = FALSE
  )
