##### Setup #####

library(readxl)
library(mktnewsie)
library(tidyverse)


##### Load Data #####

# Load 2017 Milk Movement Data
milk_by_state_raw <- read_excel("data/raw/MilkAndMilkPooledByState2017.xlsx")

# Load 2019 Milk Movement Data
milk_by_state_2019 <- read_excel("data/raw/MilkAndMilkPooledByState2019.xlsx")

# Load State-Region Match Table
state_region_match <- read_csv("data/raw/State_Region_Match_Table.csv")


# Milk Utilization by Class 2017
  # They want everything in flat files, not API pulls




##### Process Data #####

# Process 2019 Milk Movement Data (Used to calculate California shipments):
ca_share_2019 <- milk_by_state_2019 %>%
  # Drop the first three rows:
  slice(4:n()) %>%
  # Calculate each state's share of total milk production pooled In California:
  mutate(CA_Share = `51` / `NASS Milk Marketings`) %>%
  # Drop unnecessary variables:
  select(
    State = `2019            State`,
    FIPS = `...4`,
    CA_Share
  )

# Process 2017 Milk Movement Data
milk_by_state_processed <- milk_by_state_raw %>%
  # Keep only useful variables and assign more appropriate names:
  select(
    State,
    FIPS = `...4`,
    Milk_Prod = `NASS Milk Marketings`,
    Milk_Pooled = `Milk Pooled Pounds`,
    Northeast = `1`,
    Appalachian = `5`,
    Florida = `6`,
    Southeast = `7`,
    Upper_Midwest = `30`,
    Central = `32`,
    Mideast = `33`,
    Pacific_Northwest = `124`,
    Southwest = `126`,
    Arizona = `131`
  ) %>%
  # Drop the first three rows:
  slice(4:n()) %>%
  # Join 2019 California Shares:
  left_join(
    ca_share_2019,
    by = c("State", "FIPS")
  ) %>%
  # Calculate new variables:
  mutate(
    # Quantity of milk pooled in California (See README for more information):
    California = case_when(
      State == "California" ~ Milk_Prod - Milk_Pooled,
      .default = Milk_Prod * CA_Share
    )
  ) %>%
  select(-CA_Share) %>%
  mutate(
    Milk_Pooled = rowSums(across(Northeast:California), na.rm = T)
  ) %>%
  mutate(
    # Quantity of milk not pooled in any FMMO region:
    Not_Pooled = case_when(
      State == "California" ~ 0, # Due to mandatory pooling provision.
      Milk_Prod - Milk_Pooled >= 0 ~ Milk_Prod - Milk_Pooled,
      .default = 0
    )
  ) %>%
  # Join State-Region Match Table:
  left_join(
    state_region_match,
    by = c("State", "FIPS")
  ) %>%
  # Drop rows without a Region (only Alaska and Hawaii):
  filter(!is.na(Region)) %>%
  # Group by Region and sum to the regional level:
  group_by(Region) %>%
  summarize(
    Milk_Prod = sum(Milk_Prod),
    Milk_Pooled = sum(Milk_Pooled),
    Northeast = sum(Northeast, na.rm = T),
    Appalachian = sum(Appalachian, na.rm = T),
    Florida = sum(Florida, na.rm = T),
    Southeast = sum(Southeast, na.rm = T),
    Upper_Midwest = sum(Upper_Midwest, na.rm = T),
    Central = sum(Central, na.rm = T),
    Mideast = sum(Mideast, na.rm = T),
    California = sum(California, na.rm = T),
    Pacific_Northwest = sum(Pacific_Northwest, na.rm = T),
    Southwest = sum(Southwest, na.rm = T),
    Arizona = sum(Arizona, na.rm = T),
    Not_Pooled = sum(Not_Pooled)
  )


# Accounting for Net Flows and Loops in 2017 Milk Movement Data
milk_by_state_processed %>%
  select(Origin_Region = Region, Northeast:Not_Pooled) %>%
  pivot_longer(
    Northeast:Arizona, 
    names_to = "Destination_Region", 
    values_to = "Milk_Flow"
  ) %>%
  mutate(
    Milk_Flow = case_when(
      Origin_Region == Destination_Region ~ Milk_Flow + Not_Pooled,
      .default = Milk_Flow
    ),
    Unregulated = case_when(
      Origin_Region == "Unregulated" ~ Not_Pooled,
      .default = 0
    )
  ) 



