# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 2_Process_Milk_Shipments.R
# Authors: Tristan Hanon and Pierre Merel
# Date: July 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R/2_Process_Milk_Shipments.R")


# Load Packages
library(here)
library(tidyverse)


# Define Functions
collapse_cycle <- function(data, cycle) {
  n <- length(cycle)

  # identify the flows in the cycle
  flows <- sapply(
    seq_along(cycle),
    function(i) {
      from <- cycle[i]
      to <- cycle[ifelse(i == n, 1, i + 1)]
      data[from, to]
    }
  )

  # smallest trade flow
  delta <- min(flows)

  # remove delta from all trade links in the cycle
  for (i in seq_along(cycle)) {
    from <- cycle[i]
    to <- cycle[ifelse(i == n, 1, i + 1)]
    data[from, to] <- data[from, to] - delta
  }

  # put delta into self consumption of every node
  for (i in seq_along(cycle)) {
    data[cycle[i], cycle[i]] <- data[cycle[i], cycle[i]] + delta
  }

  return(data)
}


##### Process Data #############################################################

# Clean 2019 Milk Movement Data (Used to calculate California shipments):
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

# Clean 2017 Milk Movement Data
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
    `Upper-Midwest` = `30`,
    Central = `32`,
    Mideast = `33`,
    `Pacific-Northwest` = `124`,
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
    Milk_Pooled = rowSums(across(Northeast:California), na.rm = T),
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
    `Upper-Midwest` = sum(`Upper-Midwest`, na.rm = T),
    Central = sum(Central, na.rm = T),
    Mideast = sum(Mideast, na.rm = T),
    California = sum(California, na.rm = T),
    `Pacific-Northwest` = sum(`Pacific-Northwest`, na.rm = T),
    Southwest = sum(Southwest, na.rm = T),
    Arizona = sum(Arizona, na.rm = T),
    Not_Pooled = sum(Not_Pooled)
  ) %>%
  mutate(Region = factor(Region, levels = fmmo_regions)) %>%
  arrange(Region)


# Add milk not pooled to self consumption and save as data frame with rownames:
milk_by_state_matrix <- milk_by_state_processed %>%
  select(Origin_Region = Region, Northeast:Not_Pooled) %>%
  pivot_longer(
    Northeast:Arizona,
    names_to = "Destination_Region",
    values_to = "Milk_Flow"
  ) %>%
  mutate(
    # Add milk not pooled to self consumption:
    Milk_Flow = case_when(
      Origin_Region == Destination_Region ~ Milk_Flow + Not_Pooled,
      .default = Milk_Flow
    ),
    # Define Unregulated self consumption as milk not pooled from the
    # Unregulated region, zero otherwise:
    Unregulated = case_when(
      Origin_Region == "Unregulated" ~ Not_Pooled,
      .default = 0
    )
  ) %>%
  pivot_wider(names_from = Destination_Region, values_from = Milk_Flow) %>%
  relocate(Unregulated, .after = last_col()) %>%
  select(-Not_Pooled) %>%
  column_to_rownames(var = "Origin_Region")

# Create list of region pairs with bilateral milk flows:
net_flow_pairs <- list(
  c("Appalachian", "Northeast"),
  c("Mideast", "Northeast"),
  c("Southeast", "Appalachian"),
  c("Appalachian", "Florida"),
  c("Florida", "Southeast"),
  c("Appalachian", "Upper-Midwest"),
  c("Southeast", "Upper-Midwest"),
  c("Appalachian", "Central"),
  c("Southeast", "Central"),
  c("Upper-Midwest", "Central"),
  c("Mideast", "Central"),
  c("Appalachian", "Mideast"),
  c("Upper-Midwest", "Mideast"),
  c("Central", "Southwest"),
  c("Southwest", "Arizona")
)

# Create list of regions with loops:
milk_flow_loops <- list(
  c("Upper-Midwest", "Northeast", "Mideast"),
  c("Florida", "Appalachian", "Southeast"),
  c("Central", "California", "Arizona", "Southwest"),
  c("Southwest", "California", "Arizona")
)

# Apply collapse_cycle() function to net flows and loops:
milk_shipments <- reduce(
  c(net_flow_pairs, milk_flow_loops),
  collapse_cycle,
  .init = milk_by_state_matrix
)


##### Save Output ##############################################################

# Save milk shipments matrix as CSV file in GAMS folder:
milk_shipments %>%
  write.csv(here("GAMS", "CSV DATA FILES", "Milk_Shipments_Jul2026.csv"))

# Calculate Total Milk Utilization by Region:
milk_shipments %>%
  as_tibble(rownames = "Region") %>%
  summarize(across(Northeast:Unregulated, sum)) %>%
  pivot_longer(everything(), names_to = "Region", values_to = "Utilization") %>%
  write_csv(here("data", "processed", "Milk_Utilization.csv"))

# Calculated Total Milk Production by Region:
milk_shipments %>%
  as_tibble(rownames = "Region") %>%
  rowwise() %>%
  mutate(Production = sum(c_across(Northeast:Unregulated))) %>%
  select(Region, Production) %>%
  write_csv(here("data", "processed", "Milk_Production.csv"))
