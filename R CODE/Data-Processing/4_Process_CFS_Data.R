# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 4_Process_CFS_Data.R
# Authors: Tristan Hanon
# Date: July 2026
#
# Note: The data used in this script and the methods applied to process these
#   these data are primarily discussed in Chapter 3 of Hanon (2023).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R CODE/Data-Processing/4_Process_CFS_Data.R")

# Load Packages
library(tidyverse)
library(here)


##### Clean Data ###############################################################

# CFS Commodity Groups
cfs_commodities <- cfs_raw %>%
  distinct(COMM, COMM_LABEL) %>%
  mutate(
    Comm_Group = case_when(
      COMM == "02200" ~ "Corn",
      COMM %in% c("03400", "04140") ~ "Soybeans",
      COMM %in%
        c(
          "02100",
          "02901",
          "02902",
          "02903",
          "02904",
          "02909"
        ) ~ "Other-Grains",
      COMM %in% c("03502", "03503", "03505") ~ "Other-Oilseeds",
      COMM %in% c("04110", "04130") ~ "Hay",
      COMM %in%
        c(
          "03100",
          "03211",
          "03212",
          "03213",
          "03214",
          "03219",
          "03221",
          "03229",
          "03311",
          "03312",
          "03319",
          "03321",
          "03322",
          "03331",
          "03323",
          "03324",
          "03329",
          "03339",
          "03341",
          "03342",
          "03501",
          "03504",
          "03506",
          "03509",
          "03602",
          "03921",
          "03922",
          "03930",
          "03992"
        ) ~ "Other-Crops",
      COMM %in% c("07111", "07899") ~ "Beverage",
      COMM %in% c("07130", "07199", "06399") ~ "Softs",
      COMM == "07120" ~ "Cheese",
      COMM %in% c("07112", "07119", "07191") ~ "Butter-Powder",
      TRUE ~ "DROP"
    ),
    Model_Level = case_when(
      Comm_Group %in%
        c(
          "Corn",
          "Soybeans",
          "Hay",
          "Other-Grains",
          "Other-Oilseeds",
          "Other-Crops"
        ) ~ "Crops",
      Comm_Group %in%
        c("Beverage", "Softs", "Cheese", "Butter-Powder") ~ "Dairy"
    )
  )

# CFS Transportation Modes
cfs_modes <- cfs_raw %>%
  distinct(DMODE, DMODE_LABEL)

# CFS Origin Regions
cfs_origins <- cfs_raw %>%
  distinct(GEO_ID, NAME) %>%
  mutate(
    Origin_Region = NAME %>%
      replace_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  )

# CFS Destination Regions
cfs_destinations <- cfs_raw %>%
  distinct(DDESTGEO, DDEDSTGEO_LABEL) %>%
  mutate(
    Dest_Region = DDEDSTGEO_LABEL %>%
      replace_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  )

# Generate clean CFS data:
cfs_data <- cfs_raw %>%
  # First two digits of GEO_ID and DDESTGEO indicate the level of aggregation:
  separate(
    GEO_ID,
    into = c("Origin_Level", "Origin_Code"),
    sep = 2,
    remove = F
  ) %>%
  separate(
    DDESTGEO,
    into = c("Dest_Level", "Dest_Code"),
    sep = 2,
    remove = F
  ) %>%
  filter(
    # Keep only "ALL MODES" of transportation...
    DMODE == "001",
    # ... and National, Region, Division, State aggregation levels:
    Origin_Level %in% c("01", "02", "03", "04"),
    Dest_Level %in% c("01", "02", "03", "04")
  ) %>%
  left_join(cfs_commodities, by = c("COMM", "COMM_LABEL")) %>%
  left_join(cfs_origins, by = c("GEO_ID", "NAME")) %>%
  left_join(cfs_destinations, by = c("DDESTGEO", "DDEDSTGEO_LABEL")) %>%
  select(
    GEO_ID,
    Origin_Level,
    NAME,
    Origin_Region,
    DDESTGEO,
    Dest_Level,
    DDEDSTGEO_LABEL,
    Dest_Region,
    COMM,
    COMM_LABEL,
    Comm_Group,
    Model_Level,
    VAL,
    VAL_F,
    TON,
    TON_F
  ) %>%
  # Group DC with Maryland:
  mutate(
    NAME = replace_values(NAME, "District of Columbia" ~ "Maryland"),
    DDEDSTGEO_LABEL = replace_values(
      DDEDSTGEO_LABEL,
      "District of Columbia" ~ "Maryland"
    )
  ) %>%
  # Drop commodity groups that are not of interest:
  filter_out(Comm_Group == "DROP")


# FAF Distance Data:
faf_distance <- faf_raw %>%
  separate(dms_orig, into = c("Orig_Code", "Origin_State"), sep = "-") %>%
  separate(dms_dest, into = c("Dest_Code", "Dest_State"), sep = "-") %>%
  select(
    Origin_State,
    Dest_State,
    dms_mode,
    Tons = `thousand tons in 2017`,
    Ton_Miles = `million ton-miles in 2017`
  ) %>%
  mutate(
    Origin_State = replace_values(Origin_State, "Washington DC" ~ "Maryland"),
    Dest_State = replace_values(Dest_State, "Washington DC" ~ "Maryland"),
    Miles = (Ton_Miles * 1000) / Tons
  ) %>%
  summarize(
    Distance = weighted.mean(Miles, Tons),
    .by = c(Origin_State, Dest_State)
  )

# NASS Milk Production:
milk_prod <- milk_prod_raw %>%
  mutate(
    State = str_to_title(State),
    FMMO = replace_values(
      State,
      from = state_region_match$State,
      to = state_region_match$Region
    )
  ) %>%
  group_by(FMMO) %>%
  mutate(Region_Milk_Share = Value / sum(Value)) %>%
  select(State, FMMO, Milk_Prod = Value, Region_Milk_Share)

# Census Dairy Import and Export Data:

census_dairy_exports <- census_dairy_exports_raw %>%
  select(State, Commodity, Value = `Total Value ($US)`) %>%
  filter(!(State == "Total")) %>%
  separate(Commodity, c("HS_Code", "Product"), sep = " ", extra = "merge") %>%
  mutate(HS_Code = as.numeric(HS_Code), Dest_Region = "ROW")

census_dairy_imports <- census_dairy_imports_raw %>%
  select(State, Commodity, Value = `Total Value ($US)`) %>%
  filter(!(State == "Total")) %>%
  separate(Commodity, c("HS_Code", "Product"), sep = " ", extra = "merge") %>%
  mutate(HS_Code = as.numeric(HS_Code), Origin_Region = "ROW")

# Census Dairy Commodities
census_dairy_commodities <- bind_rows(
  census_dairy_imports,
  census_dairy_exports
) %>%
  distinct(HS_Code, Product) %>%
  mutate(
    Comm_Group = recode_values(
      HS_Code,
      from = hscode_class$HS_Code,
      to = hscode_class$Comm_Group
    )
  )

# Census Regions
census_destinations <- census_dairy_exports %>%
  distinct(State) %>%
  mutate(
    Dest_Region = recode_values(
      State,
      from = state_region_match$State,
      to = state_region_match$Region
    )
  )

# Census Origins
census_origins <- census_dairy_imports %>%
  distinct(State) %>%
  mutate(
    Origin_Region = recode_values(
      State,
      from = state_region_match$State,
      to = state_region_match$Region
    )
  )

# Finish Processing Census Imports and Exports
census_dairy_imports_processed <- census_dairy_imports %>%
  left_join(
    select(state_region_match, State, Dest_Region = Region),
    by = "State"
  ) %>%
  left_join(census_dairy_commodities, by = c("HS_Code", "Product")) %>%
  filter_out(State == "Unknown") %>%
  summarise(
    Value = sum(Value),
    .by = c(Origin_Region, Dest_Region, Comm_Group)
  ) %>%
  arrange(Dest_Region, Comm_Group)

census_dairy_exports_processed <- census_dairy_exports %>%
  left_join(
    select(state_region_match, State, Origin_Region = Region),
    by = "State"
  ) %>%
  left_join(census_dairy_commodities, by = c("HS_Code", "Product")) %>%
  filter_out(when_any(State == "Unknown", Comm_Group == "Beverage")) %>%
  summarise(
    Value = sum(Value),
    .by = c(Origin_Region, Dest_Region, Comm_Group)
  ) %>%
  arrange(Origin_Region, Comm_Group)

census_dairy_imports_exports <- bind_rows(
  census_dairy_imports_processed,
  census_dairy_exports_processed
)


##### Define Functions for RAS Corrections #####################################

# See Lahr and de Mesnard (2004) for more information.

ras_fix <- function(tab, row, col, tolerance = .Machine$double.eps) {
  if (!isTRUE(all.equal(sum(row), sum(col)))) {
    stop("sum(u) must be equal to sum(v)")
  }

  if (any(tab == 0)) {
    warning("convergence is not guaranteed when some cells are equal to 0")
  }

  if (any(tab < 0)) {
    stop("elements of tab must all be >= 0")
  }

  if (any(row <= 0) || any(col <= 0)) {
    stop("elements of row and col must all be > 0")
  }

  # Destroyed by operations
  attr <- attributes(tab)

  while (
    !(isTRUE(all.equal(rowSums(tab), row, tolerance, check.attributes = F)) &&
      isTRUE(all.equal(colSums(tab), col, tolerance, check.attributes = F)))
  ) {
    r <- row / rowSums(tab)
    tab <- diag(r) %*% tab
    c <- col / colSums(tab)
    tab <- tab %*% diag(c)
  }

  attributes(tab) <- attr
  tab
}

ras_correction <- function(flow_data) {
  # Conserve Identifying Columns
  ident_cols <- flow_data %>%
    select(
      Origin_State,
      Origin_Region,
      Dest_State,
      Dest_Region,
      COMM,
      Comm_Group
    )

  # Extract Row Sums and Column Sums
  row <- flow_data %>%
    group_by(Origin_State) %>%
    summarise(Supply = mean(Supply)) %>%
    select(Supply) %>%
    as_vector()

  col <- flow_data %>%
    group_by(Dest_State) %>%
    summarise(Demand = mean(Demand)) %>%
    select(Demand) %>%
    as_vector()

  # Convert Data Frame to Flow Table
  tab <- flow_data %>%
    ungroup() %>%
    select(Origin_State, Dest_State, Flow) %>%
    xtabs(Flow ~ Origin_State + Dest_State, data = .)

  # Use RAS Function and Convert Output to Tibble
  flow_tib <- ras_fix(tab, row, col, tolerance = 0.000001) %>%
    as_tibble() %>%
    rename(Flow = n)

  # Replace Identifying Columns and Return
  left_join(ident_cols, flow_tib, by = c("Origin_State", "Dest_State")) %>%
    return()
}


##### Define Function to Calculate Flows Using Gravity Approach ################

# See Gabela (2020) for more information.

flow_calculation <- function(supply_demand, distances, params = c(1, 1, 1)) {
  # Calculate Tradable Factors and SHIN Own
  tradable <- supply_demand %>%
    mutate(GAP = abs((Demand - Supply) / ((Demand + Supply) / 2))) %>%
    mutate(
      AGAP = mean(GAP),
      Tradable = (1 + 0.5 * exp(5 * (AGAP - 1))) / (1 + exp(5 * (AGAP - 1)))
    ) %>%
    rowwise() %>%
    mutate(
      SHIN_Own = min(Supply / Demand, 1) * Tradable,
      Flow_Own = SHIN_Own * Demand
    )

  # Calculate Uncorrected Flow
  flow_uncorrected <- distances %>%
    full_join(
      select(supply_demand, Origin_State = State, Origin_Region = FMMO, Supply),
      by = c("Origin_State")
    ) %>%
    full_join(
      select(supply_demand, Dest_State = State, Dest_Region = FMMO, Demand),
      by = c("Dest_State")
    ) %>%
    full_join(
      select(
        tradable,
        Dest_State = State,
        Dest_Region = FMMO,
        Tradable,
        SHIN_Own
      ),
      by = c("Dest_State", "Dest_Region")
    ) %>%
    mutate(
      Prod_Supply = sum(Supply),
      Bracket1 = (Supply / Prod_Supply) / (Distance)^params[1]
    ) %>%
    group_by(Dest_State) %>%
    mutate(
      Bracket2 = (1 - SHIN_Own) / sum(Bracket1[Origin_State != Dest_State]),
      SHIN_Off = (Bracket1) * (Bracket2),
      SHIN = case_when(Origin_State == Dest_State ~ SHIN_Own, TRUE ~ SHIN_Off),
      Flow = SHIN * Demand,
      Dem_Check = sum(Flow)
    ) %>%
    group_by(Origin_State) %>%
    mutate(Supply_Check = sum(Flow))

  # Correct Flow Using RAS Correction
  flow_corrected <- flow_uncorrected %>%
    ras_correction()

  return(flow_corrected)
}


##### Calculate Dairy Regional Supply and Demand #####

# US Total Supplies and Demands
cfs_us_to_us <- cfs_data %>%
  filter(Model_Level == "Dairy", Origin_Level == "01", Dest_Level == "01") %>%
  summarize(US_to_US = sum(VAL), .by = c(COMM, Comm_Group))


# Census Region Supplies and Demands

cfs_reg_to_us <- cfs_data %>%
  filter(Model_Level == "Dairy", Origin_Level == "02", Dest_Level == "01") %>%
  mutate(
    Val_Fixed = case_when(VAL_F == "S" ~ NA_real_, TRUE ~ VAL),
    RtoUS_Miss = case_when(is.na(Val_Fixed) ~ 1, TRUE ~ 0)
  ) %>%
  left_join(cfs_us_to_us, by = c("COMM", "Comm_Group")) %>%
  group_by(COMM, Comm_Group) %>%
  mutate(
    Sum_Supply = sum(Val_Fixed, na.rm = T),
    Missing_Supply = US_to_US - Sum_Supply,
    Reg_to_US = case_when(RtoUS_Miss == 1 ~ Missing_Supply, TRUE ~ Val_Fixed),
    Sum_Check = sum(Reg_to_US)
  ) %>%
  select(Region = NAME, COMM, Comm_Group, Reg_to_US)

cfs_us_to_reg <- cfs_data %>%
  filter(Model_Level == "Dairy", Origin_Level == "01", Dest_Level == "02") %>%
  mutate(
    Val_Fixed = case_when(VAL_F == "S" ~ NA_real_, TRUE ~ VAL),
    UStoR_Miss = case_when(is.na(Val_Fixed) ~ 1, TRUE ~ 0)
  ) %>%
  left_join(cfs_us_to_us, by = c("COMM", "Comm_Group")) %>%
  group_by(COMM, Comm_Group) %>%
  mutate(
    Sum_Demand = sum(Val_Fixed, na.rm = T),
    Missing_Demand = US_to_US - Sum_Demand,
    US_to_Reg = case_when(UStoR_Miss == 1 ~ Missing_Demand, TRUE ~ Val_Fixed),
    Sum_Check = sum(US_to_Reg)
  ) %>%
  select(Region = DDEDSTGEO_LABEL, COMM, Comm_Group, US_to_Reg)


# Census Division Supplies and Demands

cfs_div_to_us <- cfs_data %>%
  filter(Model_Level == "Dairy", Origin_Level == "03", Dest_Level == "01") %>%
  mutate(
    Val_Fixed = case_when(VAL_F == "S" ~ NA_real_, TRUE ~ VAL),
    DivtoUS_Miss = case_when(is.na(Val_Fixed) ~ 1, TRUE ~ 0)
  ) %>%
  left_join(
    select(census_regions_divisions_states, NAME = Division, Region) %>%
      distinct(NAME, Region),
    by = "NAME"
  ) %>%
  left_join(
    select(cfs_reg_to_us, Region, COMM, Comm_Group, Reg_to_US),
    by = c("Region", "COMM", "Comm_Group")
  ) %>%
  group_by(Region, COMM, Comm_Group) %>%
  mutate(
    Sum_Supply = sum(Val_Fixed, na.rm = T),
    Missing_Supply = Reg_to_US - Sum_Supply,
    Div_to_US = case_when(DivtoUS_Miss == 1 ~ Missing_Supply, TRUE ~ Val_Fixed),
    Sum_Check = sum(Div_to_US)
  ) %>%
  select(Division = NAME, Region, COMM, Comm_Group, Div_to_US)

cfs_us_to_div <- cfs_data %>%
  filter(Model_Level == "Dairy", Origin_Level == "01", Dest_Level == "03") %>%
  mutate(
    Val_Fixed = case_when(VAL_F == "S" ~ NA_real_, TRUE ~ VAL),
    UStoDiv_Miss = case_when(is.na(Val_Fixed) ~ 1, TRUE ~ 0)
  ) %>%
  left_join(
    select(
      census_regions_divisions_states,
      DDEDSTGEO_LABEL = Division,
      Region
    ) %>%
      distinct(DDEDSTGEO_LABEL, Region),
    by = "DDEDSTGEO_LABEL"
  ) %>%
  left_join(
    select(cfs_us_to_reg, Region, COMM, Comm_Group, US_to_Reg),
    by = c("Region", "COMM", "Comm_Group")
  ) %>%
  group_by(Region, COMM, Comm_Group) %>%
  mutate(
    Sum_Demand = sum(Val_Fixed, na.rm = T),
    Missing_Demand = US_to_Reg - Sum_Demand,
    US_to_Div = case_when(UStoDiv_Miss == 1 ~ Missing_Demand, TRUE ~ Val_Fixed),
    Sum_Check = sum(US_to_Div) - US_to_Reg
  ) %>%
  select(Division = DDEDSTGEO_LABEL, Region, COMM, Comm_Group, US_to_Div)


# Calculate State Supplies and Demands from CFS

cfs_state_to_us <- cfs_data %>%
  filter(Model_Level == "Dairy", Origin_Level == "04", Dest_Level == "01") %>%
  mutate(Val_Fixed = case_when(VAL_F == "S" ~ NA_real_, TRUE ~ VAL)) %>%
  select(
    State = NAME,
    FMMO = Origin_Region,
    COMM,
    Comm_Group,
    State_to_US = Val_Fixed
  ) %>%
  summarize(
    State_to_US = sum(State_to_US),
    .by = c(State, FMMO, COMM, Comm_Group)
  ) %>%
  left_join(census_regions_divisions_states, by = "State") %>%
  select(State, Division, Region, FMMO, everything())

cfs_us_to_state <- cfs_data %>%
  filter(Model_Level == "Dairy", Origin_Level == "01", Dest_Level == "04") %>%
  mutate(Val_Fixed = case_when(VAL_F == "S" ~ NA_real_, TRUE ~ VAL)) %>%
  select(
    State = DDEDSTGEO_LABEL,
    FMMO = Dest_Region,
    COMM,
    Comm_Group,
    US_to_State = Val_Fixed
  ) %>%
  summarize(
    US_to_State = sum(US_to_State),
    .by = c(State, FMMO, COMM, Comm_Group)
  ) %>%
  left_join(census_regions_divisions_states, by = "State") %>%
  select(State, Division, Region, FMMO, everything())

# Isolate Known Values of Intra-Regional Trade Flows

cfs_dairy_intraregional <- cfs_data %>%
  filter(
    Model_Level == "Dairy",
    Origin_Level == "04",
    Dest_Level == "04",
    NAME == DDEDSTGEO_LABEL
  ) %>%
  mutate(Val_Fixed = case_when(VAL_F == "S" ~ NA_real_, TRUE ~ VAL)) %>%
  select(
    State = NAME,
    FMMO = Origin_Region,
    COMM,
    Comm_Group,
    Intra_Known = Val_Fixed
  ) %>%
  summarize(
    Intra_Known = sum(Intra_Known),
    .by = c(State, FMMO, COMM, Comm_Group)
  ) %>%
  left_join(census_regions_divisions_states, by = "State") %>%
  select(State, Division, Region, FMMO, everything())


# Full Supply and Demand Data Frame

cfs_dairy_supply_demand <- full_join(
  cfs_state_to_us,
  cfs_us_to_state,
  by = c("State", "Division", "Region", "FMMO", "COMM", "Comm_Group")
) %>%
  # Add explicit missing values for Wyoming (causes issues if not present):
  bind_rows(
    expand_grid(
      State = "Wyoming",
      Division = "Mountain Division",
      Region = "West Region",
      FMMO = "Unregulated",
      COMM = c("07112", "07119", "07191"),
      Comm_Group = "Butter-Powder",
      State_to_US = NA_real_,
      US_to_State = NA_real_
    )
  ) %>%
  left_join(
    cfs_div_to_us,
    by = c("Division", "Region", "COMM", "Comm_Group")
  ) %>%
  left_join(
    cfs_us_to_div,
    by = c("Division", "Region", "COMM", "Comm_Group")
  ) %>%
  left_join(
    cfs_dairy_intraregional,
    by = c("State", "Division", "Region", "FMMO", "COMM", "Comm_Group")
  ) %>%
  group_by(Division, COMM, Comm_Group) %>%
  mutate(
    Sum_Supply = sum(State_to_US, na.rm = T),
    Sum_Demand = sum(US_to_State, na.rm = T)
  ) %>%
  full_join(
    select(milk_prod, State, FMMO, Milk_Prod),
    by = c("State", "FMMO")
  ) %>%
  left_join(
    select(census_pop, State, FMMO, Population),
    by = c("State", "FMMO")
  ) %>%
  mutate(
    StoUS_Miss = case_when(is.na(State_to_US) ~ 1, TRUE ~ 0),
    UStoS_Miss = case_when(is.na(US_to_State) ~ 1, TRUE ~ 0),
    Sum_Intra_Sup = sum(Intra_Known * StoUS_Miss, na.rm = T),
    Sum_Intra_Dem = sum(Intra_Known * UStoS_Miss, na.rm = T),
    Missing_Supply = Div_to_US - Sum_Supply,
    Missing_Demand = US_to_Div - Sum_Demand,
    Excess_Supply = Missing_Supply - Sum_Intra_Sup,
    Excess_Demand = Missing_Demand - Sum_Intra_Dem,
    Sup_Milk_Share = case_when(
      sum(Milk_Prod * StoUS_Miss, na.rm = T) == 0 ~ 0,
      TRUE ~ (Milk_Prod * StoUS_Miss) / sum(Milk_Prod * StoUS_Miss, na.rm = T)
    ),
    Dem_Pop_Share = case_when(
      sum(Population * UStoS_Miss, na.rm = T) == 0 ~ 0,
      TRUE ~ (Population * UStoS_Miss) / sum(Population * UStoS_Miss, na.rm = T)
    ),
    Imputed_Supply = case_when(
      is.na(Intra_Known) ~ Sup_Milk_Share * Excess_Supply,
      TRUE ~ Sup_Milk_Share * Excess_Supply + Intra_Known
    ),
    Imputed_Demand = case_when(
      is.na(Intra_Known) ~ Dem_Pop_Share * Excess_Demand,
      TRUE ~ Dem_Pop_Share * Excess_Demand + Intra_Known
    ),
    Supply = case_when(StoUS_Miss == 1 ~ Imputed_Supply, TRUE ~ State_to_US),
    Demand = case_when(UStoS_Miss == 1 ~ Imputed_Demand, TRUE ~ US_to_State),
    Sup_Check = sum(Supply),
    Sup_Diff = Sup_Check - Div_to_US,
    Dem_Check = sum(Demand),
    Dem_Diff = Dem_Check - US_to_Div,
    Sup_lt_Intra = Supply - Intra_Known,
    Dem_lt_Intra = Demand - Intra_Known
  ) %>%
  group_by(COMM) %>%
  mutate(
    Sum_Supply = sum(Supply),
    Sum_Demand = sum(Demand),
    Sum_Diff = Sum_Supply - Sum_Demand,
    Share_Supply = Supply / sum(Supply),
    Share_Diff = Share_Supply * Sum_Diff,
    New_Supply = Supply - Share_Diff,
    Sum_Check = sum(New_Supply) - Sum_Demand
  ) %>%
  select(
    State,
    FMMO,
    COMM,
    Comm_Group,
    Supply = New_Supply,
    Demand,
    Intra_Known,
    Milk_Prod,
    Population
  )


# Calculate Tradeability Factor

cfs_dairy_tradable <- cfs_dairy_supply_demand %>%
  mutate(GAP = abs((Demand - Supply) / ((Demand + Supply) / 2))) %>%
  group_by(COMM) %>%
  mutate(
    AGAP = mean(GAP),
    Tradable = (1 + 0.5 * exp(5 * (AGAP - 1))) / (1 + exp(5 * (AGAP - 1)))
  ) %>%
  rowwise() %>%
  mutate(
    SHIN_Own = min(Supply / Demand, 1) * Tradable,
    Flow_Own = SHIN_Own * Demand,
    Flow_with_Known = case_when(
      is.na(Intra_Known) ~ Flow_Own,
      TRUE ~ Intra_Known
    ),
    SHIN_with_Known = Flow_with_Known / Demand
  )


# Calculate Degree of Specialization

cfs_dairy_specialization <- cfs_dairy_supply_demand %>%
  ungroup() %>%
  mutate(Sum_Products = sum(Supply), .by = State) %>%
  mutate(Sum_States = sum(Supply), .by = COMM) %>%
  mutate(
    Sum_All = sum(Supply),
    Special = case_when(
      Sum_Products > 0 ~ (Supply / Sum_Products) / (Sum_States / Sum_All),
      TRUE ~ 0
    )
  )


# Calculate SHIN and Flow:

cfs_dairy_flow <- faf_distance %>%
  full_join(
    select(
      cfs_dairy_supply_demand,
      Origin_State = State,
      Origin_Region = FMMO,
      COMM,
      Comm_Group,
      Supply,
      Origin_Milk = Milk_Prod
    ),
    by = c("Origin_State"),
    relationship = "many-to-many"
  ) %>%
  full_join(
    select(
      cfs_dairy_supply_demand,
      Dest_State = State,
      Dest_Region = FMMO,
      COMM,
      Comm_Group,
      Demand,
      Dest_Pop = Population
    ),
    by = c("Dest_State", "COMM", "Comm_Group")
  ) %>%
  full_join(
    select(
      cfs_dairy_tradable,
      Dest_State = State,
      Dest_Region = FMMO,
      COMM,
      Comm_Group,
      Tradable,
      SHIN_Own,
      SHIN_with_Known
    ),
    by = c("Dest_State", "Dest_Region", "COMM", "Comm_Group")
  ) %>%
  full_join(
    select(
      cfs_dairy_specialization,
      Origin_State = State,
      Origin_Region = FMMO,
      COMM,
      Comm_Group,
      Special
    ),
    by = c("Origin_State", "Origin_Region", "COMM", "Comm_Group")
  ) %>%
  filter(Supply > 0, Demand > 0) %>%
  group_by(Origin_State, COMM) %>%
  mutate(
    G_Interior = (Origin_Milk * Dest_Pop * Special) / Distance,
    Sum_G_Interior = sum(G_Interior),
    G = case_when(Sum_G_Interior == 0 ~ 0, TRUE ~ Supply / Sum_G_Interior),
    Flow_Std = (Origin_Milk * Dest_Pop / Distance) * Special * G,
    Flow_Std_Known = case_when(
      Origin_State == Dest_State ~ SHIN_with_Known * Demand,
      TRUE ~ Flow_Std
    ),
    Flow_Std_Intrp = case_when(
      Origin_State == Dest_State ~ SHIN_Own * Demand,
      TRUE ~ Flow_Std
    )
  ) %>%
  group_by(COMM) %>%
  mutate(
    Prod_Supply = sum(Supply),
    Bracket1 = (Supply / Prod_Supply) / (Distance)
  ) %>%
  group_by(Dest_State, COMM) %>%
  mutate(
    Bracket2_Interpolate = (1 - SHIN_Own) /
      sum(Bracket1[Origin_State != Dest_State]),
    Bracket2_with_Known = (1 - SHIN_with_Known) /
      sum(Bracket1[Origin_State != Dest_State]),
    SHIN_Off_Interpolate = (Bracket1) * (Bracket2_Interpolate),
    SHIN_Off_with_Known = (Bracket1) * (Bracket2_with_Known),
    SHIN_Interpolate = case_when(
      Origin_State == Dest_State ~ SHIN_Own,
      TRUE ~ SHIN_Off_Interpolate
    ),
    SHIN_with_Known = case_when(
      Origin_State == Dest_State ~ SHIN_with_Known,
      TRUE ~ SHIN_Off_with_Known
    ),
    Flow_Ext_Intrp = SHIN_Interpolate * Demand,
    Flow_Ext_Known = SHIN_with_Known * Demand,
    Dem_Check_1 = sum(Flow_Ext_Intrp),
    Dem_Check_2 = sum(Flow_Ext_Known)
  ) %>%
  group_by(Origin_State, COMM) %>%
  mutate(
    Sup_Check_1 = sum(Flow_Ext_Intrp),
    Sup_Check_2 = sum(Flow_Ext_Known)
  ) %>%
  select(
    Origin_State,
    Origin_Region,
    Dest_State,
    Dest_Region,
    Comm_Group,
    COMM,
    Supply,
    Demand,
    Flow_Std_Known,
    Flow_Std_Intrp,
    Flow_Ext_Known,
    Flow_Ext_Intrp
  )


# Generate Output

cfs_dairy_flow_processed_ext_intrp <- cfs_dairy_flow %>%
  rename(Flow = Flow_Ext_Intrp) %>%
  split(.$COMM) %>%
  map(ras_correction) %>%
  bind_rows()

cfs_dairy_flow_processed_ext_known <- cfs_dairy_flow %>%
  rename(Flow = Flow_Ext_Known) %>%
  filter(Flow > 0) %>%
  split(.$COMM) %>%
  map(ras_correction) %>%
  bind_rows()

cfs_dairy_flow_processed_std_intrp <- cfs_dairy_flow %>%
  rename(Flow = Flow_Std_Intrp) %>%
  split(.$COMM) %>%
  map(ras_correction) %>%
  bind_rows()

cfs_dairy_flow_processed_std_known <- cfs_dairy_flow %>%
  rename(Flow = Flow_Std_Known) %>%
  filter(Flow > 0) %>%
  split(.$COMM) %>%
  map(ras_correction) %>%
  bind_rows()

# Merge Output from All Methods

cfs_dairy_flow_processed <- cfs_dairy_flow_processed_ext_intrp %>%
  rename(Flow_Ext_Intrp = Flow) %>%
  left_join(
    cfs_dairy_flow_processed_ext_known,
    by = c(
      "Origin_State",
      "Origin_Region",
      "Dest_State",
      "Dest_Region",
      "COMM",
      "Comm_Group"
    )
  ) %>%
  rename(Flow_Ext_Known = Flow) %>%
  left_join(
    cfs_dairy_flow_processed_std_intrp,
    by = c(
      "Origin_State",
      "Origin_Region",
      "Dest_State",
      "Dest_Region",
      "COMM",
      "Comm_Group"
    )
  ) %>%
  rename(Flow_Std_Intrp = Flow) %>%
  left_join(
    cfs_dairy_flow_processed_std_known,
    by = c(
      "Origin_State",
      "Origin_Region",
      "Dest_State",
      "Dest_Region",
      "COMM",
      "Comm_Group"
    )
  ) %>%
  rename(Flow_Std_Known = Flow) %>%
  mutate(
    Flow_Ext_Known = replace_na(Flow_Ext_Known, 0),
    Flow_Std_Known = replace_na(Flow_Std_Known, 0)
  )


# Write to CSV for GAMS

cfs_dairy_flow_processed %>%
  ungroup() %>%
  filter(Origin_Region != "Drop", Dest_Region != "Drop") %>%
  select(Origin_Region:Comm_Group, Flow = Flow_Ext_Known) %>%
  summarize(
    Value = sum(Flow) * 1000000,
    .by = c(Origin_Region, Dest_Region, Comm_Group)
  ) %>%
  arrange(Origin_Region, Dest_Region, Comm_Group) %>%
  bind_rows(census_dairy_imports_exports) %>%
  write_csv(
    here("GAMS CODE", "CSV DATA FILES", "CFS_Dairy_for_GAMS_Gravity.csv"),
    col_names = F
  )
