# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 3_Process_Component_Values_Volumes.R
# Authors: Tristan Hanon
# Date: August 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R/3_Process_Component_Values_Volumes.R")

# Load Packages
library(here)
library(tidyverse)


##### Load Intermediate Processed Data #########################################

# NOTE: These intermediate files were created in the process of calculating the
#   milk shipments in the script 2_Process_Milk_Shipments.R. If that script was
#   recently run, the data may already be available in memory, but this step
#   ensures they will be loaded.

# Load Total Milk Utilization
milk_utilization <- read_csv(
  here("Data", "Processed", "Milk_Utilization.csv")
)

# Load Total Milk Production
milk_production <- read_csv(
  here("Data", "Processed", "Milk_Production.csv")
)

# Load Total Milk Not Pooled
milk_not_pooled <- read_csv(
  here("Data", "Processed", "Milk_Not_Pooled.csv")
)


##### Merge Utilization Data ###################################################

util_data_merged <- tibble(
  Class = c("Beverage", "Softs", "Cheese", "Butter-Powder"),
  Data = list(
    class1_util_raw,
    class2_util_raw,
    class3_util_raw,
    class4_util_raw
  )
) %>%
  unnest(Data) %>%
  filter_out(is.na(`Order No`)) %>%
  select(Region = `Federal Milk Marketing Order Area`, Class, Jan:YTD) %>%
  mutate(Region = str_replace(Region, " ", "-"))


##### Clean and Merge Component Test Data ######################################

# California Component Volumes
ca_components_processed <- ca_data_raw %>%
  filter(Measure %in% c("Fat", "NonfatSolids")) %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  mutate(
    Region = "California",
    Class = CA_Class %>%
      recode_values(
        "1" ~ "Beverage",
        c("2", "3") ~ "Softs",
        "4b" ~ "Cheese",
        "4a" ~ "Butter-Powder"
      )
  ) %>%
  summarize(
    Fat_Pounds = sum(Fat),
    NFS_Pounds = sum(NonfatSolids),
    .by = c(Region, Class)
  ) %>%
  left_join(
    select(util_volumes_processed, Region, Class, Class_Volume),
    by = c("Region", "Class")
  ) %>%
  mutate(
    Fat = (Fat_Pounds / Class_Volume) * 100,
    NFS = (NFS_Pounds / Class_Volume) * 100
  )

# Fat Test Data
fat_data_processed <- tibble(
  Class = c("Beverage", "Softs", "Cheese", "Butter-Powder"),
  Component = "Fat",
  Data = list(
    class1_fat_raw,
    class2_fat_raw,
    class3_fat_raw,
    class4_fat_raw
  )
) %>%
  mutate(
    Data = map(
      Data,
      \(data) {
        data %>%
          filter_out(is.na(`Order No`)) %>%
          select(Region = `Federal Milk Marketing Order Area`, Test_Pct = YTD)
      }
    )
  ) %>%
  unnest(Data) %>%
  relocate(Region)

# Nonfat Solids Test Data
nfs_data_processed <- tibble(
  Class = c("Beverage", "Softs", "Butter-Powder"),
  Component = "NFS",
  Data = list(
    class1_nfs_raw,
    class2_nfs_raw,
    class4_nfs_raw
  )
) %>%
  mutate(
    Data = map(
      Data,
      \(data) {
        data %>%
          filter_out(is.na(`Order No`)) %>%
          select(Region = `Federal Milk Marketing Order Area`, Test_Pct = YTD)
      }
    )
  ) %>%
  unnest(Data) %>%
  relocate(Region)

# Protein and Other Solids Test Data
prot_os_data_processed <- tibble(
  Class = rep("Cheese", 2),
  Component = c("Protein", "OS"),
  Data = list(
    class3_protein_raw,
    class3_os_raw
  )
) %>%
  mutate(
    Data = map(
      Data,
      \(data) {
        data %>%
          filter_out(is.na(`Order No`)) %>%
          select(Region = `Federal Milk Marketing Order Area`, Test_Pct = YTD)
      }
    )
  ) %>%
  unnest(Data) %>%
  relocate(Region)


# Merge Component Test Data
component_tests_processed <- bind_rows(
  fat_data_processed,
  nfs_data_processed,
  prot_os_data_processed
) %>%
  mutate(Region = str_replace(Region, " ", "-")) %>%
  pivot_wider(names_from = Component, values_from = Test_Pct) %>%
  bind_rows(
    ca_components_processed %>%
      select(Region, Class, Fat, NFS)
  )


##### Clean Price Data ###############################################

# Class I Milk Utilization Monthly
class1_milk_monthly <- class1_milk_raw %>%
  filter_out(is.na(`Order No`)) %>%
  select(Region = `Federal Milk Marketing Order Area`, Jan:Dec) %>%
  bind_rows(ca_class1_raw) %>%
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "Class1_Pounds")

class1_fat_monthly <- class1_fat_raw %>%
  filter_out(is.na(`Order No`)) %>%
  select(Region = `Federal Milk Marketing Order Area`, Jan:Dec) %>%
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "Class1_FatTest")

# Class I Prices
class1_prices_monthly <- adv_prices_raw %>%
  select(
    Date = `Report Date`,
    MilkPrice = `Base Class 1 Price`,
    SkimPrice = `Base Skim Milk Class 1 Price`,
    FatPrice = `Advanced Butterfat Factor`
  ) %>%
  mutate(
    Month = mdy(Date) %>%
      rollforward(roll_to_first = TRUE) %>%
      month(label = TRUE)
  ) %>%
  select(-Date)

# Class I Prices
class1_prices_processed <- left_join(
  class1_milk_monthly,
  class1_fat_monthly,
  by = c("Region", "Month")
) %>%
  left_join(class1_prices_monthly, by = "Month") %>%
  left_join(class1_diffs, by = "Region") %>%
  mutate(
    Region = str_replace(Region, " ", "-"),
    Class1_FatPounds = case_when(
      Region == "California" ~ Class1_Pounds,
      .default = Class1_Pounds * (Class1_FatTest / 100)
    ),
    Class1_SkimPounds = case_when(
      Region == "California" ~ Class1_Pounds,
      .default = Class1_Pounds - Class1_FatPounds
    ),
    MilkPrice = MilkPrice + Class1_Diff,
    SkimPrice = SkimPrice + Class1_Diff,
    FatPrice = FatPrice + (Class1_Diff / 100)
  ) %>%
  summarize(
    Class = "Beverage",
    MilkPrice = weighted.mean(MilkPrice, Class1_Pounds) %>% round(2),
    SkimPrice = weighted.mean(SkimPrice, Class1_SkimPounds) %>% round(2),
    FatPrice = weighted.mean(FatPrice, Class1_FatPounds) %>% round(4),
    .by = Region
  )

# Class II, III, and IV Prices
class_prices_processed <- class_prices_raw %>%
  summarize(
    Softs_MilkPrice = mean(`Class 2 Price`) %>% round(2),
    Softs_FatPrice = mean(`Class 2 Butterfat Price`) %>% round(4),
    Cheese_MilkPrice = mean(`Class 3 Price`) %>% round(2),
    Cheese_SkimPrice = mean(`Class 3 Skim Milk Price`) %>% round(2),
    `Butter-Powder_MilkPrice` = mean(`Class 4 Price`) %>% round(2),
    `Butter-Powder_SkimPrice` = mean(`Class 4 Skim Milk Price`) %>% round(2),
    Cheese_FatPrice = mean(`Butterfat Price`) %>% round(4),
    `Butter-Powder_FatPrice` = mean(`Butterfat Price`) %>% round(4),
    `Butter-Powder_NFSPrice` = mean(`Nonfat Solids Price`) %>% round(4),
    Cheese_ProtPrice = mean(`Protein Price`) %>% round(4),
    Cheese_OSPrice = mean(`Other Solids Price`) %>% round(4),
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Class", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(
    SkimPrice = SkimPrice %>%
      replace_values(NA ~ (MilkPrice - FatPrice * 3.5) / 0.965),
    NFSPrice = case_when(
      Class == "Softs" ~ SkimPrice / 9,
      .default = NFSPrice
    )
  )


##### Process Dairy Product Production Data ####################################

# Clean NASS Dairy Products Data
dairy_products_processed <- dairy_products_raw %>%
  separate(`Data Item`, into = c("Long_Product", "Drop"), sep = " - ") %>%
  mutate(
    State = str_to_title(State),
    Region = str_to_title(Region),
    Product = Long_Product %>%
      recode_values(
        from = dairy_product_groups$NASS_Name,
        to = dairy_product_groups$Category
      )
  ) %>%
  left_join(comp_conversions, by = "Product") %>%
  mutate(
    Production = case_when(
      is.na(Gal_to_Lbs) ~ Value,
      .default = Value * Gal_to_Lbs
    )
  ) %>%
  select(State, Region, Product, Production)

# Summarize total production for states with reported values:
dairy_products_states <- dairy_products_processed %>%
  filter(State %in% continental_states) %>%
  mutate(
    Region = State %>%
      recode_values(from = nass_regions$State, to = nass_regions$Region)
  ) %>%
  summarize(State_Prod = sum(Production), .by = c(State, Region, Product))

# Summarize total production at NASS regional level (Atlantic, Central, West):
dairy_products_regions <- dairy_products_processed %>%
  filter_out(is.na(Region)) %>%
  summarize(Total_Prod = sum(Production), .by = c(Region, Product))

# Calculate difference between known state-level production and regional total:
dairy_products_other <- dairy_products_states %>%
  summarize(Known_Prod = sum(State_Prod), .by = c(Region, Product)) %>%
  full_join(dairy_products_regions, by = c("Region", "Product")) %>%
  mutate(
    Other_Prod = case_when(
      is.na(Known_Prod) ~ Total_Prod,
      .default = Total_Prod - Known_Prod
    )
  ) %>%
  select(Region, Product, Other_Prod)

# Calculate component use by FMMO Region and Product Class:
component_use_fmmo <- milk_prod_raw %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  mutate(
    State = str_to_title(State),
    Region = State %>%
      recode_values(from = nass_regions$State, to = nass_regions$Region),
    FMMO = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      )
  ) %>%
  select(State, Region, FMMO, Milk_Prod = Value) %>%
  full_join(
    dairy_products_other,
    by = "Region",
    relationship = "many-to-many"
  ) %>%
  full_join(dairy_products_states, by = c("State", "Region", "Product")) %>%
  mutate(
    Remaining_Milk = case_when(
      is.na(State_Prod) ~ Milk_Prod,
      .default = NA_real_
    ),
    Remaining_Milk_Share = Remaining_Milk / sum(Remaining_Milk, na.rm = T),
    Production = case_when(
      is.na(State_Prod) ~ Remaining_Milk_Share * Other_Prod,
      .default = State_Prod
    ),
    .by = c(Region, Product)
  ) %>%
  left_join(comp_conversions, by = "Product") %>%
  mutate(
    Fat_Use = Production * (Fat_Share / 100),
    SNF_Use = Production * (SNF_Share / 100),
    FMMO = factor(FMMO, levels = fmmo_regions),
    FMMO_Class = Product %>%
      recode_values(
        c("Cheese", "Whey") ~ "Cheese",
        c("Butter", "Condensed_Milk", "NFDM") ~ "Butter-Powder",
        default = "Softs"
      ) %>%
      factor(levels = c("Softs", "Cheese", "Butter-Powder"))
  ) %>%
  summarize(
    Fat_Use = sum(Fat_Use),
    NFS_Use = sum(SNF_Use),
    .by = c(FMMO, FMMO_Class)
  ) %>%
  mutate(
    Fat_Util = Fat_Use / sum(Fat_Use),
    NFS_Util = NFS_Use / sum(NFS_Use),
    .by = FMMO
  ) %>%
  arrange(FMMO_Class, FMMO)


##### Estimate Utilization of Milk Not Pooled ##################################

# Clean Producer Receipts Data
fmmo_receipts_processed <- fmmo_receipts_raw %>%
  filter_out(is.na(`Order No`)) %>%
  select(Region = `Federal Milk Marketing Order Area`, Jan:Dec) %>%
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "Receipts") %>%
  mutate(Region = str_replace(Region, " ", "-"))

# Estimate Utilization
nonpool_utilization <- util_data_merged %>%
  filter_out(Class == "Beverage") %>%
  select(-YTD) %>%
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "Utilization") %>%
  left_join(fmmo_receipts_processed, by = c("Region", "Month")) %>%
  mutate(
    Avg_Util = mean(Utilization),
    Days = days_in_month(ymd(paste0(2017, Month, 1))),
    Daily_Util = (Receipts / Days) * (Utilization / 100),
    Avg_Daily_Util = mean(Daily_Util),
    Below_Avg_Daily_Util = case_when(
      Utilization < Avg_Util & Daily_Util < Avg_Daily_Util ~ 1,
      .default = 0
    ),
    Diff_Daily_Util = Daily_Util - Avg_Daily_Util,
    .by = c(Region, Class)
  ) %>%
  summarize(
    Avg_Diff = sum(Below_Avg_Daily_Util * Diff_Daily_Util),
    .by = c(Region, Class)
  ) %>%
  mutate(Est_NonPool_Util = (Avg_Diff / sum(Avg_Diff)) * 100, .by = Region) %>%
  select(Region, Class, Est_NonPool_Util)

# Output Table C.2
sink(here("MANUSCRIPT TABLES", "Supplemental Material", "Table_C2.txt"))
nonpool_utilization %>%
  bind_rows(
    tibble(
      Region = rep("California", 3),
      Class = c("Softs", "Cheese", "Butter-Powder"),
      Est_NonPool_Util = c(0, 100, 0)
    )
  ) %>%
  mutate(
    Est_NonPool_Util = round(Est_NonPool_Util),
    Region = Region %>%
      str_replace("-", " ") %>%
      factor(levels = str_replace(fmmo_regions, "-", " "))
  ) %>%
  arrange(Region) %>%
  pivot_wider(names_from = Class, values_from = Est_NonPool_Util) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
sink()


##### Calculate Milk Utilization Volumes by Class ##############################

# Process California Utilization Data
ca_util_processed <- ca_data_raw %>%
  filter(Measure == "Utilization") %>%
  mutate(
    Region = "California",
    Class = CA_Class %>%
      recode_values(
        "1" ~ "Beverage",
        c("2", "3") ~ "Softs",
        "4b" ~ "Cheese",
        "4a" ~ "Butter-Powder"
      )
  ) %>%
  summarize(Util_Share = sum(Value), .by = c(Region, Class))

# Utilization Volumes by Region and Class
util_volumes_processed <- util_data_merged %>%
  select(-(Jan:Dec), Util_Share = YTD) %>%
  bind_rows(ca_util_processed) %>%
  mutate(Region = str_replace(Region, " ", "-")) %>%
  left_join(milk_utilization, by = "Region") %>%
  left_join(milk_not_pooled, by = "Region") %>%
  mutate(
    Total_Pooled = Utilization - Not_Pooled,
    Class_Volume = Total_Pooled * (Util_Share / 100)
  ) %>%
  bind_rows(wny_data_raw) %>%
  mutate(
    Region = Region %>%
      replace_values("Western-New-York" ~ "Northeast")
  ) %>%
  summarize(
    Utilization = sum(Utilization, na.rm = TRUE),
    Class_Volume = sum(Class_Volume),
    .by = c(Region, Class)
  ) %>%
  mutate(
    Not_Pooled = Utilization - sum(Class_Volume),
    .by = Region
  )


##### Calculate Component Volumes and Values for Pooled Milk ###################

# Beverage Component Volumes and Values
beverage_comp_value_volume <- util_volumes_processed %>%
  filter(Class == "Beverage") %>%
  left_join(component_tests_processed, by = c("Region", "Class")) %>%
  left_join(class1_prices_processed, by = c("Region", "Class")) %>%
  left_join(
    ca_components_processed %>% select(Region, Class, ends_with("Pounds")),
    by = c("Region", "Class")
  ) %>%
  mutate(
    Fat_Pounds = Fat_Pounds %>%
      replace_values(NA ~ Class_Volume * (Fat / 100)),
    NFS_Pounds = NFS_Pounds %>%
      replace_values(NA ~ Class_Volume * (NFS / 100)),
    SkimPounds = Class_Volume - Fat_Pounds,
    Fat_Value = Fat_Pounds * FatPrice,
    SkimValue = (SkimPounds / 100) * SkimPrice,
    Protein_Value = SkimValue * (3.1 / 9),
    OS_Value = SkimValue * (5.9 / 9),
    Protein_Pounds = case_when(
      is.na(NFS_Pounds) ~ SkimPounds * (3.1 / (100 - Fat)),
      .default = NFS_Pounds * (3.1 / 9)
    ),
    OS_Pounds = case_when(
      is.na(NFS_Pounds) ~ SkimPounds * (5.9 / (100 - Fat)),
      .default = NFS_Pounds * (5.9 / 9)
    )
  ) %>%
  select(
    Region,
    Class,
    Fat_Pounds,
    Protein_Pounds,
    OS_Pounds,
    Fat_Value,
    Protein_Value,
    OS_Value
  )

# Soft Products Component Volumes and Values
softs_comp_value_volume <- util_volumes_processed %>%
  filter(Class == "Softs") %>%
  left_join(component_tests_processed, by = c("Region", "Class")) %>%
  left_join(class_prices_processed, by = "Class") %>%
  left_join(
    ca_components_processed %>% select(Region, Class, ends_with("Pounds")),
    by = c("Region", "Class")
  ) %>%
  mutate(
    Fat_Pounds = Fat_Pounds %>%
      replace_values(NA ~ Class_Volume * (Fat / 100)),
    NFS_Pounds = NFS_Pounds %>%
      replace_values(NA ~ Class_Volume * (NFS / 100)),
    SkimPounds = Class_Volume - Fat_Pounds,
    Fat_Value = Fat_Pounds * FatPrice,
    SkimOrNFSValue = case_when(
      is.na(NFS_Pounds) ~ (SkimPounds / 100) * SkimPrice,
      .default = NFS_Pounds * NFSPrice
    ),
    Protein_Value = SkimOrNFSValue * (3.1 / 9),
    OS_Value = SkimOrNFSValue * (5.9 / 9),
    Protein_Pounds = case_when(
      is.na(NFS_Pounds) ~ SkimPounds * (3.1 / (100 - Fat)),
      .default = NFS_Pounds * (3.1 / 9)
    ),
    OS_Pounds = case_when(
      is.na(NFS_Pounds) ~ SkimPounds * (5.9 / (100 - Fat)),
      .default = NFS_Pounds * (5.9 / 9)
    )
  ) %>%
  select(
    Region,
    Class,
    Fat_Pounds,
    Protein_Pounds,
    OS_Pounds,
    Fat_Value,
    Protein_Value,
    OS_Value
  )

# Cheese Component Volumes and Values
cheese_comp_value_volume <- util_volumes_processed %>%
  filter(Class == "Cheese") %>%
  left_join(component_tests_processed, by = c("Region", "Class")) %>%
  left_join(class_prices_processed, by = "Class") %>%
  left_join(
    ca_components_processed %>% select(Region, Class, ends_with("Pounds")),
    by = c("Region", "Class")
  ) %>%
  mutate(
    Fat_Pounds = Fat_Pounds %>%
      replace_values(NA ~ Class_Volume * (Fat / 100)),
    SkimPounds = Class_Volume - Fat_Pounds,
    Protein_Pounds = case_when(
      Region == "California" ~ NFS_Pounds *
        (mean(Protein, na.rm = T) /
          (mean(Protein, na.rm = T) + mean(OS, na.rm = T))),
      is.na(Protein) ~ SkimPounds * (3.1 / (100 - Fat)),
      .default = Class_Volume * (Protein / 100)
    ),
    OS_Pounds = case_when(
      Region == "California" ~ NFS_Pounds *
        (mean(OS, na.rm = T) /
          (mean(Protein, na.rm = T) + mean(OS, na.rm = T))),
      is.na(OS) ~ SkimPounds * (5.9 / (100 - Fat)),
      .default = Class_Volume * (OS / 100)
    ),
    SkimValue = (SkimPounds / 100) * SkimPrice,
    Fat_Value = Fat_Pounds * FatPrice,
    Protein_Value = case_when(
      is.na(Protein) ~ SkimValue * (3.1 / 9),
      .default = Protein_Pounds * ProtPrice
    ),
    OS_Value = case_when(
      is.na(OS) ~ SkimValue * (5.9 / 9),
      .default = OS_Pounds * OSPrice
    )
  ) %>%
  select(
    Region,
    Class,
    Fat_Pounds,
    Protein_Pounds,
    OS_Pounds,
    Fat_Value,
    Protein_Value,
    OS_Value
  )

# Butter-Powder Component Volumes and Values
butterpowder_comp_value_volume <- util_volumes_processed %>%
  filter(Class == "Butter-Powder") %>%
  left_join(component_tests_processed, by = c("Region", "Class")) %>%
  left_join(class_prices_processed, by = "Class") %>%
  left_join(
    ca_components_processed %>% select(Region, Class, ends_with("Pounds")),
    by = c("Region", "Class")
  ) %>%
  mutate(
    Fat_Pounds = Fat_Pounds %>%
      replace_values(NA ~ Class_Volume * (Fat / 100)),
    NFS_Pounds = NFS_Pounds %>%
      replace_values(NA ~ Class_Volume * (NFS / 100)),
    SkimPounds = Class_Volume - Fat_Pounds,
    Fat_Value = Fat_Pounds * FatPrice,
    SkimOrNFSValue = case_when(
      is.na(NFS_Pounds) ~ (SkimPounds / 100) * SkimPrice,
      .default = NFS_Pounds * NFSPrice
    ),
    Protein_Value = SkimOrNFSValue * (3.1 / 9),
    OS_Value = SkimOrNFSValue * (5.9 / 9),
    Protein_Pounds = case_when(
      is.na(NFS_Pounds) ~ SkimPounds * (3.1 / (100 - Fat)),
      .default = NFS_Pounds * (3.1 / 9)
    ),
    OS_Pounds = case_when(
      is.na(NFS_Pounds) ~ SkimPounds * (5.9 / (100 - Fat)),
      .default = NFS_Pounds * (5.9 / 9)
    )
  ) %>%
  select(
    Region,
    Class,
    Fat_Pounds,
    Protein_Pounds,
    OS_Pounds,
    Fat_Value,
    Protein_Value,
    OS_Value
  )

# Merge Pooled Milk Component Volumes and Values
comp_volumes_values_pooled <- bind_rows(
  beverage_comp_value_volume,
  softs_comp_value_volume,
  cheese_comp_value_volume,
  butterpowder_comp_value_volume
) %>%
  pivot_longer(
    -c(Region, Class),
    names_to = c("Component", ".value"),
    names_pattern = "(.*)_(.*)"
  )


##### Calculate Component Volumes and Values for Milk Not Pooled ###############

comp_volumes_values_nonpool <- util_volumes_processed %>%
  select(Region, Class, Not_Pooled) %>%
  filter_out(Class == "Beverage") %>%
  left_join(nonpool_utilization, by = c("Region", "Class")) %>%
  mutate(
    Est_NonPool_Util = case_when(
      Region == "California" & Class == "Cheese" ~ 1,
      is.na(Est_NonPool_Util) ~ 0,
      .default = Est_NonPool_Util
    ),
    NonPool_Class_Volume = Not_Pooled * (Est_NonPool_Util / 100)
  ) %>%
  left_join(component_tests_processed, by = c("Region", "Class")) %>%
  bind_cols(
    class_prices_processed %>%
      filter(Class == "Butter-Powder") %>%
      select(FatPrice, NFSPrice)
  ) %>%
  mutate(
    NFS = NFS %>%
      replace_values(NA ~ mean(NFS, na.rm = T)),
    Protein = Protein %>%
      replace_values(NA ~ mean(Protein, na.rm = T)),
    OS = OS %>%
      replace_values(NA ~ mean(OS, na.rm = T)),
    Fat_Pounds = NonPool_Class_Volume * (Fat / 100),
    Protein_Pounds = case_when(
      Class == "Cheese" ~ NonPool_Class_Volume * (Protein / 100),
      .default = NonPool_Class_Volume * (NFS / 100) * (3.1 / 9)
    ),
    OS_Pounds = case_when(
      Class == "Cheese" ~ NonPool_Class_Volume * (OS / 100),
      .default = NonPool_Class_Volume * (NFS / 100) * (5.9 / 9)
    ),
    Fat_Value = Fat_Pounds * FatPrice,
    Protein_Value = Protein_Pounds * NFSPrice,
    OS_Value = OS_Pounds * NFSPrice,
    .by = Class
  ) %>%
  select(
    Region,
    Class,
    Fat_Pounds,
    Protein_Pounds,
    OS_Pounds,
    Fat_Value,
    Protein_Value,
    OS_Value
  ) %>%
  pivot_longer(
    -c(Region, Class),
    names_to = c("Component", ".value"),
    names_pattern = "(.*)_(.*)"
  )


##### Unregulated Utilization and Components ###################################

# Component Levels for All FMMO Regions
all_markets_comp_tests <- tibble(
  Component = c("Fat", "Protein", "OS"),
  Data = list(total_fat_raw, total_prot_raw, total_os_raw)
) %>%
  mutate(
    Data = map(
      Data,
      \(data) {
        data %>%
          filter(is.na(`Order No`)) %>%
          select(Test_Pct = YTD)
      }
    )
  ) %>%
  unnest(Data)

# Calculate Beverage Milk Consumption in Unregulated Region
beverage_milk_per_capita <- dairy_per_capita %>%
  filter(Year == 2017, Category == "Fluid beverage milk") %>%
  pull(Quantity)

unreg_beverage_use <- census_pop %>%
  mutate(Beverage_Consumption = Population * beverage_milk_per_capita) %>%
  summarize(Beverage_Consumption = sum(Beverage_Consumption), .by = FMMO) %>%
  filter(FMMO == "Unregulated") %>%
  pull(Beverage_Consumption)

# Calculate average Class I component tests across all FMMO regions:
avg_class1_tests <- util_volumes_processed %>%
  left_join(component_tests_processed, by = c("Region", "Class")) %>%
  filter(Class == "Beverage") %>%
  summarize(
    Fat_BevTest = weighted.mean(Fat, Class_Volume),
    NFS_BevTest = weighted.mean(NFS, Class_Volume, na.rm = T),
  ) %>%
  mutate(
    Protein_BevTest = NFS_BevTest * (3.1 / 9),
    OS_BevTest = NFS_BevTest * (5.9 / 9)
  ) %>%
  select(-NFS_BevTest) %>%
  pivot_longer(
    ends_with("Test"),
    names_to = c("Component", ".value"),
    names_pattern = ("(.*)_(.*)")
  )

# Extract and reformat Component Use by Class for Unregulated region:
unreg_comp_use <- component_use_fmmo %>%
  filter(FMMO == "Unregulated") %>%
  mutate(Protein = NFS_Util, OS = NFS_Util) %>%
  select(FMMO_Class, Fat = Fat_Util, Protein, OS) %>%
  pivot_wider(names_from = FMMO_Class, values_from = c(Fat, Protein, OS)) %>%
  pivot_longer(
    everything(),
    names_to = c("Component", ".value"),
    names_pattern = "(.*)_(.*)"
  )

# Estimate Unregulated Utilization by Class
comp_volumes_values_unreg <- milk_utilization %>%
  filter(Region == "Unregulated") %>%
  bind_cols(all_markets_comp_tests) %>%
  left_join(avg_class1_tests, by = "Component") %>%
  left_join(unreg_comp_use, by = "Component") %>%
  mutate(
    Comp_Volume = Utilization * (Test_Pct / 100),
    Beverage_Utilization = unreg_beverage_use,
    Beverage_Pounds = Beverage_Utilization * (BevTest / 100),
    Remaining_Volume = Comp_Volume - Beverage_Pounds,
    Softs_Pounds = Remaining_Volume * Softs,
    Cheese_Pounds = Remaining_Volume * Cheese,
    `Butter-Powder_Pounds` = Remaining_Volume * `Butter-Powder`
  ) %>%
  bind_cols(
    class_prices_processed %>%
      filter(Class == "Butter-Powder") %>%
      select(FatPrice, NFSPrice)
  ) %>%
  mutate(
    Beverage_Value = case_when(
      Component == "Fat" ~ Beverage_Pounds * FatPrice,
      .default = Beverage_Pounds * NFSPrice
    ),
    Softs_Value = case_when(
      Component == "Fat" ~ Softs_Pounds * FatPrice,
      .default = Softs_Pounds * NFSPrice
    ),
    Cheese_Value = case_when(
      Component == "Fat" ~ Cheese_Pounds * FatPrice,
      .default = Cheese_Pounds * NFSPrice
    ),
    `Butter-Powder_Value` = case_when(
      Component == "Fat" ~ `Butter-Powder_Pounds` * FatPrice,
      .default = `Butter-Powder_Pounds` * NFSPrice
    )
  ) %>%
  select(Region, Component, matches("*_Value|*_Pounds")) %>%
  pivot_longer(
    matches("*_Value|*_Pounds"),
    names_to = c("Class", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  relocate(Class, .before = Component)


##### Merge Component Volumes and Values #######################################

comp_volumes_values_total <- bind_rows(
  comp_volumes_values_pooled,
  comp_volumes_values_nonpool,
  comp_volumes_values_unreg
) %>%
  summarize(
    Pounds = sum(Pounds),
    Value = sum(Value),
    .by = c(Region, Class, Component)
  ) %>%
  mutate(
    Region = factor(Region, levels = fmmo_regions),
    Class = factor(
      Class,
      levels = c("Beverage", "Softs", "Cheese", "Butter-Powder")
    ),
    Component = Component %>%
      replace_values("OS" ~ "Other-Solids") %>%
      factor(levels = c("Fat", "Protein", "Other-Solids"))
  ) %>%
  arrange(Component, Class, Region)

# Output Component Volumes
comp_volumes_values_total %>%
  select(-Value) %>%
  write_csv(
    here("GAMS", "CSV DATA FILES", "Comp_Quantity_for_GAMS.csv"),
    col_names = F
  )

# Output Component Values
comp_volumes_values_total %>%
  select(-Pounds) %>%
  write_csv(
    here("GAMS", "CSV DATA FILES", "Comp_Value_for_GAMS.csv"),
    col_names = F
  )
