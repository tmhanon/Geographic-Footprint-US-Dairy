# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 5_Silage_Regressions.R
# Authors: Tristan Hanon
# Date: July 2026
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R/5_Silage_Regressions.R")

# Load Packages
library(here)
library(stargazer)
library(broom)
library(tidyverse)


##### Clean Data ###############################################################

# Silage Data
silage_data_processed <- silage_data_1519_raw %>%
  separate(`Data Item`, into = c("Commodity", "Measure"), sep = " - ") %>%
  separate(Measure, into = c("Measure", "Drop1"), sep = ",") %>%
  separate(Commodity, into = c("Commodity", "Drop2"), sep = ",") %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  select(Year, State, Commodity, Measure, Value) %>%
  mutate(
    State = str_to_title(State),
    Region = State %>%
      recode_values(
        from = state_region_match$State,
        to = state_region_match$Region
      ),
    Comm_Group = "Silage",
    Value = parse_number(Value, na = "(NA)")
  ) %>%
  pivot_wider(names_from = c(Commodity, Measure), values_from = Value) %>%
  mutate(
    Corn_Price = case_when(
      is.na(`CORN_PRICE RECEIVED`) ~ mean(`CORN_PRICE RECEIVED`, na.rm = T),
      .default = `CORN_PRICE RECEIVED`
    ),
    Silage_Price = Corn_Price * 8,
    Corn_Value = case_when(
      is.na(CORN_PRODUCTION) ~ 0,
      .default = CORN_PRODUCTION * Silage_Price
    ),
    Sorghum_Value = case_when(
      is.na(SORGHUM_PRODUCTION) ~ 0,
      .default = SORGHUM_PRODUCTION * 0.9 * Silage_Price
    ),
    Silage_Value = Corn_Value + Sorghum_Value,
    Silage_Quant = case_when(
      is.na(SORGHUM_PRODUCTION) ~ CORN_PRODUCTION,
      .default = CORN_PRODUCTION + SORGHUM_PRODUCTION
    ),
    .by = Year
  ) %>%
  select(Year, State, Region, Silage_Value, Silage_Quant)


# Haylage Data
haylage_data_processed <- haylage_data_1519 %>%
  filter(State %in% str_to_upper(continental_states)) %>%
  select(Year, State, Commodity, Value) %>%
  pivot_wider(names_from = Commodity, values_from = Value, values_fill = 0) %>%
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
  select(Year, State, Region, Value, HAYLAGE)


# Livestock Data
livestock_1519_processed <- nass_livestock_data_raw %>%
  filter(
    State %in% str_to_upper(continental_states),
    Program == "SURVEY",
    Commodity == "CATTLE"
  ) %>%
  mutate(
    State = str_to_title(State),
    Category = `Data Item` %>%
      recode_values(
        from = livestock_groups$NASS_Data_Item,
        to = livestock_groups$Category
      ),
    Value = parse_number(Value, na = c("(D)", "(NA)"))
  ) %>%
  summarize(Value = sum(Value, na.rm = T), .by = c(Year, State, Category)) %>%
  pivot_wider(
    names_from = "Category",
    values_from = "Value",
    values_fill = 0
  ) %>%
  mutate(OtherCattle = TotalCattle - Dairy - CattleFeed) %>%
  select(-TotalCattle)


# Merge Data
silage_livestock_data <- left_join(
  silage_data_processed,
  haylage_data_processed,
  by = c("Year", "State", "Region")
) %>%
  mutate(
    Silage_Value = Silage_Value + Value,
    Silage_Quant = Silage_Quant + HAYLAGE
  ) %>%
  select(-c(Value, HAYLAGE)) %>%
  left_join(
    livestock_1519_processed,
    by = c("Year", "State")
  )


##### Fit Regression Models ####################################################

silage_models <- silage_livestock_data %>%
  mutate(
    Merged_Region = Region %>%
      replace_values(
        c("California", "Pacific-Northwest", "Arizona") ~ "West",
        c("Southeast", "Florida") ~ "SE-FL",
        c("Southwest", "Central") ~ "SW-Cen",
        c("Upper-Midwest", "Mideast") ~ "UM-ME",
        c("Northeast", "Appalachian") ~ "NE-App"
      )
  ) %>%
  arrange(Merged_Region) %>%
  nest(.by = Merged_Region) %>%
  mutate(
    model = map(data, \(x) lm(Silage_Quant ~ Dairy + CattleFeed - 1, data = x)),
    results = map(model, tidy)
  )


##### Produce Appendix Table C.5 ###############################################

# Save fitted models separately:
regNEAP <- filter(silage_models, Merged_Region == "NE-App")$model[[1]]
regSEFL <- filter(silage_models, Merged_Region == "SE-FL")$model[[1]]
regUMME <- filter(silage_models, Merged_Region == "UM-ME")$model[[1]]
regCESW <- filter(silage_models, Merged_Region == "SW-Cen")$model[[1]]
regWest <- filter(silage_models, Merged_Region == "West")$model[[1]]
regUnre <- filter(silage_models, Merged_Region == "Unregulated")$model[[1]]

# Produce LaTeX table using {stargazer} and save as .txt file:
# NOTE: Run each line in order, i.e., sink(...), stargazer(...), sink().
sink(here("MANUSCRIPT TABLES", "Supplemental Material", "Table_C5.txt"))
stargazer(
  regNEAP,
  regSEFL,
  regUMME,
  regCESW,
  regWest,
  regUnre,
  dep.var.caption = "Silage Quantity",
  dep.var.labels.include = FALSE,
  column.labels = c(
    "Northeast-Appalachian",
    "Southeast-Florida",
    "Upper Midwest-Mideast",
    "Central-Southwest",
    "West",
    "Unregulated"
  ),
  column.separate = c(1, 1, 1, 1, 1, 1),
  covariate.labels = c("Dairy Cattle", "Cattle on Feed")
)
sink()


##### Calculate Dairy Share of Silage Allocation ###############################

# Calculate dairy share and save CSV:
dairy_silage_share <- silage_models %>%
  unnest(results) %>%
  select(-c(model, std.error, statistic)) %>%
  pivot_wider(
    names_from = term,
    values_from = c(estimate, p.value)
  ) %>%
  unnest(data) %>%
  filter(Year == 2017) %>%
  summarize(
    Dairy = sum(Dairy),
    CattleFeed = sum(CattleFeed),
    estimate_Dairy = mean(estimate_Dairy, na.rm = TRUE),
    estimate_CattleFeed = mean(estimate_CattleFeed, na.rm = TRUE),
    p.value_Dairy = mean(p.value_Dairy, na.rm = TRUE),
    p.value_CattleFeed = mean(p.value_CattleFeed, na.rm = TRUE),
    .by = Region
  ) %>%
  mutate(
    estimate_CattleFeed = case_when(
      is.nan(estimate_CattleFeed) ~ 0,
      p.value_CattleFeed > 0.2 ~ 0,
      .default = estimate_CattleFeed
    ),
    Dairy_Share = Dairy *
      estimate_Dairy /
      (Dairy * estimate_Dairy + CattleFeed * estimate_CattleFeed),
    CattleFeed_Share = 1 - Dairy_Share
  ) %>%
  arrange(Region) %>%
  select(Region, Dairy_Share, CattleFeed_Share) %>%
  write_csv(here("Data", "Processed", "Dairy_Silage_Share.csv"))

# Save Table C.6 as .txt file:
# NOTE: Run each line in order, i.e., sink(...), dairy_silage_share..., sink().
sink(here("MANUSCRIPT TABLES", "Supplemental Material", "Table_C6.txt"))
dairy_silage_share %>%
  mutate(
    Region = str_replace(Region, "-", " ") %>%
      factor(levels = str_replace(fmmo_regions, "-", " ")),
    `Dairy cattle` = round(Dairy_Share * 100),
    `Cattle on feed` = round(CattleFeed_Share * 100),
    .keep = "none"
  ) %>%
  arrange(Region) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
sink()
