# Title: The Geographic Footprint of U.S. Dairy Policy
# Script: 6_Process_Feed_Use_Data.R
# Authors: Tristan Hanon
# Date: August 2026
#
# Description:
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

##### Setup ####################################################################

# Identify Project Root
here::i_am("R CODE/Data-Processing/6_Process_Feed_Use_Data.R")

# Load Packages
library(here)
library(tidyverse)


##### Clean FCAU Data ##########################################################

total_fcaus <- feed_grains_yearbook_raw %>%
  filter(str_detect(table_name, "Table 30"), year %in% 2016:2017) %>%
  mutate(
    Crop_Type = str_extract(commodity_group, "(?<=\\().*?(?=\\))"),
    Category = commodity %>%
      replace_values(
        "Cattle on feed" ~ "CattleFeed",
        "Cattle, other" ~ "OtherCattle",
        "Livestock, other" ~ "OtherLivestock",
        "All animals" ~ "Total"
      )
  ) %>%
  select(Crop_Type, Category, year, amount) %>%
  pivot_wider(names_from = year, names_prefix = "MY_", values_from = amount) %>%
  # 2017 FCAUs are weighted average of 2016 and 2017 marketing years:
  mutate(Total_FCAUs = (8 * MY_2016 + 4 * MY_2017) / 12) %>%
  select(-starts_with("MY")) %>%
  filter_out(Crop_Type == "GRCAU")


##### Clean Feed Use Data ######################################################

# Grains
grains_use_processed <- feed_grains_yearbook_raw %>%
  filter(
    when_any(
      str_detect(table_name, "Table 4"),
      str_detect(table_name, "Table 5"),
      str_detect(table_name, "Table 6"),
      str_detect(table_name, "Table 7")
    ),
    year %in% 2016:2017,
    frequency == "Annual",
    attribute %in% c("Feed and residual use", "Total domestic use")
  ) %>%
  # Convert million bushels to million pounds:
  mutate(
    Mil_Pounds = case_when(
      commodity %in% c("Corn", "Sorghum") ~ amount * 56,
      commodity == "Barley" ~ amount * 48,
      commodity == "Oats" ~ amount * 32
    )
  ) %>%
  select(commodity, attribute, year, Mil_Pounds) %>%
  pivot_wider(
    names_from = year,
    names_prefix = "MY_",
    values_from = Mil_Pounds
  ) %>%
  mutate(
    Mil_Pounds = (8 * MY_2016 + 4 * MY_2017) / 12,
    commodity = commodity %>%
      replace_values(
        c("Sorghum", "Barley", "Oats") ~ "Other-Grains"
      ),
    attribute = attribute %>%
      recode_values(
        "Feed and residual use" ~ "Feed_Use",
        "Total domestic use" ~ "Domestic_Use",
      )
  ) %>%
  select(-starts_with("MY")) %>%
  summarize(
    Mil_Pounds = sum(Mil_Pounds),
    .by = c(commodity, attribute)
  ) %>%
  pivot_wider(names_from = attribute, values_from = Mil_Pounds)

# Corn Ethanol Byproducts
corn_byproducts_processed <- bioenergy_stats_raw %>%
  filter(
    table %in% c(8.1, 8.2, 8.3),
    year %in% 2016:2017,
    data_item == "Domestic use"
  ) %>%
  select(commodity, attribute = data_item, year, value) %>%
  pivot_wider(
    names_from = year,
    names_prefix = "MY_",
    values_from = value
  ) %>%
  mutate(
    # Weighted average of marketing years:
    Feed_Use = ((8 * MY_2016 + 4 * MY_2017) / 12) * 2204.623,
    # Assumption is all corn byproducts used domestically are used for feed:
    Domestic_Use = Feed_Use,
    commodity = "Corn"
  ) %>%
  select(commodity, Feed_Use, Domestic_Use) %>%
  summarize(
    Feed_Use = sum(Feed_Use),
    Domestic_Use = sum(Domestic_Use),
    .by = commodity
  )

# Oilseeds
oilseed_use_processed <- oil_crops_yearbook_raw %>%
  filter(
    Table_number %in% c(3:5, 18:20, 22:27, 29:31),
    Marketing_Year %in% c("2016/17", "2017/18"),
    Attribute_Desc %in%
      c(
        "Feed and residual use",
        "Other domestic use",
        "Nonoil use and seed use",
        "Seed use",
        "Residual use",
        "Crush",
        "Exports",
        "Total domestic use",
        "Total disappearance"
      )
  ) %>%
  select(
    commodity = Commodity_Desc,
    attribute = Attribute_Desc,
    year = Marketing_Year,
    MY_Definition,
    unit = Unit_Desc,
    Amount
  ) %>%
  pivot_wider(
    names_from = year,
    names_prefix = "MY_",
    values_from = Amount
  ) %>%
  separate_wider_delim(
    MY_Definition,
    delim = "\u2013",
    names = c("MY_Start", "MY_End")
  ) %>%
  mutate(
    # Determine interval lengths of start and end of marketing years:
    MY_Start = interval(ymd(paste0("2017", MY_Start, "01")), ymd(20180101)) %/%
      months(1),
    MY_End = interval(
      ymd(20161231),
      rollforward(ymd(paste0("2017", MY_End, "01")))
    ) %/%
      months(1),
    Amount = case_when(
      # Calculate weighted average of marketing years using start and end months
      # and convert all quantities to million pounds:
      unit == "Thousand short tons" ~
        ((MY_End * `MY_2016/17` + MY_Start * `MY_2017/18`) / 12) * 2,
      commodity == "Soybeans" & unit == "Million bushels" ~
        ((MY_End * `MY_2016/17` + MY_Start * `MY_2017/18`) / 12) * 60,
      commodity == "Flaxseed" & unit == "Thousand bushels" ~
        (((MY_End * `MY_2016/17` + MY_Start * `MY_2017/18`) / 12) * 56) / 1000,
      .default = (MY_End * `MY_2016/17` + MY_Start * `MY_2017/18`) / 12
    ),
    attribute = case_when(
      # Soybean feed use and cottonseed other uses assumed to be feed use:
      attribute %in%
        c(
          "Feed and residual use",
          "Residual use",
          "Other domestic use"
        ) ~ "Feed_Residual_Use",
      str_detect(
        attribute,
        regex("seed use", ignore_case = TRUE)
      ) ~ "Other_Dom_Use",
      str_detect(commodity, "meal") &
        attribute == "Total domestic use" ~ "Meal_Dom_Use",
      str_detect(commodity, "oil") &
        attribute == "Total domestic use" ~ "Oil_Dom_Use",
      str_detect(commodity, "meal|oil") &
        attribute %in% c("Total disappearance", "Exports") ~ "DROP",
      .default = str_replace(str_to_title(attribute), " ", "_")
    ),
    commodity = case_when(
      str_detect(commodity, "Soybean") ~ "Soybeans",
      str_detect(commodity, "Linseed") ~ "Flaxseed",
      .default = str_replace(commodity, " meal| oil", "")
    )
  ) %>%
  select(-c(unit, starts_with("MY"))) %>%
  filter_out(attribute == "DROP") %>%
  pivot_wider(names_from = attribute, values_from = Amount, values_fill = 0) %>%
  mutate(
    Feed_Residual_Use = case_when(
      commodity == "Canola" ~ Total_Disappearance - Exports - Crush,
      .default = Feed_Residual_Use
    ),
    Crush_Feed_Use = Crush * (Meal_Dom_Use / (Meal_Dom_Use + Oil_Dom_Use)),
    Feed_Use = Feed_Residual_Use + Crush_Feed_Use,
    Domestic_Use = Total_Disappearance - Exports,
    commodity = case_when(
      commodity != "Soybeans" ~ "Other-Oilseeds",
      .default = commodity
    )
  ) %>%
  summarize(
    Feed_Use = sum(Feed_Use),
    Domestic_Use = sum(Domestic_Use),
    .by = commodity
  )


##### Clean Livestock Inventory Data ###########################################

livestock_2017_processed <- nass_livestock_data_raw %>%
  filter(
    Year == 2017,
    when_any(Program == "CENSUS", str_detect(`Data Item`, "HEIFERS"))
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
    names_from = Category,
    values_from = Value,
    values_fill = 0
  ) %>%
  mutate(OtherCattle = TotalCattle - Dairy - CattleFeed) %>%
  select(-TotalCattle) %>%
  pivot_longer(
    -c(Year, State),
    names_to = "Category",
    values_to = "Inventory"
  ) %>%
  mutate(Share = Inventory / sum(Inventory), .by = "Category")


##### Calculate State-Level FCAUs ##############################################

state_fcau_shares <- left_join(
  livestock_2017_processed,
  filter_out(total_fcaus, Category == "Total"),
  by = "Category",
  relationship = "many-to-many"
) %>%
  mutate(
    State_FCAUs = (Total_FCAUs * 1000000) * Share,
    Crop_Type = Crop_Type %>%
      recode_values(
        "GCAU" ~ "Grains",
        "HPAU" ~ "Oilseeds",
        "RCAU" ~ "Hay"
      ),
    Total = sum(State_FCAUs),
    .by = c(State, Crop_Type)
  ) %>%
  select(State, Crop_Type, Category, State_FCAUs, Total) %>%
  pivot_wider(names_from = Category, values_from = State_FCAUs) %>%
  mutate(
    Dairy_Share = Dairy / Total,
    Share_FCAU = Total / sum(Total),
    .by = Crop_Type
  ) %>%
  select(State, Crop_Type, Dairy_Share, Share_FCAU) %>%
  write_csv(here("Data", "Processed", "State_FCAU_Shares.csv"))


##### Calculate Domestic Feed Use Shares #######################################

crop_feed_shares <- bind_rows(
  grains_use_processed,
  corn_byproducts_processed,
  oilseed_use_processed
) %>%
  summarize(
    Feed_Use = sum(Feed_Use),
    Domestic_Use = sum(Domestic_Use),
    .by = commodity
  ) %>%
  mutate(Dom_Share_Feed = Feed_Use / Domestic_Use) %>%
  select(Comm_Group = commodity, Dom_Share_Feed) %>%
  bind_rows(
    tribble(
      ~Comm_Group , ~Dom_Share_Feed ,
      "Hay"       ,               1 ,
      "Silage"    ,               1
    )
  ) %>%
  write_csv(here("Data", "Processed", "Crop_Feed_Shares.csv"))


##### Output Table C.3 #########################################################

sink(here("MANUSCRIPT TABLES", "Supplemental Material", "Table_C3.txt"))
crop_feed_shares %>%
  mutate(
    Dom_Share_Feed = format(round(Dom_Share_Feed, 3), nsmall = 3),
    Comm_Group = Comm_Group %>%
      replace_values(
        "Other-Grains" ~ "Other grains",
        "Other-Oilseeds" ~ "Other oilseeds"
      )
  ) %>%
  rename(Commodity = Comm_Group, Share = Dom_Share_Feed) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
sink()


##### Output Table C.4 #########################################################

sink(here("MANUSCRIPT TABLES", "Supplemental Material", "Table_C4.txt"))
total_fcaus %>%
  mutate(
    `Feed type` = Crop_Type %>%
      recode_values(
        "GCAU" ~ "Grains",
        "HPAU" ~ "High-protein",
        "RCAU" ~ "Roughage"
      ),
    Category = Category %>%
      replace_values(
        "CattleFeed" ~ "Cattle on feed",
        "OtherCattle" ~ "Other cattle",
        "OtherLivestock" ~ "Other livestock"
      ),
    Total_FCAUs = round(Total_FCAUs, 2),
    .keep = "none"
  ) %>%
  pivot_wider(names_from = Category, values_from = Total_FCAUs) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
sink()
