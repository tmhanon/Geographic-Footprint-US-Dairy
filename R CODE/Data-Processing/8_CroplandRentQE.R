# Title: FMMO project
# Author: Pierre Merel
# Date: July 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

options(dplyr.summarise.inform = FALSE)
suppressPackageStartupMessages({
  library(tidyverse)
})

####COMPUTING CROPLAND VALUES BASED ON STATE-LEVEL DATA FROM USDA/NASS
LandRent <- read.csv(here::here("Data", "Raw", "CroplandRentState.csv"))
LandAcres <- read.csv(here::here("Data", "Raw", "CroplandAcresState.csv"))

LandAcresMerge <- LandAcres %>%
  dplyr::filter(!State %in% c("HAWAII", "ALASKA")) %>%
  dplyr::mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  dplyr::rename(STATEFP = "State.ANSI") %>%
  mutate(
    STATEFP = case_when(
      STATEFP < 10 ~ paste0("0", as.character(STATEFP)),
      T ~ as.character(STATEFP)
    )
  ) %>%
  dplyr::rename(Acres = "Value")

FO1STATES <- c("09", "10", "25", "33", "34", "44", "50", "11")
FO5STATES <- c("37", "45")
FO7STATES <- c("01", "05", "22", "28")
FO32STATES <- c("20", "40")
FO126STATES <- c("35", "48")

###WE USE LAND RENT AND TAKE A WEIGHTED AVERAGE
LandRentMerge <- LandRent %>%
  dplyr::filter(Year %in% c(2015:2019)) %>%
  dplyr::filter(!State == "HAWAII") %>%
  dplyr::mutate(Value = as.numeric(gsub(",", "", Value))) %>%
  group_by(State, State.ANSI) %>%
  dplyr::summarise(Value = mean(Value), na.rm = T) %>%
  ungroup() %>%
  dplyr::rename(STATEFP = "State.ANSI") %>%
  mutate(
    STATEFP = case_when(
      STATEFP < 10 ~ paste0("0", as.character(STATEFP)),
      T ~ as.character(STATEFP)
    )
  ) %>%
  mutate(
    Region = case_when(
      STATEFP %in% FO1STATES ~ "Northeast",
      STATEFP == "23" ~ "Northeast",
      STATEFP == "24" ~ "Northeast",
      STATEFP == "36" ~ "Northeast",
      STATEFP == "42" ~ "Northeast",
      STATEFP == "21" ~ "Appalachian",
      STATEFP %in% FO5STATES ~ "Appalachian",
      STATEFP == "47" ~ "Appalachian",
      STATEFP == "51" ~ "Appalachian",
      STATEFP == "12" ~ "Florida",
      STATEFP %in% FO7STATES ~ "Southeast",
      STATEFP == "13" ~ "Southeast",
      STATEFP == "27" ~ "Upper-Midwest",
      STATEFP == "38" ~ "Upper-Midwest",
      STATEFP == "55" ~ "Upper-Midwest",
      STATEFP == "08" ~ "Central",
      STATEFP == "17" ~ "Central",
      STATEFP == "19" ~ "Central",
      STATEFP %in% FO32STATES ~ "Central",
      STATEFP == "29" ~ "Central",
      STATEFP == "31" ~ "Central",
      STATEFP == "46" ~ "Central",
      STATEFP == "18" ~ "Mideast",
      STATEFP == "26" ~ "Mideast",
      STATEFP == "39" ~ "Mideast",
      STATEFP == "54" ~ "Mideast",
      STATEFP == "06" ~ "California",
      STATEFP == "41" ~ "Pacific-Northwest",
      STATEFP == "53" ~ "Pacific-Northwest",
      STATEFP %in% FO126STATES ~ "Southwest",
      STATEFP == "04" ~ "Arizona",
      T ~ "Unregulated",
    )
  ) %>%
  ungroup() %>%
  as.data.frame

LandRentRegion <- merge(LandRentMerge, LandAcresMerge, by = "STATEFP") %>%
  dplyr::select(STATEFP, State.x, Region, Value, Acres) %>%
  setNames(c("STATEFP", "STATE", "REGION", "RENT", "ACRES")) %>%
  group_by(REGION) %>%
  dplyr::summarise(LANDRENTREGION = sum(RENT * ACRES) / sum(ACRES)) %>%
  ungroup() %>%
  as.data.frame

write.table(
  LandRentRegion,
  file = here("GAMS CODE", "CSV DATA FILES", "Cropland_Rent.csv"),
  sep = ",",
  row.names = FALSE,
  col.names = FALSE
)
