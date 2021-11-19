
library(tidyverse)
library(readxl)
library(here)
library(gganimate)
library(cowplot)

oc_raw <- read_excel(here("data", "OceanConservancy_CA.xlsx"))
colnames(oc_raw)
str(oc_raw)
head(oc_raw)

## keep only Coastal Cleanup Day dates? Sep 2020 was "coastal cleanup month"
ccd_dates <- c("2016-09-17", "2017-09-16", "2018-09-15", "2019-09-21", "2021-09-19")
ccd_dates <- as.Date(ccd_dates)
oc_use <- oc_raw %>%
  mutate(`Cleanup Date` = as.Date(`Cleanup Date`)) %>%
  dplyr::filter(`Cleanup Date` %in% ccd_dates | 
                  (`Cleanup Date` >= as.Date('2020-09-01') & `Cleanup Date` <= as.Date('2020-09-30')))


## remove explicitly non-plastic items and items that cannot be identified
rm_nonp <- c("Bottle Caps (Metal)", "Beverage Bottles (Glass)", "Beverage Cans", "Paper Bags", "Cups, Plates (Paper)", "Other Plastic/Foam Packaging", "Other Packaging (Clean Swell)", "Other Trash (Clean Swell)", "Personal Hygiene (Clean Swell)", "Foam Pieces", "Glass Pieces", "Plastic Pieces")
oc_use <- oc_use %>%
  select(-any_of(rm_nonp))


## create separate metadata table and tidy table