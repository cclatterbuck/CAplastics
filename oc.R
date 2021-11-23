
library(tidyverse)
library(readxl)
library(here)
library(gganimate)
library(cowplot)
library(janitor)

oc_raw <- read_excel(here("data", "OceanConservancy_CA.xlsx"), 
                     col_types = c("numeric", "text", "text", "text", "numeric", 
                                   "text", "date", "text", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric"))

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
levels(as.factor(oc_use$`Cleanup Date`)) ## double check


## remove explicitly non-plastic items and items that cannot be identified
rm_nonp <- c("Bottle Caps (Metal)", "Beverage Bottles (Glass)", "Beverage Cans", "Paper Bags", "Cups, Plates (Paper)", "Other Plastic/Foam Packaging", "Other Packaging (Clean Swell)", "Other Trash (Clean Swell)", "Personal Hygiene (Clean Swell)", "Foam Pieces", "Glass Pieces", "Plastic Pieces")
oc_use <- oc_use %>%
  select(-any_of(rm_nonp))


## continue cleaning, exploring & filtering dataset
oc_use <- janitor::clean_names(oc_use)
colnames(oc_use)
str(oc_use)
n_distinct(oc_use$cleanup_id) ## should equal nobs in oc_use. Cleanup ID can be used as an index column


## create separate metadata table and tidy table, which removes rows with values = 0
oc_meta <- oc_use %>%
  dplyr::select(cleanup_id:number_of_bags)

oc_tidy <- oc_use %>%
  dplyr::select(cleanup_id, zone, cleanup_date, people, miles, cigarette_butts:gloves_masks_ppe) %>%
  pivot_longer(cols = cigarette_butts:gloves_masks_ppe, names_to = "item", values_to = "value") %>%
  drop_na(value)
oc_tidy$year <- as.numeric(format(oc_tidy$cleanup_date, "%Y"))

## double-check: did this remove rows where no specific items were reported as Sarah K warned on Nov 15?
zero_rows <- oc_use %>%
  filter_at(vars(15:52), any_vars(! is.na(.)))
n_distinct(zero_rows$cleanup_id)
n_distinct(oc_tidy$cleanup_id) ## if equal, then yes. 


## get overall & yearly ranks of items

## get overall & yearly ranks of items normalized by # of people per effort & compare to above

## county-level analysis


  
