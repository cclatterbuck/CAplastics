#------------------------------
# Title: process_oc.R
# Date: Tue Jun 27 08:12:51 2023
# Author: Corey Clatterbuck
#------------------------------

# This script combines the two datasets provided by the Ocean Conservancy and
# and filters out cleanups based on a number of criteria. See the data decisions
# flowchart in the figures folder for context.

# 1. load data & libraries ----

library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(janitor)
library(car)

oc_raw1 <- read_excel(here("data", "raw", "OceanConservancy_CA.xlsx"), 
                     col_types = c("numeric", "text", "text", "text", "text", 
                                   "text", "date", "text", "numeric", "numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric"
                                   )) |>
  clean_names()

oc_raw2 <- read_excel(here("data", "raw", "OC_CA_2021_2022.xlsx"), 
                      col_types = c("numeric", "text", "text", "text", "text",
                                    "text", "text", "date", "text", "text",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric"
                                    )) |>
  clean_names()

# 2. remove 2021 from oc_raw1 ----
oc_proc1 <- filter(oc_raw1, cleanup_date < "2021-01-01") ## obs = 18357

# 3. clean datasets to bind_rows ----
setdiff(colnames(oc_raw1), colnames(oc_raw2)) ## in oc_raw1 but not oc_raw2
setdiff(colnames(oc_raw2), colnames(oc_raw1)) ## in oc_raw2 but not oc_raw1

## remove nonmatching cols ----
oc_proc2 <- dplyr::select(oc_raw2, -environment, -email, -distance_mi, -area_mi,
                          -lines_nets_traps_ropes_etc, -other_plastic_waste,
                          -tobacco_products_lighters_cigar_tips_wrap,
                          -other_waste_metal_paper_etc, -plastic_foam_pieces)
oc_proc1 <- dplyr::select(oc_proc1, -miles)

## rename matching cols ----
oc_proc2 <- oc_proc2 |>
  rename(other_plastic_bags = other_bags_plastic,
         beverages_sachets = beverage_sachets_pouches,
         take_out_away_containers_foam = food_containers_foam,
         take_out_away_containers_plastic = food_containers_plastic,
         straws_stirrers = straws_stirrers_plastic,
         forks_knives_spoons = utensils_plastic,
         tampons_tampon_applicators = tampons_applicators)

## combine split cols ----
oc_proc2$clothing_shoes <- rowSums(oc_proc2[,c("clothing", "footwear_shoes_slippers")], 
                                   na.rm=TRUE)
oc_proc2$package_place <- rowSums(oc_proc2[,c("foam_packaging", "other_plastic_foam_packaging")], 
                                   na.rm=TRUE)
oc_proc2 <- dplyr::select(oc_proc2, -clothing, -footwear_shoes_slippers,
                          -foam_packaging, -other_plastic_foam_packaging)
oc_proc2 <- rename(oc_proc2, other_plastic_foam_packaging = package_place)

## check & bind_rows ----
setdiff(colnames(oc_proc1), colnames(oc_proc2)) ## in oc_raw1 but not oc_raw2
setdiff(colnames(oc_proc2), colnames(oc_proc1)) ## in oc_raw2 but not oc_raw1
## retain the 2 new cols in oc_proc2, but move to end of dataset or remove & add back if necessary

oc_bind <- bind_rows(oc_proc1, oc_proc2) #23351 obs

# 4. filter datasets ----

## reduce dates ----
ccd_dates <- c("2016-09-17", "2017-09-16", "2018-09-15", "2019-09-21", "2021-09-18", "2022-09-17")
ccd_dates <- as.Date(ccd_dates)
oc_filter <- oc_bind %>%
  mutate(cleanup_date = as.Date(cleanup_date)) %>%
  dplyr::filter(cleanup_date %in% ccd_dates | 
                  (cleanup_date >= as.Date('2020-09-01') &
                     cleanup_date <= as.Date('2020-09-30')))
levels(as.factor(oc_filter$cleanup_date)) ## double check; 11021 obs

## find & remove duplicates ----
oc_filter <- oc_filter |>
  distinct() ## all rows distinct
dupe_rows <- oc_filter |> 
  get_dupes(cleanup_id)
dupe_ids <- unique(dupe_rows$cleanup_id)
oc_filter <- oc_filter |>
  dplyr::filter(!cleanup_id %in% dupe_ids) ## remove none

## remove unidentifiable pieces ----
oc_filter <- oc_filter |>
  dplyr::rename("Fishing Nets" = "fishing_net_pieces",
                "Foam Dock" = "foam_dock_pieces") |>
  dplyr::select(-ends_with(c("clean_swell", "pieces", "collected"))) |>
  dplyr::rename("fishing_net_pieces" = "Fishing Nets",
                "foam_dock_pieces" = "Foam Dock") |>
  dplyr::filter(!if_all(14:60, is.na))  ## removes 1569 cleanups

## remove collected item:people ratio <1 | 0 ---- 
## Remove cols use to calculate these, if desired 
oc_filter <- oc_filter |>
  dplyr::mutate(total_collected = rowSums(across(14:60), na.rm = TRUE),
                ratio_collected = round(total_collected/people, 4)) |>
  relocate(ratio_collected, .after = people) |>
  dplyr::filter(ratio_collected > 0.9999) |>
  dplyr::filter(is.finite(ratio_collected)) |>
  dplyr::select(-total_collected, -ratio_collected) ## removes 1,016 cleanups

## 5. save OC_tidy ----
write_csv(oc_filter, here("data", "processed" ,"OC_tidy.csv"))

## 6. view? ----
oc_filter |>
  dplyr::mutate(Year = year(cleanup_date)) |> ## fails?
  ggplot(aes(x = Year)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  scale_y_continuous(limits = c(0,3000)) +
  xlab("Year") +
  ylab("No. of cleanup efforts") +
  ggtitle("No. of Cleanup Efforts, 2016-2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
