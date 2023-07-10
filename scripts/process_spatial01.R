#------------------------------
# Title: process_spatial01
# Date: Thu Jun 29 09:06:03 2023
# Author: Corey Clatterbuck
#------------------------------

# About ----

# 1. load data & libraries ----

library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(janitor)
library(car)

oc_tidy <- read_csv(here("data", "processed", "OC_tidy.csv"), 
                    col_types = c("n", "c", "c", "c", "c",
                                  "c", "D", "c", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n",
                                  "n", "n", "n", "n", "n"))

coarse_raw <- read_excel(here("data", "processed", "coarse_data_relational_table.xlsx"))

# 2. view & remove county summaries ----
## identify county-based summaries
county <- oc_tidy |>
  dplyr::filter(grepl('County',group_name) | grepl('county',group_name)) ##  visual exam for removal
county_ids1 <- c(12833, 27247)

co <- oc_tidy |>
  dplyr::filter(grepl(' CO ', group_name) |
                  grepl(' Co ', group_name) |
                  grepl(' co ', group_name) |
                  grepl(' CO. ', group_name) |
                  grepl(' Co. ', group_name) |
                  grepl(' co. ', group_name)) ## 2 obs to be removed
county_ids2 <- pull(co, cleanup_id)

difference <- oc_tidy |>
  dplyr::filter(grepl('difference', group_name) |
                  grepl('Difference', group_name) |
                  grepl('differences', group_name) |
                  grepl('Differences', group_name)) ## 2 obs, same as co
icc <- oc_tidy |>
  dplyr::filter(grepl('ICC', group_name)) ## think these are okay

## remove county-based summaries, only 4 found
county_ids <- c(county_ids1, county_ids2)
spdata <- oc_tidy |>
  dplyr::filter(!cleanup_id %in% county_ids)

# 3. create coarse categories ----
## subset relational table for unique oc plastic items only ----
coarse_oc <- coarse_raw |>
  dplyr::select(oc_name, used_coarse_name) |>
  distinct() |>
  dplyr::filter(!is.na(oc_name)) |>
  mutate(oc_name = make_clean_names(oc_name))

## make oc data long & join coarse names ----
## NOTE: this does include take out/away containers as a separate category
colnames(spdata)
oc_long <- spdata %>%
  dplyr::select(1,2,5,7,9,11,14:60) %>% ## keep columns to help calculate effort later
  remove_empty(which = "rows") %>%
  pivot_longer(!c("cleanup_id", "zone", "gps", "adults", "people", "cleanup_date"), names_to = "item", values_to = "count") %>%
  left_join(coarse_oc, by = c("item" = "oc_name"))

## examine & remove Item without a used_coarse_name ----
oc_long |> dplyr::filter(is.na(used_coarse_name)) |> distinct(item) |> print(n=25)
oc_long <- oc_long |> 
  dplyr::filter(!is.na(used_coarse_name))

##add counts within same used_coarse_name and cleanup_id ----
try <- oc_long |>
  dplyr::filter(!is.na(count)) |>
  dplyr::select(cleanup_id, count, used_coarse_name) |>
  group_by(cleanup_id, used_coarse_name) |>
  summarize(COUNT = sum(count, na.rm=TRUE)) |>
  ungroup() |>
  pivot_wider(names_from = used_coarse_name, values_from = COUNT)

## join with other identifying info & pivot_wider (each row a cleanup; tidy format) ----
spdata_save <- spdata |>
  dplyr::select(-c(14:60)) |>
  left_join(try, by = "cleanup_id")

## which cleanups in spdata are not in try?
oop <- spdata |>
  dplyr::filter(!cleanup_id %in% try$cleanup_id)
## the missing cleanups only have categories that are not included in the coarse data.

## remove cleanups without coarse categories ----
colnames(spdata_save)
spdata_save <- spdata_save |>
  dplyr::filter(!if_all(14:46, is.na))

# 4. save as spatialdata_01_main.csv ----
write_csv(spdata_save, here("data", "processed" ,"spatialdata_01_main.csv"))
