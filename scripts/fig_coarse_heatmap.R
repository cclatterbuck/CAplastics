#------------------------------
# Title: CCD coarse categories
# Date: Mon Feb  6 10:32:02 2023
# Author: Corey Clatterbuck
#------------------------------

# load data & libraries ----
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(janitor)

ccd_raw <- read_excel(here("data", "raw", "Coastalcleanupday_data.xlsx"))
oc_raw <- read_excel(here("data", "raw", "OceanConservancy_CA.xlsx"), 
                     col_types = c("numeric", "text", "text", "text", "text", 
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
coarse_raw <- read_excel(here("data", "raw", "coarse_data_relational_table.xlsx"))



## reduce oc data to ccd dates ----
ccd_dates <- c("2016-09-17", "2017-09-16", "2018-09-15", "2019-09-21", "2021-09-19")
ccd_dates <- as.Date(ccd_dates)
oc_use <- oc_raw %>%
  mutate(`Cleanup Date` = as.Date(`Cleanup Date`)) %>%
  dplyr::filter(`Cleanup Date` %in% ccd_dates | 
                  (`Cleanup Date` >= as.Date('2020-09-01') & `Cleanup Date` <= as.Date('2020-09-30')))
levels(as.factor(oc_use$`Cleanup Date`)) ## double check

## clean outliers from oc data ----
# see explore_outliers_OCdata.qmd for details
## remove unidentifiable pieces
oc_use <- oc_use |>
  dplyr::rename("Fishing Nets" = "Fishing Net & Pieces") |>
  dplyr::select(-ends_with(c("(Clean Swell)", "Pieces", "Collected"))) |>
  dplyr::rename("Fishing Net & Pieces" = "Fishing Nets") |>
  dplyr::filter(!if_all(15:57, is.na))  ## removes ~1600 cleanups

## remove collected item:people ratio less than one, as well as 0s. Remove cols use to calculate these, if desired 
oc_use <- oc_use |>
  dplyr::mutate(total_collected = rowSums(across(15:57), na.rm = TRUE),
                ratio_collected = round(total_collected/People, 4)) |>
  relocate(ratio_collected, .after = People) |>
  dplyr::filter(ratio_collected > 0.9999) |>
  dplyr::filter(is.finite(ratio_collected)) |>
  dplyr::select(-total_collected, -ratio_collected)

# wrangle CCD_raw data to long ----
## subset relational table for unique ccd plastic items only ----
coarse_ccd <- coarse_raw |>
  dplyr::select(ccd_coarse_name, used_coarse_name) |>
  distinct() |>
  dplyr::filter(!is.na(ccd_coarse_name))

## make ccd data long & join coarse names ----
ccd_edit <- ccd_raw %>%
  dplyr::select(1,3:30) %>% ## remove 2016 and 2017, use oc data instead
  dplyr::filter(Item != "total") %>%
  remove_empty(which = "rows") %>%
  pivot_longer(!Item, names_to = "Year", values_to = "Count") %>%
  left_join(coarse_ccd, by = c("Item" = "ccd_coarse_name"))

## examine & remove Item without a used_coarse_name ----
## NOTE: this currently assumes Take Out/Away Containers are included in Food Wrappers/Containers; see Issue 4 in repo
ccd_edit |> dplyr::filter(is.na(used_coarse_name)) |> distinct(Item) |> print(n=25)
ccd_edit <- ccd_edit |> 
  dplyr::filter(!is.na(used_coarse_name))

## combine 2 ccd categories ----
### sum Oil/Lube Bottles & Bleach/Cleaner Bottles/Other Plastic Bottles
sum_ccd_bottles <- ccd_edit |>
  dplyr::filter(used_coarse_name == "other plastic bottles") |>
  group_by(Year) |>
  summarise(Count = sum(Count, na.rm=TRUE)) |>
  dplyr::mutate(Item = NA,
                used_coarse_name = "other plastic bottles") |>
  dplyr::relocate(Item, Year, Count, used_coarse_name)

### remove Oil/Lube Bottles & Bleach/Cleaner Bottles/Other Plastic Bottles and row_bind sum_ccd_bottles
ccd_edit <- ccd_edit |>
  dplyr::filter(used_coarse_name != "other plastic bottles") |>
  bind_rows(sum_ccd_bottles)


# wrangle OC_use data to long ----
## subset relational table for unique oc plastic items only ----
coarse_oc <- coarse_raw |>
  dplyr::select(oc_name, used_coarse_name) |>
  distinct() |>
  dplyr::filter(!is.na(oc_name))

## make oc data long & join coarse names ----
## NOTE: this does include take out/away containers as a separate category
colnames(oc_use)
oc_long <- oc_use %>%
  dplyr::select(1,2,7,9,11,15:57) %>% ## keep columns to help calculate effort later
  remove_empty(which = "rows") %>%
  pivot_longer(!c("Cleanup ID", "Zone", "Adults", "People", "Cleanup Date"), names_to = "Item", values_to = "Count") %>%
  left_join(coarse_oc, by = c("Item" = "oc_name"))

## examine & remove Item without a used_coarse_name ----
oc_long |> dplyr::filter(is.na(used_coarse_name)) |> distinct(Item) |> print(n=25)
oc_long <- oc_long |> 
  dplyr::filter(!is.na(used_coarse_name))

## create oc_edit to mimic format of ccd_edit ----
colnames(ccd_edit)
colnames(oc_long)
oc_edit <- oc_long |>
  dplyr::filter(!is.na(Count)) |> 
  mutate(Year = year(`Cleanup Date`)) |>
  dplyr::select(Year, Count, used_coarse_name) |>
  group_by(Year, used_coarse_name) |>
  summarize(Count = sum(Count, na.rm=TRUE)) |>
  ungroup() |>
  dplyr::filter(Year > 2015) ## replace ccd 2016 and 2017 with oc 2016 and 2017. 


# combine _edit dfs & rank ----
## create df with all coarse categories and years ----
na_df <- coarse_raw |>
  dplyr::select(used_coarse_name) |>
  distinct() |>
  slice(rep(1:n(), each = 34)) |> ## seq.int(1988:2021) = 34 years
  dplyr::mutate(Year = rep(1988:2021, times = 43)) ## 43 used_coarse_names

## bind data sets, join with blank df, create ranks
fig_df <- ccd_edit |>
  dplyr::select(!Item) |>
  mutate(Year = as.numeric(Year)) |> ## aid bind_rows
  bind_rows(oc_edit) |>
  right_join(na_df, by = c("Year", "used_coarse_name")) |>
  group_by(Year) |>
  mutate(Rank = rank(desc(Count), na.last = "keep", ties.method = "average")) |> ## not certain what ties.method to use
  ungroup() |>
  arrange(desc(Year), Rank)


# plot ----
## get scale_y_discrete order
axis_order <- fig_df |>
  group_by(used_coarse_name) |>
  summarize(mean_rank = mean(Rank, na.rm=TRUE),
            max_count = sum(Count, na.rm=TRUE)) |>
  arrange(desc(max_count))

## heatmap plot
ggplot(fig_df, aes(x = Year, y = used_coarse_name)) +
  geom_tile(aes(fill = Rank), colour = "white") +
  scale_fill_continuous(na.value = 'white') +
  scale_y_discrete(limits = rev(axis_order$used_coarse_name)) +
  labs(x = "Year", y = "Collected item category") +
  coord_fixed() +
  theme_bw() +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 15,
                                title = "item rank\nwithin year",
                                reverse = TRUE))



## combine split categories into coarse categories for figures
## this will include: food wrappers & take out/away containers; all bottle caps and lids; all cutlery; all bags
## decide on new column titles; add to data dictionary
## eliminate clean swell items


# old code ----
# # determine coarse categories from early data 
# ## subset explicitly non-plastic items
# rm_nonp <- c("Bags PAPER", "Beverage Bottles (Glass)", "Beverage Cans", "Bottle Caps (Metal)", "Cups & Plates (Paper)", "Foam Pieces (less than 2.5 cm)", "Glass Pieces (less than 2.5 cm)", "Light Bulbs/Tubes", "Plastic Pieces (less than 2.5 cm)")
# 
# ## subset items counted twice (e.g., grocery bags and other plastic bags).
# ## "Cups, Plates, Forks, Knives, Spoons TOTAL" is not just plastic and foam items unlike the plastic bags
# ## remove "Buoys/Floats" and "Crab/Lobster/Fish Traps" as there is a total row for these that is consistent thruout dataset
# rm_extra <- c("Bags PLASTIC - GROCERY", "Bags PLASTIC - OTHER", "Buoys/Floats", "Crab/Lobster/Fish Traps")
# 
# early_items <- ccd_raw %>%
#   dplyr::filter(!Item %in% rm_nonp) %>% ## remove non-plastic items
#   dplyr::filter(!Item %in% rm_extra) %>% ## remove items counted twice
#   dplyr::filter(Item != "total") ## remove total row
# 
# 
# 
# 
# # determine categories from late data
# colnames(oc_use)
# 
# ## combine split categories into coarse categories for figures
# ## this will include: food wrappers & take out/away containers; all bottle caps and lids; all cutlery; all bags
# ## decide on new column titles; add to data dictionary
# ## eliminate clean swell items
# 
# 
# ## subset explicitly non-plastic items
# rm_nonp_oc <- c("Bottle Caps (Metal)", "Beverage Bottles (Glass)", "Beverage Cans", "Paper Bags", "Cups, Plates (Paper)", "Glass Pieces", "Plastic Pieces")
# 
# ## subset items counted twice (e.g., grocery bags and other plastic bags).
# ## "Cups, Plates, Forks, Knives, Spoons TOTAL" is not just plastic and foam items unlike the plastic bags
# ## remove "Buoys/Floats" and "Crab/Lobster/Fish Traps" as there is a total row for these that is consistent thruout dataset
# rm_extra_oc <- c("Bags PLASTIC - GROCERY", "Bags PLASTIC - OTHER", "Buoys/Floats", "Crab/Lobster/Fish Traps")
