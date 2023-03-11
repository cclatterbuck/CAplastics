#------------------------------
# Title: fig_coarse_boxplots
# Date: Fri Mar 10 11:10:58 2023
# Author: Corey Clatterbuck
#------------------------------

# load data & libraries ----
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(janitor)

ccd_raw <- read_excel(here("data", "Coastalcleanupday_data.xlsx"))
oc_raw <- read_excel(here("data", "OceanConservancy_CA.xlsx"), 
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
coarse_raw <- read_excel(here("data", "coarse_data_relational_table.xlsx"))



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
  dplyr::select(1,3:30) %>% ## removes 2016 and 2017 data; will use OC data instead
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
  mutate(Year = year(`Cleanup Date`)) |>
  dplyr::select(Year, Count, used_coarse_name) |>
  group_by(Year, used_coarse_name) |>
  summarize(Count = sum(Count, na.rm=TRUE)) |>
  ungroup()
# dplyr::filter(Year > 2017) ## ccd already has data through 2017; could replace ccd 2016 and 2017 with oc 2016 and 2017. 



# counts: combine _edit dfs & rank ----
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

## plot ----
## get scale_y_discrete order
axis_order <- fig_df |>
  group_by(used_coarse_name) |>
  summarize(mean_rank = mean(Rank, na.rm=TRUE),
            max_count = sum(Count, na.rm=TRUE)) |>
  arrange(desc(max_count)) |>
  slice_head(n = 20)

fig_a <- fig_df |>
  dplyr::filter(used_coarse_name %in% axis_order$used_coarse_name) |>
  dplyr::filter(Year < 2021) |> ## 2021 data is incomplete
  ggplot(aes(x = used_coarse_name, y = Count)) +
  geom_boxplot() +
  scale_y_log10( breaks = scales::trans_breaks("log10", function(x) 10^x),
                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "b") +
  scale_x_discrete(limits = rev(axis_order$used_coarse_name)) +
  coord_flip() +
  annotate("label", x = 20, y = 110, label = "A", fill = "black", color = "gray80", size = 5) +
  labs(x = "Collected item category", y = "Annual count, 1988-2020") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
  

# normalized data: combine _edit dfs & rank ----
## create df with coarse categories and 2016-2021 years ----
na_df_n <- coarse_raw |>
  dplyr::select(used_coarse_name) |>
  distinct() |>
  slice(rep(1:n(), each = 6)) |> ## seq.int(2016:2021) = 6 years
  dplyr::mutate(Year = rep(2016:2021, times = 43)) ## 43 used_coarse_names

## normalize data, join with blank df, create ranks ----
colnames(oc_long)
fig_df_n <- oc_long |>
  mutate(Year = year(`Cleanup Date`),
         n_count = Count/People) |>
  dplyr::select(Year, n_count, used_coarse_name) |>
  group_by(Year, used_coarse_name) |>
  summarize(n_count = sum(n_count, na.rm=TRUE)) |>
  ungroup() |>
  right_join(na_df_n, by = c("Year", "used_coarse_name")) |>
  group_by(Year) |>
  mutate(Rank = rank(desc(n_count), na.last = "keep", ties.method = "average")) |>
  ungroup() |>
  arrange(desc(Year), Rank)

## plot ----
fig_b <- fig_df_n |>
  dplyr::filter(used_coarse_name %in% axis_order$used_coarse_name) |>
  dplyr::filter(Year < 2021) |> ## 2021 data is incomplete
  ggplot(aes(x = used_coarse_name, y = n_count)) +
  geom_boxplot() +
  scale_y_log10( breaks = scales::trans_breaks("log10", function(x) 10^x),
                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "b") +
  scale_x_discrete(limits = rev(axis_order$used_coarse_name), position = "top") +
  coord_flip() +
  annotate("label", x = 20, y = 110, label = "B", fill = "black", color = "gray80", size = 5) +
  labs(x = "Collected item category", y = "Normalized annual count, 2016-2020") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


# combine plots ----
# use cowplot to combine the plots from counts and normalized data
library(cowplot)

## plot.grid ----
title <- ggdraw() + 
  draw_label("Similar trends in the abundance of collected items",
             size = 18, x = 0, hjust = 0) +
  draw_label("Distribution of annual counts based on counts alone (A) and normalized by number of cleanup volunteers (B)",
             size = 10, y = 0.05, x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))
grid1 <- plot_grid(fig_a, fig_b, label_size = 12)
caption <- ggdraw() +
  draw_label("Collected items in B without a boxplot indicate lack of data for these categories in 2016-2020.",
             size = 8, x = 0, hjust = 0, colour = "gray40") +
  theme(plot.margin = margin(5, 0, 0, 0))

plot_grid(title, grid1, caption, ncol = 1,
          rel_heights = c(0.1, 1, 0.05))

## same grid, diff colors ----



