


# load & clean data -------------------------------------------------------

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



# overall & yearly item rankings ------------------------------------------

## data ----
summary_1 <- oc_tidy %>%
  dplyr::select(year, item, value) %>%
  group_by(year, item) %>%
  summarize(yearly_count = sum(value)) %>%
  ungroup() %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-yearly_count),
         count_rel = yearly_count/yearly_count[rank==1],
         count_lbl = item) %>%
  group_by(item) %>% 
  filter(rank <=10) %>%
  ungroup()


## Top 10 frequency ----
summary_1 %>%
  group_by(item) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(item,(count)), y = count)) +
  geom_bar(stat = 'identity') +
  ylab("Years in Top 10 rank of OC data") +
  xlab("Plastic-inclusive items, OC data") +
  coord_flip() +
  theme_bw()

## Top 10 boxplot data ----
cats_top10 <- unique(summary_1$item)
boxplot1_data <- oc_tidy %>%
  dplyr::filter(item %in% cats_top10) %>%
  dplyr::select(year, item, value) %>%
  group_by(year, item) %>%
  summarize(yearly_count = sum(value)) %>%
  ungroup() %>%
  dplyr::mutate(item_f = factor(item, levels = c("straws_stirrers", "lids_plastic", "food_wrappers_candy_chips_etc", "cigarette_butts", "bottle_caps_plastic", "beverage_bottles_plastic", "other_plastic_bags", "grocery_bags_plastic", "take_out_away_containers_plastic", "construction_materials", "take_out_away_containers_foam", "cups_plates_plastic", "gloves_masks_ppe")))

## Top 10 boxplot ----
boxplot1_data %>%
  ggplot(aes(x = item_f, y = yearly_count)) +
  geom_boxplot(outlier.shape = NA, varwidth = TRUE) +
  scale_x_discrete(limits = rev(levels(boxplot1_data$item_f))) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  xlab(NULL) +
  ylab("Count, log10 scale") +
  scale_y_log10(labels = scales::comma) +
  annotation_logticks(sides = "bt") +
  coord_flip()

## ranked median of top 10 data ----
median_ranked <- boxplot1_data %>%
  group_by(item) %>%
  summarise(median_ranked = median(yearly_count),
            num_years = n()) %>%
  arrange(desc(median_ranked))



# normalized overall & yearly item rankings ------------------------------------

## prep normalized data ----
# rows which have 0 people
zero_people <- oc_tidy[oc_tidy$people == 0,]
zero_ids <- unique(zero_people$cleanup_id) # 7 cleanup efforts that have 0 people recorded.
# rows which have 0 count?
zero_count <- oc_tidy[oc_tidy$value == 0,] # none

## norm data ----
summary_norm <- oc_tidy %>%
  dplyr::filter(!cleanup_id %in% zero_ids) %>% ## remove cleanup ids which have 0 people observed
  dplyr::select(year, item, value, people) %>%
  mutate(norm_value = value / people) %>%
  group_by(year, item) %>%
  summarize(yearly_norm_count = round(sum(norm_value),2)) %>%
  ungroup() %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-yearly_norm_count),
         count_rel = yearly_norm_count/yearly_norm_count[rank==1],
         count_lbl = item) %>%
  group_by(item) %>% 
  filter(rank <=10) %>%
  ungroup()

## norm top 10 frequency ----
summary_norm %>%
  group_by(item) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(item,(count)), y = count)) +
  geom_bar(stat = 'identity') +
  ylab("Years in Top 10") +
  xlab("Plastic-inclusive items, OC data") +
  ggtitle("Normalized frequency of years in top 10 rank, 2016-2021") +
  coord_flip() +
  theme_bw()

## norm top 10 boxplot data ----
cats_top10_norm <- unique(summary_norm$item)
norm_boxplot1_data <- oc_tidy %>%
  dplyr::filter(item %in% cats_top10_norm) %>%
  dplyr::filter(!cleanup_id %in% zero_ids) %>%
  dplyr::select(year, item, value, people) %>%
  mutate(norm_value = value / people) %>%
  group_by(year, item) %>%
  summarize(yearly_norm_count = round(sum(norm_value),2)) %>%
  ungroup() %>%
  dplyr::mutate(item_f = factor(item, levels = c("straws_stirrers", "lids_plastic", "food_wrappers_candy_chips_etc", "cigarette_butts", "bottle_caps_plastic", "beverage_bottles_plastic", "forks_knives_spoons", "grocery_bags_plastic", "take_out_away_containers_plastic", "construction_materials", "take_out_away_containers_foam", "cups_plates_plastic", "gloves_masks_ppe")))


## norm top 10 boxplot ----
norm_boxplot1_data %>%
  ggplot(aes(x = item_f, y = yearly_norm_count)) +
  geom_boxplot(outlier.shape = NA, varwidth = TRUE) +
  scale_x_discrete(limits = rev(levels(norm_boxplot1_data$item_f))) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  xlab(NULL) +
  ylab("Count, normalized by no. people in cleanup") +
  ggtitle("Normalized count of top items, 2016-2021") +
  # scale_y_log10(labels = scales::comma) +
  # annotation_logticks(sides = "bt") +
  coord_flip(ylim = c(0, 20000))

## ranked median of top 10 norm data ----
norm_median_ranked <- norm_boxplot1_data %>%
  group_by(item) %>%
  summarise(median_norm = median(yearly_norm_count),
            num_years = n()) %>%
  arrange(desc(median_norm))


# county-level analysis ----

## clean county data ----
class(oc_tidy$zone)
levels(as.factor(oc_tidy$zone))
oc_tidy$zone[oc_tidy$zone == 'San Clemente, CA, USA'] <- 'Orange County, CA, USA'
oc_tidy$zone[oc_tidy$zone == 'Cayucos, CA 93430, USA'] <- 'San Luis Obispo County, CA, USA'
levels(as.factor(oc_tidy$zone))

oc_tidy$zone2 <- ifelse(oc_tidy$zone == "Del Norte County, CA, USA" |
                          oc_tidy$zone == "Humboldt County, CA, USA" |
                          oc_tidy$zone == "Mendocino County, CA, USA" |
                          oc_tidy$zone == "Sonoma County, CA, USA" |
                          oc_tidy$zone == "Marin County, CA, USA" |
                          oc_tidy$zone == "San Francisco County, CA, USA" |
                          oc_tidy$zone == "San Mateo County, CA, USA" |
                          oc_tidy$zone == "Santa Cruz County, CA, USA" |
                          oc_tidy$zone == "Monterey County, CA, USA" |
                          oc_tidy$zone == "San Luis Obispo County, CA, USA" |
                          oc_tidy$zone == "Santa Barbara County, CA, USA" |
                          oc_tidy$zone == "Ventura County, CA, USA" |
                          oc_tidy$zone == "Los Angeles County, CA, USA" |
                          oc_tidy$zone == "Orange County, CA, USA" |
                          oc_tidy$zone == "San Diego County, CA, USA",
                        "coast", "inland")

##   
