#------------------------------
# Title: fig_ccd_bump_ranks
# Date: Mon Jan 23 11:45:18 2023
# Author: Corey Clatterbuck
#------------------------------

## Purpose is to create a bump chart of the ranks of coarse plastic types over time from the CCD & OC datasets.
## Helpful walkthrough here: https://www.r-bloggers.com/2018/04/bump-chart/

# Load data & libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(here)
library(cowplot)
library(janitor)

ccd_raw <- read_excel(here("data", "Coastalcleanupday_data.xlsx"))

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

# Data wrangling ----

## OC data first: select CCD dates
head(oc_raw)

ccd_dates <- c("2016-09-17", "2017-09-16", "2018-09-15", "2019-09-21", "2021-09-19")
ccd_dates <- as.Date(ccd_dates)
oc_wrangle <- oc_raw %>%
  mutate(`Cleanup Date` = as.Date(`Cleanup Date`)) %>%
  dplyr::filter(`Cleanup Date` %in% ccd_dates | 
                  (`Cleanup Date` >= as.Date('2020-09-01') & `Cleanup Date` <= as.Date('2020-09-30')))
levels(as.factor(oc_wrangle$`Cleanup Date`)) ## double check

## OC: combine columns to create coarse plastics totals similar to older CCD data
colnames(oc_wrangle)
oc_wrangle <- oc_wrangle %>%
  rowwise() %>%
  dplyr::mutate("Food Wrappers/Containers" = sum(c_across("Food Wrappers (candy, chips, etc.)":"Take Out/Away Containers (Foam)"), na.rm=TRUE),
                "Caps, Lids TOTAL" = sum(c_across("Bottle Caps (Plastic)":"Lids (Plastic)"), na.rm=TRUE),
                "Cups, Plates, Forks, Knives, Spoons TOTAL" = sum(c_across(c(23,30:32)), na.rm=TRUE),
                "Bags TOTAL" = sum(c_across(27:29), na.rm=TRUE)) %>%
  rename("Cigarettes/Cigarette Filters" = "Cigarette Butts",
         "Beverage Bottles (Plastic) 2 liters or less" = "Beverage Bottles (Plastic)",
         "Rope" = "Rope (1 yard/meter = 1 piece)",
         "Fishing Line" = "Fishing Line (1 yard/meter = 1 piece)",
         "Fishing Nets" = "Fishing Net & Pieces",
         "Six-Pack Holders" = "6-Pack Holders",
         "Bleach/Cleaner Bottles/Other Plastic Bottles" = "Other Plastic Bottles (oil, bleach, etc.)",
         "Building/Construction Materials" = "Construction Materials")

## OC: tidy & summarize plastic counts by year; pivot to match CCD data
oc_wrangle <- dplyr::select(oc_wrangle, c(7,15,22,24,33:61,66:69)) %>% ## select columns of interest
  dplyr::select(!ends_with("(Clean Swell)"))## remove clean swell columns
oc_wrangle$`Cleanup Date` <- year(oc_wrangle$`Cleanup Date`) ## make date year only
oc_wrangle <- oc_wrangle %>%
  group_by(`Cleanup Date`) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'keep')

oc_tidy <- oc_wrangle %>%
  pivot_longer(!`Cleanup Date`, names_to = "Item", values_to = "value") %>%
  rename("Year" = `Cleanup Date`)

oc_formatted <- oc_tidy %>%
  group_by(Year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = Item) %>%
  group_by(Item) %>% 
  filter(rank <=15) %>%
  ungroup()
                


# Basic bump chart ----
show.top.n <- 10

ggplot(data = oc_formatted, aes(x = Year, y = rank, group = Item)) +
  geom_line(aes(color = Item, alpha = 1), size = 2) +
  geom_point(aes(color = Item, alpha = 1), size = 4) +
  scale_y_reverse(breaks = show.top.n:1) +
  coord_cartesian(ylim = c(show.top.n,1))

# Styled bump chart ----
my_theme <- function() {
  
  # Colors
  color.background = "white"
    color.text = "#22211d"
      
    # Begin construction of chart
    theme_bw(base_size=15) +
      
      # Format background colors
      theme(panel.background = element_rect(fill=color.background, color=color.background)) +
      theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
      theme(panel.border     = element_rect(color=color.background)) +
      theme(strip.background = element_rect(fill=color.background, color=color.background)) +
      
      # Format the grid
      theme(panel.grid.major.y = element_blank()) +
      theme(panel.grid.minor.y = element_blank()) +
      theme(axis.ticks       = element_blank()) +
      
      # Format the legend
      theme(legend.position = "none") +
      
      # Format title and axis labels
      theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
      theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
      theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
      theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
      theme(axis.text.y      = element_text(size=10, color = color.text)) +
      theme(strip.text       = element_text(face = "bold")) +
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

show.top.n <- 10

ggplot(data = oc_formatted, aes(x = Year, y = rank, group = Item)) +
  geom_line(aes(color = Item, alpha = 1), size = 2) +
  geom_point(aes(color = Item, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 2016:2021, minor_breaks = 2016:2021, expand = c(.05, .05)) +
  geom_text(data = oc_formatted %>% filter(Year == "2016"),
            aes(label = Item, x = 2016.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = oc_formatted %>% filter(Year == "2021"),
            aes(label = Item, x = 2021.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(show.top.n,1)) + 
  theme(legend.position = "none") +
  labs(x = "Year",
       y = "Rank",
       title = "Plastics collected on Coastal Cleanup Day, California, USA",
       subtitle = "Plastics ranked by count") +
  my_theme() 

# Highlighted bump chart ----