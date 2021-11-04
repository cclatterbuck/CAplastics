
library(tidyverse)
library(readxl)
library(here)
library(gganimate)
library(cowplot)

ccd_raw <- read_excel(here("data", "Coastalcleanupday_data.xlsx"))

ccd_raw <- ccd_raw %>%
  dplyr::select(1:34) %>%
  dplyr::slice(1:61)
colnames(ccd_raw)

## remove explicitly non-plastic items
rm_nonp <- c("Bags PAPER", "Beverage Bottles (Glass)", "Beverage Cans", "Bottle Caps (Metal)", "Cups & Plates (Paper)", "Foam Pieces (less than 2.5 cm)", "Glass Pieces (less than 2.5 cm)", "Light Bulbs/Tubes", "Plastic Pieces (less than 2.5 cm)")

## remove items counted twice (e.g., grocery bags and other plastic bags).
## can look at breakdown of plastic items within these larger categories (e.g., plastic bags) later.
## "Cups, Plates, Forks, Knives, Spoons TOTAL" is not just plastic and foam items unlike the plastic bags
## remove "Buoys/Floats" and "Crab/Lobster/Fish Traps" as there is a total row for these that is consistent thruout dataset
rm_extra <- c("Bags PLASTIC - GROCERY", "Bags PLASTIC - OTHER", "Buoys/Floats", "Crab/Lobster/Fish Traps")

## remove explicitly non-plastic categories & unidentifiable pieces
ccd_use <- ccd_raw %>%
  filter(!Item %in% rm_nonp,
         !Item %in% rm_extra)

## make tidy
ccd_tidy <- ccd_use %>%
  dplyr::select(-Source, -TOTALS, -"%") %>%
  pivot_longer(!Item, names_to = "Year", values_to = "value")

## rank
ccd_formatted <- ccd_tidy %>%
  group_by(Year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = Item) %>%
  group_by(Item) %>% 
  filter(rank <=10) %>%
  ungroup()

## make silly color vector for histogram
items <- levels(as.factor(ccd_formatted$Item))
textcolors <- c("darkmagenta", "brown2", "black", "black", "darkmagenta",
                "black", "brown2", "black", "black", "black",
                "black", "brown2", "black", "black", "black",
                "black", "black", "black", "black")
textcolors.df <- data.frame(items, textcolors)
colnames(textcolors.df) <- c("Item", "textcolors")
ccd_formatted <- left_join(ccd_formatted, textcolors.df, by = "Item")


## plot histogram
# png(here("figures", "Fig1.png"), units="in", width=6, height=4, res=300)
ccd_formatted %>%
  group_by(Item) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(Item,(count)), y = count)) +
  geom_bar(stat = 'identity') +
  ylab("Years in Top 10 rank of CCD data") +
  xlab("Plastic-inclusive items, CCD data") +
  coord_flip() +
  theme_bw() 
# dev.off()

# png(here("figures", "Fig2.png"), units="in", width=6, height=4, res=300)
ccd_formatted %>%
  group_by(Item) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(Item,(count)), y = count, fill = textcolors)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("black", "brown2", "darkmagenta")) +
  ylab("Years in Top 10 rank of CCD data") +
  xlab("Plastic-inclusive items, CCD data") +
  # theme(axis.text.x = element_text(colour = textcolors)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
# dev.off()



### plot boxplots
## match factor order in histograms
Item_f <- factor(ccd_formatted$Item, levels = c("Food Wrappers/Containers", "Cups, Plates, Forks, Knives, Spoons TOTAL", "Caps, Lids TOTAL", "Beverage Bottles (Plastic) 2 liters or less", "Bags TOTAL", "Straws, Stirrers", "Cigarettes/Cigarette Filters", "Building/Construction Materials", "Clothing, Shoes", "Cigar Tips", "Bags PLASTIC", "Balloons", "Other Plastic/Foam Packaging", "Bottle Caps (Plastic)", "Pull Tabs", "Cigarette Lighters", "Rope", "Six-Pack Holders", "Fishing Line")) 

# png(here("figures", "Fig1_boxplot.png"), units="in", width=6, height=4, res=300)
ccd_tidy %>%
  dplyr::filter(Item %in% Item_f) %>%
  dplyr::mutate(Item_f = factor(Item, levels = c("Food Wrappers/Containers", "Cups, Plates, Forks, Knives, Spoons TOTAL", "Caps, Lids TOTAL", "Beverage Bottles (Plastic) 2 liters or less", "Bags TOTAL", "Straws, Stirrers", "Cigarettes/Cigarette Filters", "Building/Construction Materials", "Clothing, Shoes", "Cigar Tips", "Bags PLASTIC", "Balloons", "Other Plastic/Foam Packaging", "Bottle Caps (Plastic)", "Pull Tabs", "Cigarette Lighters", "Rope", "Six-Pack Holders", "Fishing Line"))) %>%
  ggplot(aes(x = Item_f, y = value)) +
  geom_boxplot(outlier.shape = NA, varwidth = TRUE) +
  scale_x_discrete(limits = rev(levels(Item_f))) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  xlab(NULL) +
  ylab("Count, log10 scale") +
  scale_y_log10(labels = scales::comma) +
  annotation_logticks(sides = "bt") +
  coord_flip()
# dev.off()

## get mean of top 10 items
ccd_formatted %>%
  dplyr::filter(Item %in% Item_f) %>%
  group_by(Item) %>%
  summarise(mean = mean(value)) %>%
  arrange(-mean)

### plots with plastics subcategories added later
## dinnerware plots
subcat.temp <- ccd_use %>%
  dplyr::filter(Item == "Cups & Plates (Foam)" |
                  Item == "Cups & Plates (Plastic)" |
                  Item == "Forks, Knives, Spoons") %>%
  dplyr::select(-"%", -Source, -TOTALS) %>%
  pivot_longer(!Item, names_to = "Year", values_to = "value") %>%
  group_by(Year) %>%
  summarise("Dinnerware (Plastic)" = sum(value, na.rm=TRUE))
subcat.temp$Item <- "Dinnerware (Plastic)"
colnames(subcat.temp) <- c("Year", "value", "Item")
subcat.temp <- subcat.temp[,c("Item", "Year", "value")]
subcat.temp[subcat.temp == 0] <- NA

subcat1 <- ccd_use %>%
  dplyr::filter(Item == "Cups, Plates, Forks, Knives, Spoons TOTAL") %>%
  dplyr::select(-"%", -Source, -TOTALS) %>%
  pivot_longer(!Item, names_to = "Year", values_to = "value") %>%
  bind_rows(., subcat.temp)
str(subcat1)
subcat1$Item[subcat1$Item == "Cups, Plates, Forks, Knives, Spoons TOTAL"]  <- "Dinnerware (Total)"

dinner1 <- ggplot(subcat1, aes(x = Year, y = value, group=Item, color=Item)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkmagenta", "brown2")) +
  # scale_color_viridis(discrete = TRUE) +
  ggtitle("Count of plastic dinnerware & total dinnerware over time") +
  theme_bw() +
  ylab("Count of dinnerware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(size = 10, face = "bold"))
dinner3 <- subcat1 %>%
  dplyr::filter(Year > 2012) %>%
  ggplot(aes(fill=Item, y = value, x = Year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("darkmagenta", "brown2")) +
  theme_bw() +
  ggtitle("Counts of plastic & total dinnerware") +
  theme(plot.title = element_text(size = 10, face = "bold"))
dinner2 <- subcat1 %>%
  dplyr::filter(Year > 2012) %>%
  pivot_wider(names_from = Item, values_from = value) %>%
  `colnames<-`(c("Year", "Total", "Plastic")) %>%
  rowwise(Year) %>%
  dplyr::mutate(NonPlastic = Total - Plastic) %>%
  dplyr::select(-Total) %>%
  `colnames<-`(c("Year", "Dinnerware (Plastic)", "Dinnerware (Non-Plastic)")) %>%
  pivot_longer(!Year, names_to = "Item", values_to = "value") %>%
  ggplot(., aes(x = Year, y = value, fill = Item)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c("brown2", "darkmagenta")) +
  theme_bw() +
  ylab("Proportion") +
  ggtitle("Proportion of plastic to total dinnerware") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))
library(cowplot)
bottom_row <- plot_grid(dinner2, dinner3, labels = c('B', 'C'), label_size = 12, rel_widths = c(1.33,2))
# png(here("figures", "Fig3_dinnerware.png"), units="in", width=8, height=5, res=300)
plot_grid(dinner1, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)
# dev.off()

## plastic dinnerware types
subcat2 <- ccd_use %>%
  dplyr::filter(Item == "Cups & Plates (Foam)" |
                  Item == "Cups & Plates (Plastic)" |
                  Item == "Forks, Knives, Spoons") %>%
  dplyr::select(-"%", -Source, -TOTALS) %>%
  pivot_longer(!Item, names_to = "Year", values_to = "value") %>%
  drop_na()
plasticdinner1 <- subcat2 %>%
  ggplot(aes(fill=Item, y = value, x = Year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis_d() +
  theme_bw() +
  ggtitle("Counts of dinnerware in plastics subcategories") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        legend.position = "bottom")
plasticdinner2 <- subcat2 %>%
  ggplot(aes(x = Year, y = value, fill = Item)) +
  geom_col(position = "fill") +
  scale_fill_viridis_d() +
  theme_bw() +
  ylab("Proportion") +
  ggtitle("Proportions of dinnerware in plastics subcategories") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))
# png(here("figures", "Fig4_dinnerware.png"), units="in", width=6, height=8, res=300)
plot_grid(plasticdinner1, plasticdinner2, labels = c('A', 'B'), label_size = 12, ncol = 1, rel_heights = c(1.33,1))
# dev.off()


## plastic bags plots

subcat_bags <- ccd_raw %>%
  dplyr::filter(Item == "Bags TOTAL" |
                  Item == "Bags PLASTIC" |
                  Item == "Bags PLASTIC - GROCERY" |
                  Item == "Bags PLASTIC - OTHER") %>%
  dplyr::select(-"%", -Source, -TOTALS) %>%
  pivot_longer(!Item, names_to = "Year", values_to = "value")
str(subcat_bags)
bags1 <- ggplot(subcat_bags, aes(x = Year, y = value, group=Item, color=Item)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkmagenta", "darkseagreen", "darkseagreen4", "brown2")) +
  # scale_color_viridis(discrete = TRUE) +
  ggtitle("Count of plastic bags & total bags over time") +
  theme_bw() +
  ylab("Count of plastic bags") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(size = 10, face = "bold"))
bags2 <- subcat_bags %>%
  dplyr::filter(Year > 2007) %>%
  dplyr::filter(!Item == "Bags PLASTIC - GROCERY") %>%
  dplyr::filter(!Item == "Bags PLASTIC - OTHER") %>%
  pivot_wider(names_from = Item, values_from = value) %>%
  `colnames<-`(c("Year", "Total", "Plastic")) %>%
  rowwise(Year) %>%
  dplyr::mutate(NonPlastic = Total - Plastic) %>%
  dplyr::select(-Total) %>%
  `colnames<-`(c("Year", "Bags (Plastic)", "Bags (Non-Plastic)")) %>%
  pivot_longer(!Year, names_to = "Item", values_to = "value") %>%
  ggplot(., aes(x = Year, y = value, fill = Item)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c("brown2", "darkmagenta")) +
  theme_bw() +
  ylab("Proportion") +
  ggtitle("Proportion of plastic to total bags") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))
# png(here("figures", "Fig6_bags.png"), units="in", width=8, height=3, res=300)
bags1
# dev.off()
# png(here("figures", "Fig7_bags.png"), units="in", width=4, height=3, res=300)
bags2
# dev.off()


## caps & lids plots
subcat_caps <- ccd_raw %>%
  dplyr::filter(Item == "Caps, Lids TOTAL" |
                  Item == "Bottle Caps (Plastic)" |
                  Item == "Bottle Caps (Metal)" |
                  Item == "Lids (Plastic)") %>%
  dplyr::select(-"%", -Source, -TOTALS) %>%
  pivot_longer(!Item, names_to = "Year", values_to = "value")
str(subcat_caps)
caps1 <- ggplot(subcat_caps, aes(x = Year, y = value, group=Item, color=Item)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkseagreen", "darkmagenta", "brown2", "darkseagreen4")) +
  # scale_color_viridis(discrete = TRUE) +
  ggtitle("Count of caps & lids over time") +
  theme_bw() +
  ylab("Count of caps & lids") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(size = 10, face = "bold"))
caps1
caps3 <- subcat_caps %>%
  dplyr::filter(Year > 2012 & !Item == "Caps, Lids TOTAL") %>%
  ggplot(aes(fill=Item, y = value, x = Year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("darkseagreen", "darkmagenta", "darkseagreen4")) +
  theme_bw() +
  ggtitle("Counts of Caps & Lids subcategories") +
  theme(plot.title = element_text(size = 10, face = "bold"))
caps3
caps2 <- subcat_caps %>%
  dplyr::filter(Year > 2012 & !Item == "Caps, Lids TOTAL") %>%
  ggplot(aes(x = Year, y = value, fill = Item)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c("darkseagreen", "darkmagenta", "darkseagreen4")) +
  theme_bw() +
  ylab("Proportion") +
  ggtitle("Proportions of Caps & Lids subcategories") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))
caps2
bottom_caps <- plot_grid(caps2, caps3, labels = c('B', 'C'), label_size = 12, rel_widths = c(1.33,2))
# png(here("figures", "Fig8_caps.png"), units="in", width=8, height=5, res=300)
plot_grid(caps1, bottom_caps, labels = c('A', ''), label_size = 12, ncol = 1)
# dev.off()


### histograms of item rank from 2013-2017 -- excluding aggregate categories & non-plastic categories
ccd_raw$Item
## remove explicitly non-plastic items & aggregate categories
rm_extra2 <- c("Bags TOTAL", "Caps, Lids TOTAL", "Cups, Plates, Forks, Knives, Spoons TOTAL",
               "Fishing Buoys, Pots, & Traps TOTAL", "Bags PLASTIC")

ccd_use2 <- ccd_raw %>%
  filter(!Item %in% rm_nonp,
         !Item %in% rm_extra2)

## make tidy
ccd_tidy2 <- ccd_use2 %>%
  dplyr::select(-Source, -TOTALS, -"%") %>%
  pivot_longer(!Item, names_to = "Year", values_to = "value")

## rank
ccd_formatted2 <- ccd_tidy2 %>%
  group_by(Year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = Item) %>%
  group_by(Item) %>% 
  filter(rank <=10) %>%
  ungroup()

# png(here("figures", "Fig9.png"), units="in", width=6, height=4, res=300)
ccd_formatted2 %>%
  filter(Year > 2012) %>%
  group_by(Item) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(Item,(count)), y = count)) +
  geom_bar(stat = 'identity') +
  ylab("Years in Top 10 rank of CCD data") +
  xlab("Plastic-inclusive items, CCD data") +
  coord_flip() +
  theme_bw() 
# dev.off()

Item_f2 <- factor(ccd_formatted2$Item, levels = c("Straws, Stirrers", "Other Plastic/Foam Packaging", "Lids (Plastic)", "Food Wrappers/Containers", "Cigarettes/Cigarette Filters", "Bottle Caps (Plastic)", "Beverage Bottles (Plastic) 2 liters or less", "Bags PLASTIC - OTHER", "Bags PLASTIC - GROCERY", "Take Out/Away Containers (Plastic)", "Building/Construction Materials", "Take Out/Away Containers (Foam)")) 

# png(here("figures", "Fig9_boxplot.png"), units="in", width=6, height=4, res=300)
ccd_tidy2 %>%
  dplyr::filter(Year > 2012) %>%
  dplyr::filter(Item %in% Item_f2) %>%
  dplyr::mutate(Item_f2 = factor(Item, levels = c("Straws, Stirrers", "Other Plastic/Foam Packaging", "Lids (Plastic)", "Food Wrappers/Containers", "Cigarettes/Cigarette Filters", "Bottle Caps (Plastic)", "Beverage Bottles (Plastic) 2 liters or less", "Bags PLASTIC - OTHER", "Bags PLASTIC - GROCERY", "Take Out/Away Containers (Plastic)", "Building/Construction Materials", "Take Out/Away Containers (Foam)"))) %>%
  ggplot(aes(x = Item_f2, y = value)) +
  geom_boxplot(outlier.shape = NA, varwidth = TRUE) +
  scale_x_discrete(limits = rev(levels(Item_f2))) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  xlab(NULL) +
  ylab("Count, log10 scale") +
  scale_y_log10(labels = scales::comma) +
  annotation_logticks(sides = "bt") +
  coord_flip()
# dev.off()

## get mean of top 10 items
ccd_formatted2 %>%
  dplyr::filter(Year > 2012) %>%
  dplyr::filter(Item %in% Item_f2) %>%
  group_by(Item) %>%
  summarise(mean = mean(value)) %>%
  arrange(-mean)


###################
###################
## plot animated graphs (don't work well yet)

staticplot = ggplot(ccd_formatted, aes(rank, group = Item, 
                                       fill = as.factor(Item), color = as.factor(Item))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Item, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = value, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(Year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Most commonly collected plastics',  
       subtitle  =  "Top 10 Categories that include plastics",
       caption  = "CA Coastal Cleanup Day, 1988-2017 | Data Source: www.coastalcleanupdata.org")
anim
animate(anim, 100, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("ccd_allcounties_top10.gif"))

## try line graph?
ccd_formatted %>%
  ggplot(aes(x=Year, y=value, group=Item, color=Item)) +
  geom_line() +
  geom_point() +
  # scale_color_viridis(discrete = TRUE) +
  ggtitle("Top 10 Plastic Items Collected during CA Coastal Cleanup Day") +
  theme_bw() +
  ylab("Count of items") +
  transition_reveal(Year)


## by county
ccd_sandiego <- read_excel(here("data", "CCD_SanDiego.xlsx"))

path <- here("data", "CCD_ByCounty.xlsx")

mad <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = path,
         .id = "County")


## non-plastic categories
notp <- c("Other Trash (Clean Swell)", "Other Packaging (Clean Swell)", "Cups, Plates (Paper)",
          "Bottle Caps (Metal)", "Beverage Cans", "Beverage Bottles (Glass)", "Paper Bags")

## remove non-plastic categories
mad <- mad %>%
  filter(!Item %in% notp)

## try formatting from:
mad_formatted <- mad %>%
  group_by(Year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-Total),
         Total_rel = Total/Total[rank==1],
         Total_lbl = paste0(" ",round(Total/1e9))) %>%
  group_by(Item) %>% 
  filter(rank <=10) %>%
  ungroup()

levels(mad_formatted$Item) <- gsub(" ", "\n", levels(mad_formatted$Item))

staticplot = ggplot(mad_formatted, aes(rank, group = Item, 
                                       fill = as.factor(Item), color = as.factor(Item))) +
  geom_tile(aes(y = Total/2,
                height = Total,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Item, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Total,label = Total, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(Year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Most commonly collected plastics',  
       subtitle  =  "Top 10 Categories that include plastics",
       caption  = "CA Coastal Counties, 2015-2021 | Data Source: World Bank Data")

animate(anim, 100, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))
