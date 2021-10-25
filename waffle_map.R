library("ggplot2")

library(dplyr)
library(tidyr)

fao <- read.csv2("data/FAO.csv", header = T, sep = ",")
food <- read.csv2("data/Food_Production.csv", header = T, sep = ",")


years <- paste("Y", 1961:2013, sep = "")

fao2013 <- fao %>% 
  pivot_longer(paste("Y", 1961:2013, sep = ""),
               names_to = "Year",
               values_to = "value") %>% 
  filter(Year == "Y2013") %>% 
  select(Area.Abbreviation, Item, value) %>% 
  group_by(Item) %>% 
  summarise(value = sum(value))

food.area <- food %>% 
  select(Food.product, Land.use.per.kilogram..mÂ..per.kilogram.) %>% 
  rename(land.use = Land.use.per.kilogram..mÂ..per.kilogram.) %>% 
  rename(Item = Food.product) %>% 
  mutate(land.use = as.numeric(land.use))

db <- food.area %>% 
  inner_join(fao2013, by = "Item") %>%
  mutate(total = value * land.use)

# install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)

v <- as.integer(db$total / 10000 )
names(v) <- db$Item
v <- c(v, others = 130)

# Waffle chart
waffle(v, rows = 14)

##############################################


fao <- read.csv2("data/FAOSTAT_data_10-25-2021.csv", header = T, sep = ",")

t <- summarise(fao, n = sum(Value, na.rm = T))$n

db <- fao %>% 
  select(Item, Value) %>%
  filter(!is.na(Value))

v <- as.integer(db$Value / (t / 100) * 2.7)
names(v) <- db$Item

v <- v[v != 0]
v <- v[order(v, decreasing = T)]

waffle(v[1:6], colors = hcl.colors(7, "Inferno", rev = TRUE, alpha = 0.7)[-1]) +
  labs(title = "Land use in agriculture", 
       subtitle = "one square - 1 mil. hectars")
