library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

library(dplyr)
library(tidyr)
library(SmarterPoland)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# ggplot(data = world) +
#   geom_sf()

# world %>%
#   mutate(info = 2) %>%
#   ggplot(aes(fill = info)) +
#   geom_sf()

fao <- read.csv2("data/FAO.csv", header = T, sep = ",")
food <- read.csv2("data/Food_Production.csv", header = T, sep = ",")


years <- paste("Y", 1961:2013, sep = "")

fao2013 <- fao %>% 
  pivot_longer(paste("Y", 1961:2013, sep = ""),
               names_to = "Year",
               values_to = "value") %>% 
  filter(Year == "Y2013") %>% 
  select(-Year)

typeoffood <- food %>% 
  select(Food.product, Eutrophying.emissions.per.kilogram..gPOâ..eq.per.kilogram.) %>% 
  rename(Item = Food.product)

food.summary <- fao2013 %>% 
  inner_join(typeoffood, by = "Item") %>% 
  rename(emissionCO2 = Eutrophying.emissions.per.kilogram..gPOâ..eq.per.kilogram.) %>% 
  mutate(emissionCO2 = as.numeric(emissionCO2)) %>% 
  mutate(total.emission = value * emissionCO2 * 1000) %>% 
  group_by(Area.Abbreviation, Area) %>% 
  summarise(emissionCO2 = sum(total.emission, na.rm = T)) %>% 
  rename(name = Area.Abbreviation, fullname = Area) %>% 
  mutate(emissionCO2 = as.integer(emissionCO2))

world.emission <- world %>% 
  merge(food.summary, by.x  = "iso_a3", by.y = "name", all.x = T)


world.emission <- world.emission %>% 
  mutate(emissionCO2.per.capita = emissionCO2 / pop_est)

# Qatar have big af emission, don't know why, but this broke whole plot, so
# I'm gonna delete this record

world.emission <- world.emission %>% 
  mutate(emissionCO2.per.capita = ifelse(emissionCO2.per.capita < 50,
                                         emissionCO2.per.capita, NA))

world.emission %>% 
  ggplot(aes(fill = emissionCO2.per.capita, geometry = geometry)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Yearly CO2 emission per capita",
       subtitle = "According to FAO data \nAlso with no scale but who cares",
       fill = "Kilograms of CO2 per capita (x1000)")

############################################################################
# PER PROTEIN


typeoffood <- food %>% 
  select(Food.product, Eutrophying.emissions.per.100g.protein..gPOâ..eq.per.100.grams.protein.) %>% 
  rename(Item = Food.product)

food.summary <- fao2013 %>% 
  inner_join(typeoffood, by = "Item") %>% 
  rename(emissionCO2 = Eutrophying.emissions.per.100g.protein..gPOâ..eq.per.100.grams.protein.) %>% 
  mutate(emissionCO2 = as.numeric(emissionCO2)) %>% 
  mutate(total.emission = value * emissionCO2 * 1000) %>% 
  group_by(Area.Abbreviation, Area) %>% 
  summarise(emissionCO2 = sum(total.emission, na.rm = T)) %>% 
  rename(name = Area.Abbreviation, fullname = Area) %>% 
  mutate(emissionCO2 = as.integer(emissionCO2))

world.emission <- world %>% 
  merge(food.summary, by.x  = "iso_a3", by.y = "name", all.x = T)


world.emission <- world.emission %>% 
  mutate(emissionCO2.per.capita = emissionCO2 / pop_est)

# Qatar have big af emission, don't know why, but this broke whole plot, so
# I'm gonna delete this record

world.emission <- world.emission %>%
  mutate(emissionCO2.per.capita = ifelse(emissionCO2.per.capita < 8,
                                         emissionCO2.per.capita, NA))

world.emission %>% 
  ggplot(aes(fill = emissionCO2.per.capita, geometry = geometry)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Yearly CO2 emission per capita",
       subtitle = "According to FAO data \nPer 100g protein? Does it even make sense?",
       fill = "Kilograms of CO2 per capita (x1000)")


##############################################################################
# PER WATER

typeoffood <- food %>% 
  select(Freshwater.withdrawals.per.kilogram..liters.per.kilogram., Food.product) %>% 
  rename(Item = Food.product)

food.summary <- fao2013 %>% 
  inner_join(typeoffood, by = "Item") %>% 
  rename(emissionCO2 = Freshwater.withdrawals.per.kilogram..liters.per.kilogram.) %>% 
  mutate(emissionCO2 = as.numeric(emissionCO2)) %>% 
  mutate(total.emission = value * emissionCO2 * 1000) %>% 
  group_by(Area.Abbreviation, Area) %>% 
  summarise(emissionCO2 = sum(total.emission, na.rm = T)) %>% 
  rename(name = Area.Abbreviation, fullname = Area) %>% 
  mutate(emissionCO2 = as.integer(emissionCO2))

world.emission <- world %>% 
  merge(food.summary, by.x  = "iso_a3", by.y = "name", all.x = T)


world.emission <- world.emission %>% 
  mutate(emissionCO2.per.capita = emissionCO2 / pop_est)

# Qatar have big af emission, don't know why, but this broke whole plot, so
# I'm gonna delete this record

world.emission <- world.emission %>%
  mutate(emissionCO2.per.capita = ifelse(emissionCO2.per.capita < 60,
                                         emissionCO2.per.capita, NA))

world.emission %>% 
  ggplot(aes(fill = emissionCO2.per.capita, geometry = geometry)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Yearly freshwater wasting per capita",
       subtitle = "According to FAO data \nAlso with no scale but who cares",
       fill = "Liters of freshwater per capita (x1000)")

