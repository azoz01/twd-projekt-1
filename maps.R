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

fao <- read.csv2("../db/FAO.csv", header = T, sep = ",")
food <- read.csv2("../db/Food_Production.csv", header = T, sep = ",")


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
  group_by(Area) %>% 
  summarise(emissionCO2 = sum(total.emission, na.rm = T)) %>% 
  rename(name = Area) %>% 
  mutate(emissionCO2 = as.integer(emissionCO2))

world.emission <- world %>% 
  inner_join(food.summary, by  = "name")

world.emission <- countries %>% 
  select(country, population) %>% 
  rename(name = country) %>% 
  inner_join(world.emission, by = "name")

world.emission %>% 
  ggplot(aes(fill = emissionCO2.per.capita, geometry = geometry)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Yearly CO2 emission per capita",
       subtitle = "According to FAO data \nAlso with no scale but who cares")
