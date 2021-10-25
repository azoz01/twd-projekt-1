db <- read.csv2(file = "data/WorldMeatProductionDatasets/animals-slaughtered-for-meat.csv",
                sep = ",", header = T)

library(dplyr)
library(ggplot2)
library(SmarterPoland)
library(forcats)
library(tidyr)
library(stringr)

db %>% 
  filter(Entity == "World") %>%
  mutate(meat = Cattle..cattle.slaughtered. / 1000000) %>% 
  ggplot(aes(Year, meat)) + 
  geom_path() +
  geom_point(aes(x = 1978, y = 245.202771), color = "red", size = 5) +
  geom_text(aes(x = 1978, y = 245.202771), label = "1978", vjust = -2) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  labs(title = "Cattle slaughtered around the world",
       subtitle = "JPII impact",
       x = "Year",
       y = "Cattle slaughtered (x million)")


options(scipen = 999)

china <- db %>% 
  filter(Entity == "China") %>% 
  mutate(continent = "China")

usa <- db %>% 
  filter(Entity == "United States") %>% 
  mutate(continent = "USA")

countries %>% 
  select(country, continent) %>% 
  rename(Entity = country) %>% 
  inner_join(db) %>% 
  union(china) %>% 
  union(usa) %>% 
  group_by(continent, Year) %>% 
  summarise(sum = sum(Cattle..cattle.slaughtered.)) %>% 
  ggplot(aes(Year, sum, color = continent)) + 
  geom_path(size = 1.5) + 
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Cattle slaughtered depending on continent",
       subtitle = "Also USA and China with no particural reason",
       x = "Year",
       y = "Cattle slaughtered")

countries %>% 
  select(country, continent) %>% 
  rename(Entity = country) %>% 
  inner_join(db) %>% 
  group_by(continent, Year) %>% 
  summarise(sum = sum(Cattle..cattle.slaughtered.)) %>% 
  ggplot(aes(Year, sum, color = continent)) + 
  geom_path(size = 1.5) + 
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Cattle slaughtered depending on continent",
       x = "Year",
       y = "Cattle slaughtered")


db %>% 
  filter(Entity == "China") %>% 
  ggplot(aes(Year, Cattle..cattle.slaughtered.)) + 
  geom_path(color = "red", size = 1.5) + 
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Cattle slaughtered in China",
       x = "Year",
       y = "Cattle slaughtered")

options(scipen = 999)

db %>% 
  filter(Entity == "World") %>%
  rename(cattle = Cattle..cattle.slaughtered.,
         goats = Goats..goats.slaughtered.,
         turkey = Turkey..turkeys.slaughtered.,
         pigs = Pigs..pigs.slaughtered.,
         sheep = Sheep..sheeps.slaughtered.) %>% 
  pivot_longer(cols = c(cattle, goats, turkey, pigs, sheep),
               names_to = "meat", values_to = "value") %>% 
  ggplot(aes(Year, value, color = meat)) + 
  geom_path(size = 1.2) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  labs(title = "animals slaughtered around the world",
       subtitle = "",
       x = "Year",
       y = "animals slaughtered")




##########################################################
# plots that make no particular sense, left here as archive

# countries %>% 
#   select(country, continent) %>% 
#   rename(Entity = country) %>% 
#   inner_join(db) %>% 
#   filter(continent == "Europe") %>% 
#   ggplot(aes(Year, Cattle..cattle.slaughtered., color = Entity)) + 
#   geom_path(size = 1.5) + 
#   scale_x_continuous(breaks = seq(1960, 2020, 5)) +
#   theme(axis.text.x = element_text(angle = 45)) + 
#   xlim(1990, 1994)

# countries %>% 
#   select(country, continent) %>% 
#   rename(Entity = country) %>% 
#   inner_join(db) %>% 
#   filter(continent == "Americas") %>% 
#   ggplot(aes(Year, Cattle..cattle.slaughtered., color = Entity)) + 
#   geom_path(size = 1.5) + 
#   scale_x_continuous(breaks = seq(1960, 2020, 5)) +
#   theme(axis.text.x = element_text(angle = 45)) + 
#   xlim(1975, 1980)

# countries %>% 
#   select(country, continent) %>% 
#   rename(Entity = country) %>% 
#   inner_join(db) %>%
#   select(Entity, continent, Year, Cattle..cattle.slaughtered.) %>% 
#   filter(Year >= 2004 & Year <= 2006) %>% 
#   pivot_wider(names_from = Year, values_from = Cattle..cattle.slaughtered.) %>% 
#   rename(r2004 = "2004", r2005 = "2005", r2006 = "2006") %>% 
#   mutate(diff = abs(r2006 - r2004)) %>% 
#   top_frac(0.1, diff) %>% 
#   pivot_longer(cols = c(r2004, r2005, r2006), names_to = "Year", values_to = "meat") %>%
#   mutate(Year = as.integer(str_sub(Year, start = 2))) %>% 
#   ggplot(aes(Year, meat, color = Entity)) + 
#   geom_path()


#### general

# db %>% 
#   filter(Entity == "World") %>%
#   mutate(meat = Cattle..cattle.slaughtered. / 1000000) %>% 
#   ggplot(aes(Year)) + 
#   geom_path(aes(y = Goats..goats.slaughtered.), color ="red") +
#   geom_path(aes(y = Cattle..cattle.slaughtered.), color = "blue") +
#   # geom_path(aes(y = Chicken..chicken.slaughtered., color = "green")) +
#   geom_path(aes(y = Pigs..pigs.slaughtered.), color = "black") +
#   geom_path(aes(y = Sheep..sheeps.slaughtered.), color = "purple") +
#   scale_x_continuous(breaks = seq(1960, 2020, 5)) +
#   labs(title = "animals slaughtered around the world",
#        subtitle = "",
#        x = "Year",
#        y = "animals slaughtered (x million)")




# db %>% 
#   ggplot(aes(Year, Cattle..cattle.slaughtered., color = Entity)) + 
#   geom_path() + 
#   scale_x_continuous(breaks = 1960:2020) +
#   theme(axis.text.x = element_text(angle = 45))
# 
# db %>%
# filter(Entity == "India" & Year > 1984 & Year < 1988) %>%
# summarise(min = min(Cattle..cattle.slaughtered.))

# db %>% 
# filter(Entity == "India" & Year > 1984 & Year < 1988 & Cattle..cattle.slaughtered. == 10310000)
