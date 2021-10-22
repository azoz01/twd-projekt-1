library(dplyr)
library(ggplot2)


meat_data <- read.csv('data/WorldMeatProductionDatasets/global-meat-production-by-livestock-type.csv')


# Weights source: https://www.greeneatz.com/foods-carbon-footprint.html

meat_summary <- meat_data %>% 
                  group_by(Year) %>% 
                  summarise(Sheep = sum(Sheep.and.Goat..tonnes., na.rm = T)*39.2,
                            Beef = sum(Beef.and.Buffalo..tonnes., na.rm = T)*27,
                            Pork = sum(Pigmeat..tonnes., na.rm = T)*12.1,
                            Poultry = sum(Poultry..tonnes., na.rm = T)*6.9) %>% 
                  transmute(Year = Year, 
                            'Carbon footprint of meat' = Sheep + Beef + Pork + Poultry)

meat_summary

#18 627 189 021

co2_world_data <- read.csv('data/world_ghg_emission.csv')

data_to_plot = co2_world_data %>% 
                left_join(meat_summary, by='Year') %>% 
                mutate(GHG.emitted = GHG.emitted*1000000/1e9,
                       `Carbon footprint of meat` = `Carbon footprint of meat`/1e9)

data_to_plot %>% 
ggplot() +
  geom_line(aes(Year, GHG.emitted, color = 'World summary'), size = 2) +
  geom_line(aes(Year, `Carbon footprint of meat`, color = 'Meat production'), size = 2) +
  labs(x = 'Year', y = 'Gigatons of GHG', 
       title = 'Comparison of world meat production of GHG to total world GHG emission',
       colour='Legend')

ggsave('plots/ghg_emission_plot.png',  width = 16, height = 8)

