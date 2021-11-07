
vege <- read.csv2("./data/FAO_for_waffle_enchanced/vege.csv", header = T, sep = ",")
meat <- read.csv2("./data/FAO_for_waffle_enchanced/meat.csv", header = T, sep = ",")
diary <- read.csv2("./data/FAO_for_waffle_enchanced/diary.csv", header = T, sep = ",")

library(dplyr)
library(waffle)
library(ggplot2)
library(forcats)
library(tidyr)
library(ggpubr)



vege <- mutate(vege, type = "vege") %>% 
  filter(Item %in% c("Fruit Primary", "Vegetables Primary"))
meat <- mutate(meat, type = "livestock")
diary <- mutate(diary, type = "livestock")

food <- bind_rows(vege, meat, diary) %>% 
  select(Year, Value, type) %>% 
  group_by(Year, type) %>% 
  summarise(sum = sum(Value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Year, values_from = sum, names_prefix = 'r') %>% 
  pivot_longer(cols = c("r1993", "r2013"), names_to = "Year", values_to = "Value") %>% 
  arrange(Year)

food1993 <- food %>% filter(Year == 'r1993')
food2013 <- food %>% filter(Year == 'r2013')

AVG_LOSS <- 0.33

food1993 <- food1993 %>%
  mutate(Value = Value / 1E7) %>% 
  mutate(WasteValue = Value * AVG_LOSS) %>% 
  mutate(Value = Value - WasteValue) %>% 
  mutate(Value = as.integer(Value), WasteValue = as.integer(WasteValue)) %>% 
  pivot_longer(names_to = 'isWasted', cols = c(Value, WasteValue), values_to = 'Value') %>% 
  mutate(v = paste(type, isWasted)) %>% 
  arrange(isWasted)

food2013 <- food2013 %>%
  mutate(Value = Value / 1E7) %>% 
  mutate(WasteValue = Value * AVG_LOSS) %>% 
  mutate(Value = Value - WasteValue) %>% 
  mutate(Value = as.integer(Value), WasteValue = as.integer(WasteValue)) %>% 
  pivot_longer(names_to = 'isWasted', cols = c(Value, WasteValue), values_to = 'Value') %>% 
  mutate(v = paste(type, isWasted)) %>% 
  arrange(isWasted)


food1993$v <- fct_inorder(food1993$v)
food2013$v <- fct_inorder(food2013$v)



plot2013 <- ggplot(food2013, aes(fill = v, values = Value)) +
  geom_waffle(n_rows = 5, size = 3, color = "white") +
  scale_fill_manual(
    values = c("#258e25", "#cc0000", "#85e085", "#ff9999")
  ) +
  coord_equal() + 
  theme_void()

plot1993 <- ggplot(food1993, aes(fill = v, values = Value)) +
  geom_waffle(n_rows = 5, size = 3, color = "white") +
  scale_fill_manual(
    values = c("#258e25", "#cc0000", "#85e085", "#ff9999")
  ) +
  coord_equal() + 
  theme_void()

# ggarrange(plot1993, plot2013, nrow = 2)

plot1993 +
  labs(
    title = "Food production and waste in 2013",
    fill = "Type"
  )
