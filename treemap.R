library("ggplot2")
library("treemap")
library(dplyr)

crops <- read.csv2("./data/FAO_for_treemap/FAOSTAT_data_cropsPrimary2018.csv",
                header = T, sep = ",", encoding = "UTF-8")
animal.primary <- read.csv2("./data/FAO_for_treemap/FAOSTAT_data_livestockPrimary2018.csv",
                            header = T, sep = ",", encoding = "UTF-8")
animal.processed <- read.csv2("./data/FAO_for_treemap/FAOSTAT_data_Processed2018.csv",
                              header = T, sep = ",", encoding = "UTF-8")

DEL.CONSTANT <- 1/40

# CROPS

crops <- crops %>% 
  select(Item, Value) %>% 
  mutate(Class = "crops") %>% 
  filter(!is.na(Value)) %>% 
  filter(!grepl(x = Item, pattern = "Total", fixed = T))

cropsN <- crops %>% summarise(n = sum(Value)) %>% pull(n)

crops <- crops %>%
  filter(Value > cropsN * DEL.CONSTANT)

cropsM <- crops %>% summarise(n = sum(Value)) %>% pull(n)

crops <- crops %>% add_row(Item = "Others", Value = cropsN - cropsM, Class = "crops")

# ANIMAL PRIMARY

animal.primary <- animal.primary %>% 
  select(Item, Value) %>% 
  mutate(Class = "animal.primary") %>% 
  filter(!is.na(Value)) %>% 
  filter(!grepl(x = Item, pattern = "Total", fixed = T))

animal.primaryN <- animal.primary %>% summarise(n = sum(Value)) %>% pull(n)

animal.primary <- animal.primary %>%
  filter(Value > animal.primaryN * DEL.CONSTANT)

animal.primaryM <- animal.primary %>% summarise(n = sum(Value)) %>% pull(n)

animal.primary <- animal.primary %>% add_row(Item = "Others",
                                             Value = animal.primaryN - animal.primaryM, Class = "animal.primary")

# ANIMAL PROCESSED

animal.processed <- animal.processed %>% 
  select(Item, Value) %>% 
  mutate(Class = "animal.prcessed") %>% 
  filter(!is.na(Value)) %>% 
  filter(!grepl(x = Item, pattern = "Total", fixed = T))

animal.processedN <- animal.processed %>% summarise(n = sum(Value)) %>% pull(n)

animal.processed <- animal.processed %>%
  filter(Value > animal.processedN * DEL.CONSTANT)

animal.processedM <- animal.processed %>% summarise(n = sum(Value)) %>% pull(n)

animal.processed <- animal.processed %>% add_row(Item = "Others",
                                             Value = animal.processedN - animal.processedM, Class = "animal.prcessed")

# SUMMARY

data <- bind_rows(crops, animal.primary, animal.processed) %>% 
  filter(!is.na(Value)) %>% 
  mutate(Value = as.integer(Value / 1000))

# PLOT

treemap(data,
        index=c("Class","Item"),
        vSize="Value",
        type="index",
        title = "contrubution to food market",
        border.col=c("black","grey"),            
        border.lwds=c(7,3)  ,
        fontface.labels=c(2,2), 
        bg.labels=c("transparent") ,
        align.labels=list(
          c("left", "bottom"), 
          c("center", "center")
        ),      
        overlap.labels=0.5,
        fontsize.labels=c(25,14),
        palette = "Set2"
  )

