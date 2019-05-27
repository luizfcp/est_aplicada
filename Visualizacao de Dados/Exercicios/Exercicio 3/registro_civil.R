
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Import ------------------------------------------------------------------

data <- read_excel("Registro Civil.xlsx")

# Graficos ----------------------------------------------------------------

registro_nascidos_vivos <- 
  data %>%  
  .[3, 2:6] %>%
  `colnames<-`(data[2, 2:6]) %>% 
  gather(Ano, Total) %>% 
  ggplot(aes(Ano, Total)) +
  geom_bar(fill = "#661313", stat = 'identity') +
  geom_text(aes(label = Total), nudge_y = 0.2) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  ggtitle("Registro de Nascidos Vivos")
