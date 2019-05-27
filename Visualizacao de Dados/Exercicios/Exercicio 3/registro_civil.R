
# Pacotes -----------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

options(scipen = 99999999)

# Import ------------------------------------------------------------------

data <- read_excel("Registro Civil.xlsx", skip = 2)

# Graficos ----------------------------------------------------------------

rnv_total <- 
  data %>%  
  .[1, 2:6] %>%
  gather(Ano, Total) %>% 
  ggplot(aes(Ano, Total)) +
  geom_bar(fill = "#661313", stat = 'identity') +
  geom_text(aes(label = Total), nudge_y = 0.2) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  ggtitle("Registro de Nascidos Vivos")

rnv_sexo <-
  data %>% 
  .[3:4, ] %>% 
  rename("Sexo" = `Registros de nascidos vivos`) %>%
  gather(Ano, Total, -Sexo) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sexo)) +
  geom_point(shape = 15, size = 2) +
  geom_line(aes(group = Sexo), linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("#00bfbf", "#c680cd")) +
  # scale_y_continuous(breaks = seq(1410000, 1570000, 10000)) +
  theme_linedraw() +
  ggtitle("Registro de Nascidos Vivos")

rnv_idade_mae <-
  data %>% 
  .[c(7:50), ] %>% 
  .[-(c(7,8,14,20,26,32,38,44,50)-6), ] %>% 
  gather(Ano, Total, -`Registros de nascidos vivos`) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  ggplot(aes(x = `Registros de nascidos vivos`, y = Total)) +
  geom_line(aes(group = Ano)) +
  facet_grid(~ Ano, switch = 'y') +
  theme_linedraw() +
  coord_flip() +
  theme(strip.text.y = element_text(angle = 180)) +
  labs(x = "Idade da mãe na ocasião do parto", title = "Idade da mãe na ocasião do parto por Ano")


# data %>% 
#   .[c(7,8,14,20,26,32,38,44,50), ] %>% 
#   gather(Ano, Total, -`Registros de nascidos vivos`) %>% 
#   mutate(Total = as.numeric(Total)) %>% 
#   ggplot(aes(x = `Registros de nascidos vivos`, y = Total)) +
#   geom_line(aes(group = Ano)) +
#   scale_y_continuous(breaks = seq(0, 750000, 250000)) +
#   facet_grid(~ Ano) +
#   theme_linedraw() +
#   coord_flip() +
#   labs(x = "Idade da mãe na ocasião do parto",
#        title = "Idade da mãe na ocasião do parto por Ano")



# base_mod <- data %>% .[53:104, ]
#   
# base_mod %>% 
#   mutate(ano_registro = c(as.character(2016:2011), rep("2010 ou menos", nrow(.)-6))) %>% 
#   mutate_at(2:6, ~ ifelse(.x == "...", 0, .x) %>% as.numeric()) %>% 
#   group_by(ano_registro) %>%
#   summarise(
#     "2013" = sum(`2013`, na.rm = T),
#     "2014" = sum(`2014`, na.rm = T),
#     "2015" = sum(`2015`, na.rm = T),
#     "2016" = sum(`2016`, na.rm = T),
#     "2017" = sum(`2017`, na.rm = T)
#   ) %>% 
#   gather(Ano, Total, -ano_registro) %>%
#   # mutate(Total = as.numeric(Total)) %>%
#   ggplot(aes(x = ano_registro, y = Total)) +
#   facet_grid(~ Ano) +
#   geom_col() +
#   # geom_text(aes(label = Total)) +
#   coord_flip()

rnv_sexo / (rnv_idade_mae + rnv_total)








