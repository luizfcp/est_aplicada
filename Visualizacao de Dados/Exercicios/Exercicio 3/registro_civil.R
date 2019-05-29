
# Pacotes -----------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(scales)

options(scipen = 99999999)

# Import ------------------------------------------------------------------

data <- read_excel("Registro Civil.xlsx", skip = 2)

# Graficos ----------------------------------------------------------------

rnv_total <- 
  data %>%  
  .[1, 2:6] %>%
  gather(Ano, Total) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  ggplot(aes(Ano, Total)) +
  geom_bar(fill = "#661313", stat = 'identity') +
  geom_text(aes(label = comma(Total, big.mark = ".", decimal.mark = ",")), 
            nudge_y = -99000, col = "white", fontface = "bold") +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  labs(title = "Total de Registro de Nascidos Vivos", y = "")

rnv_sexo <-
  data %>% 
  .[3:4, ] %>% 
  rename("Sexo" = `Registros de nascidos vivos`) %>%
  gather(Ano, Total, -Sexo) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sexo)) +
  geom_point(shape = 15, size = 2) +
  geom_line(aes(group = Sexo), linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("#661313", "darkgray")) +
  scale_y_continuous(breaks = seq(1410000, 1570000, 20000)) +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  ggtitle("Total de Registro de Nascidos Vivos por Sexo")

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
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  labs(x = "Idade da mãe na ocasião do parto", 
       title = "Total de Registro de Nascidos Vivos pela \n Idade da mãe na ocasião do parto")

painel <- rnv_total + rnv_idade_mae - rnv_sexo + plot_layout(ncol = 1, heights = c(2.5, 1.5))

ggsave("painel_registro_civil.png", painel, width=13, height=10, scale=1, dpi="retina")


# data %>% 
#   .[c(7:50), ] %>% 
#   .[-(c(7,8,14,20,26,32,38,44,50)-6), ] %>% 
#   mutate(classes_idade = c(as.character(rep("15 a 20 anos", nrow(.)-6)) %>% 




# data %>% 
#   .[c(7,8,14,20,26,32,38,44,50), ] %>% 
#   rename("idade_mae" = `Registros de nascidos vivos`) %>%
#   gather(Ano, Total, -idade_mae) %>% 
#   mutate(Total = as.numeric(Total)) %>% 
#   ggplot(aes(x = Ano, y = Total)) +
#   geom_line(aes(group = idade_mae, col = idade_mae))
  
  
  


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








