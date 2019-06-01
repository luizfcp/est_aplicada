
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(scales)

options(scipen = 99999999)

# Import ------------------------------------------------------------------

data1 <- read_excel("../data/Registro Civil.xlsx", skip = 2)
data2 <- read_excel("../data/Registro Civil - 2.xlsx", skip = 2)

# Graph -------------------------------------------------------------------

## rnv = Registro de Nascidos Vivos

rnv_total <- 
  data1 %>%  
  .[1, 2:6] %>%
  gather(Ano, Total) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  ggplot(aes(Ano, Total)) +
  geom_bar(fill = "#661313", stat = 'identity') +
  geom_text(aes(label = comma(Total, big.mark = ".", decimal.mark = ",")), 
            nudge_y = -99000, col = "white", fontface = "bold") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "") # , title = "Total de Registros de Nascidos Vivos"

rnv_sexo <-
  data1 %>% 
  .[3:4, ] %>% 
  rename("Sexo" = `Registros de nascidos vivos`) %>%
  gather(Ano, Total, -Sexo) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sexo)) +
  geom_point(shape = 15, size = 2) +
  geom_line(aes(group = Sexo), linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("purple4", "yellow3")) +
  scale_y_continuous(breaks = seq(1410000, 1570000, 20000)) +
  theme_minimal() # + ggtitle("Número de Registros de Nascidos Vivos por Sexo")

rnv_idade_mae <-
  data1 %>% 
  .[c(7:50), ] %>% 
  .[-(c(8,14,20,26,32,38,44)-6), ] %>% 
  mutate_at(1, ~ ifelse(.x=="Menos de 15 anos", "14 anos ou menos", .x)) %>% 
  gather(Ano, Total, -`Registros de nascidos vivos`) %>% 
  mutate(Total = as.numeric(Total)) %>% 
  ggplot(aes(x = `Registros de nascidos vivos`, y = Total)) +
  geom_line(aes(group = Ano)) +
  facet_grid(~ Ano, switch = 'y') +
  theme_linedraw() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Idade da mãe na ocasião do parto") # , title = "Número de Registros de Nascidos Vivos pela \n Idade da mãe na ocasião do parto"

rnv_mes <-
  data2 %>% 
  .[17:28, ] %>%
  rename("mes" = `Nascidos vivos ocorridos no ano`) %>%
  mutate_at(2:6, ~ as.numeric(.x)) %>% 
  mutate(mes = factor(mes, levels = data2$`Nascidos vivos ocorridos no ano`[17:28])) %>% 
  gather(Ano, Total, -mes) %>% 
  mutate(total_percent = Total %>% round(3) %>% paste0("%")) %>% 
  ggplot(aes(mes, Total, color = Ano)) +
  geom_point(shape = 18, size = 3) +
  geom_line(aes(group = Ano, linetype = Ano), size = 0.5) +
  scale_linetype_manual(values = c(rep("solid", 4), "dashed")) +
  scale_color_manual(values = c("cyan3", "green4", "yellow3", "darkblue", "red")) +
  scale_y_continuous(breaks = seq(195000, 275000, 10000)) +
  labs(x = "Mês") + # , title = "Número de Registros de Nascidos Vivos por Mês"
  theme_minimal()


painel <- 
  ( #--------------------------------------------------------------------------*
    # Plot 1
    #--------------------------------------------------------------------------*
    rnv_mes +
      labs(
        title = "Painel com dados de Registros de Nascidos Vivos \n",
        subtitle = "Número de Registros de Nascidos Vivos por Mês"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      { # <--- nested plots
        #--------------------------------------------------------------------------*
        # Plot 2
        #--------------------------------------------------------------------------*
        rnv_idade_mae + labs(subtitle = "Número de Registros de Nascidos Vivos em \n relação a Idade da mãe na ocasião do parto") +
          { # <--- nested plots
            #----------------------------------------------------------------------*
            # Plot 3
            #----------------------------------------------------------------------*
            rnv_total + labs(subtitle = "Total de Registros de Nascidos Vivos")
          } +
          patchwork::plot_layout(ncol = 2)
      } +
      { # <--- nested plots
        #----------------------------------------------------------------------*
        # Plot 4
        #----------------------------------------------------------------------*
        rnv_sexo + 
          labs(subtitle = "Número de Registros de Nascidos Vivos por Sexo",
               caption = "Fonte: IBGE - Estatísticas do Registro Civil - 2017"
          )
      } +
      plot_layout(ncol = 1, heights = c(1, 2.2, 0.8))
  ); painel

ggsave("../img/painel_registro_civil.png", painel, width=11, height=10, scale=1, dpi="retina")
