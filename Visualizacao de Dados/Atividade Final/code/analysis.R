
library(readxl)
library(dplyr)
library(stringr)
library(magrittr)
library(purrr)
library(tidyr)
library(ggplot2)

# base de dados -----------------------------------------------------------

# raw_data <- 
#   map(1:32,
#       ~ read_excel("../data/tabela_registro_civil.xlsx", skip = 4, n_max = 380, sheet = .x) %>% 
#         select(-c(1:3)) %>% 
#         rename_at(1:2, ~ c("idade_da_mae_na_ocasiao_do_parto", "ano")) %>% 
#         mutate(idade_da_mae_na_ocasiao_do_parto = rep(idade_da_mae_na_ocasiao_do_parto[seq(1,380,10)], each = 10)) %>% 
#         cbind(
#           "GR_UF" = read_excel("../data/tabela_registro_civil.xlsx", skip = 2, n_max = 2, sheet = .x) %>% 
#             select(1) %>% 
#             names() %>% 
#             str_sub(start = 49)
#         ) %>% 
#         as_tibble() %>% 
#         mutate(GR_UF = as.character(GR_UF))
#   )

base_gr <-
  map(1:5,
      ~ read_excel("../data/tabela_registro_civil.xlsx", skip = 4, n_max = 15, sheet = .x) %>%
        select(-c(1:4)) %>%
        rename_at(1, ~ c("ano")) %>%
        cbind(
          "GR" = read_excel("../data/tabela_registro_civil.xlsx", skip = 2, n_max = 2, sheet = .x) %>%
            select(1) %>%
            names() %>%
            str_sub(start = 49)
        ) %>%
        as_tibble() %>%
        mutate(GR = as.character(GR))
  )

base_uf <-
  map(6:32,
      ~ read_excel("../data/tabela_registro_civil.xlsx", skip = 4, n_max = 15, sheet = .x) %>%
        select(-c(1:4)) %>%
        rename_at(1, ~ c("ano")) %>%
        cbind(
          "UF" = read_excel("../data/tabela_registro_civil.xlsx", skip = 2, n_max = 2, sheet = .x) %>%
            select(1) %>%
            names() %>%
            str_sub(start = 49)
        ) %>%
        as_tibble() %>%
        mutate(UF = as.character(UF))
  )

# base_gr %>% bind_rows() %>% write.csv("../data/base por gr.csv", row.names = F)
# base_uf %>% bind_rows() %>% write.csv("../data/base por uf.csv", row.names = F)


# rascunho ----------------------------------------------------------------

meses_12 <- base_gr %>% 
  bind_rows() %>% 
  gather(mes, value, -ano, -GR) %>% 
  distinct(mes)

# p1 <- 
  base_gr %>% 
  bind_rows() %>% 
  gather(mes, value, -ano, -GR) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(mes) %>% 
  summarise(media = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mes = factor(mes, levels = meses_12$mes)) %>% 
  ggplot(aes(mes, media)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  theme_minimal() +
  geom_text(aes(label = media %>% round), nudge_y = -1000, color = "white") +
  labs(x = "", y = "Média", title = "Média de nascimento por mês (2003-2017)")


# -------------------------------------------------------------------------

meses

temp_sol <- 
  base_gr %>% 
  bind_rows() %>% 
  select(13,2,3, 8:10) %>% 
  gather(mes, value)
  # mutate(value = as.numeric(value)) %>% 
  # group_by(mes) %>% 
  # summarise(media = mean(value, na.rm = T)) %>% 
  # ungroup()

temp_sol %>% 
  mutate(tempo = if_else((mes=="Dezembro"|mes=="Janeiro"|mes=="Fevereiro"), "Calor", "Frio")) %>% 
  ggplot(aes(tempo, value)) +
  geom_boxplot()


# -------------------------------------------------------------------------

base_gr %>% 
  bind_rows() %>% 
  gather(mes, value, -ano, -GR) %>%
  group_by(GR, mes) %>% 
  summarise(media = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mes = factor(mes, levels = meses_12$mes)) %>% 
  ggplot(aes(mes, media)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  facet_grid(~ GR) +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = media %>% round), nudge_y = 15000) +
  labs(x = "", y = "Média", title = "Média de nascimento por mês (2003-2017)")
  































