
options(scipen = 99999)

# Pacotes utilizados ------------------------------------------------------

library(haven)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggpubr)
library(purrr)
library(flextable)

# Importacao base de dados
base <- 
  read_sav("../data/BancoTeseCris_09_07_2013.sav") %>% 
  mutate(
    Sexo = factor(Sexo, labels = c("Homem", "Mulher"))
  )

# Visualizacao sexo por idade
base %>% 
  select(Sexo, Idade) %>% 
  na.omit() %>% 
  ggplot(aes(x=Sexo, y=Idade)) + 
  geom_boxplot() +
  labs(x = "Sexo", title = "Boxplot Idade por Sexo") + 
  theme_bw()


# PEW ---------------------------------------------------------------------

base_pew <- 
  base %>% 
  select(
    Sexo, Idade, 
    PEW_ANTES, PEW_APOS
  ) %>% 
  na.omit() %>% 
  # Perda no PEW (2 = teve perda)
  mutate(
    PEW_ANTES = factor(PEW_ANTES, labels = c("Não", "Sim")),
    PEW_APOS = factor(PEW_APOS, labels = c("Não", "Sim"))
  )

# Visualizacao PEW ANTES
graf_pew_antes <- 
  base_pew$PEW_ANTES %>% table() %>% as_tibble() %>% 
  ggplot() +
  geom_bar(aes(x=., y=n, fill = .), stat = "identity") +
  theme_bw() +
  labs(x = "Presença de perda", y = "Número de pacientes", title = "PEW ANTES do RETP nos pacientes") +
  scale_y_continuous(limits = c(0, 30)) +
  guides(fill = F)

# Visualizacao PEW APOS
graf_pew_apos <- 
  base_pew$PEW_APOS %>% table() %>% as_tibble() %>% 
  ggplot() +
  geom_bar(aes(x=., y=n, fill = .), stat = "identity") +
  theme_bw() +
  labs(x = "Presença de perda", y = "Número de pacientes", title = "PEW APOS do RETP nos pacientes") + 
  scale_y_continuous(limits = c(0, 30)) +
  guides(fill = F)

grid.arrange(graf_pew_antes, graf_pew_apos, ncol = 2)

# Hipotese: O estudo teve efeito sobre PEW? 

# H0: Não teve diferença
# H1: Teve dirença

base_pew %>% select(PEW_ANTES, PEW_APOS) %>% table() %>% mcnemar.test()

#  Com base num nivel de significancia de 5% rejeitamos a hipotese nula, ou seja,
# há evidencias de que teve diferença entre as variaveis.


# Inflamacao --------------------------------------------------------------

base_inflamacao <- 
  list(
    TNFa = base %>% select(TNFa01, TNFa02) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "TNFa"),
    ICAM = base %>% select(ICAM1, ICAM2)   %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "ICAM"),
    PCR  = base %>% select(PCR01, PCR02)   %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "PCR"),
    IL6  = base %>% select(IL601, IL602)   %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "IL6"),
    VCAM = base %>% select(VCAM1, VCAM2)   %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "VCAM")
  ) %>% 
  map(~ .x %>% as_tibble)


# Análise descritiva ------------------------------------------------------

## Resumo dos marcadores de inflamação
base_inflamacao %>% map(~ summary(.x))

## Boxplot de cada marcador
base_inflamacao$ICAM %>% 
  select(-marcador) %>% 
  gather() %>% 
ggpaired(x = "key", y = "value", color = "key", ggtheme = theme_bw(),
         line.color = "gray", line.size = 0.4, palette = "jco") +
  labs(x = "ICAM", y = "Valor observado", title = "Boxplot TNFa por valor observado antes e depois") +
  guides(color = F) +
  scale_y_continuous(breaks = seq(0, 6000, 1000))


## Em geral, com base no gráfico, parece que houve uma redução no ICAM após o RETP.


# Normalidade -------------------------------------------------------------

## Normalidade: TNFa antes
shapiro.test(base_tnf$TNFa01)
## Com base num nivel de significancia de 5%, rejeitamos a hipótese de normalidade para a variavel.

## Normalidade: TNFa depois
shapiro.test(base_tnf$TNFa02)
## Com base num nivel de significancia de 5%, rejeitamos a hipótese de normalidade para a variavel.


# Tabela ------------------------------------------------------------------

base_aux <- 
  base_inflamacao %>% 
  map(~ .x %>% mutate_if(is.factor, as.character)) %>% 
  bind_rows() %>% 
  nest(-marcador) %>% 
  mutate(
    "p-valor" = map(
      data,
      ~ t.test(.x$Antes, .x$Depois, paired = T)
    )
  )

tabela_inflamacao <- 
  base_inflamacao %>% 
  map(~ .x %>% mutate_if(is.factor, as.character)) %>% 
  bind_rows() %>% 
  group_by(marcador) %>% 
  summarise(
    # Antes
    media_antes = mean(Antes) %>% round(1) %>% as.character(),
    desv_pad_antes = sd(Antes) %>% round(1) %>% as.character(),
    # Depois
    media_depois = mean(Depois) %>% round(1) %>% as.character(),
    desv_pad_depois = sd(Depois) %>% round(1) %>% as.character(),
    
    n = length(Antes)
  ) %>% 
  bind_cols(
    base_aux$`p-valor` %>% 
      map(~ .x$p.value %>% as.data.frame) %>% 
      bind_rows() %>% 
      round(3) %>% 
      `colnames<-`("p_valor") %>% 
      mutate(p_valor = ifelse(p_valor < 0.001, "<0.001", p_valor))
  )


# typology_tabela <- 
#   data.frame(
#     col_keys = c("marcador","media_antes","desv_pad_antes","media_depois","desv_pad_depois","n","p_valor"),
#     type = c("", "Antes", "Antes", "Depois", "Depois", "", ""),
#     what = c("Marcador \n Inflamatório","Média","Desvio Padrão","Média","Desvio Padrão","Tamanho da amostra","P-valor \n (Teste t)"),
#     stringsAsFactors = FALSE
#   )
# 
# tabela_inflamacao %>% 
#   map_at(2:6, ~ .x %>% str_replace_all("\\.", ",")) %>% 
#   as_tibble() %>% 
#   regulartable() %>% 
#   set_header_df(mapping = typology_tabela, key = "col_keys") %>% 
#   merge_h(part = "header") %>% 
#   merge_v(part = "header") %>% 
#   # theme_zebra() %>% 
#   theme_booktabs() %>% 
#   width(width = c(rep(1.1, 6), 0.8)) %>% 
#   align(align = "center", part = "all")
#   # set_header_labels(
#   #   marcador = "Marcador \n Inflamatório",
#   #   media_antes = "Média",
#   #   desv_pad_antes = "Desvio Padrão",
#   #   media_depois = "Média",
#   #   desv_pad_depois = "Desvio Padrão",
#   #   n = "Tamanho da amostra",
#   #   p_valor = "P-valor (Teste t)"
#   # )




  