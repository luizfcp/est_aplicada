
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
library(stringr)
library(broom)
library(pacotin)

# Bordas para as tabelas
border <- officer::fp_border(color = "black")

# Tamanhos globais para os gráficos
w <- 6.73
h <- 3.11*2


# Importacao base de dados
base <- 
  read_sav("../data/BancoTeseCris_09_07_2013.sav") %>% 
  mutate(
    Sexo = factor(Sexo, labels = c("Homem", "Mulher"))
  )

# Visualizacao sexo por idade
graf_idade_sexo <- 
  base %>% 
  select(Sexo, Idade) %>% 
  na.omit() %>% 
  ggplot(aes(x=Sexo, y=Idade, fill=Sexo)) + 
  geom_boxplot() +
  labs(x = "Sexo", title = "Boxplot Idade por Sexo") + 
  theme_bw() +
  scale_fill_manual(values = c("#4285F5", "#DB4438")) +
  guides(fill = F)

graf_idade_sexo + 
  ggsave(
    paste0("../man/figures/graf_idade_sexo.png"),
    dpi = "retina", width = w, height = h
  )


# PEW ---------------------------------------------------------------------

base_pew <- 
  base %>% 
  select(
    Sexo, Idade, 
    PEW_ANTES, PEW_APOS
  ) %>% 
  na.omit() %>% 
  # Perda no PEW (1 = teve perda)
  mutate(
    PEW_ANTES = factor(PEW_ANTES, labels = c("Sim", "Não")),
    PEW_APOS = factor(PEW_APOS, labels = c("Sim", "Não"))
  )

# Visualizacao PEW ANTES
graf_pew_antes <- 
  base_pew$PEW_ANTES %>% table() %>% as_tibble() %>% 
  ggplot() +
  geom_bar(aes(x=., y=n, fill = .), stat = "identity") +
  theme_bw() +
  labs(x = "Presença de perda", y = "Número de pacientes", title = "PEW ANTES do RETP nos pacientes") +
  scale_y_continuous(limits = c(0, 30)) +
  scale_fill_manual(values = c("#4285F5", "#DB4438")) +
  guides(fill = F)

# Visualizacao PEW APOS
graf_pew_apos <- 
  base_pew$PEW_APOS %>% table() %>% as_tibble() %>% 
  ggplot() +
  geom_bar(aes(x=., y=n, fill = .), stat = "identity") +
  theme_bw() +
  labs(x = "Presença de perda", y = "Número de pacientes", title = "PEW APOS o RETP nos pacientes") + 
  scale_y_continuous(limits = c(0, 30)) +
  scale_fill_manual(values = c("#4285F5", "#DB4438")) +
  guides(fill = F)

graf_pew <- grid.arrange(graf_pew_antes, graf_pew_apos, ncol = 2)

ggsave("../man/figures/pew.png", dpi = "retina", plot = graf_pew, width = w+1, height = h-0.8)


# Hipotese: O estudo teve efeito sobre PEW? 

# H0: Não teve diferença
# H1: Teve dirença

mc_test <- base_pew %>% select(PEW_ANTES, PEW_APOS) %>% table() %>% mcnemar.test()

#  Com base num nivel de significancia de 5% rejeitamos a hipotese nula, ou seja,
# há evidencias de que teve diferença entre as variaveis.


df_tabela_pew <- 
  base_pew %>% 
  cbind("marcador" = "PEW") %>% 
  select(marcador, PEW_ANTES, PEW_APOS) %>% 
  mutate_at(2:3, ~ if_else(.x == "Não", 0, 1)) %>% 
  group_by(marcador) %>% 
  summarise(
    # Antes
    # Sem presença de perda
    # Com presença de perda
    "sem_pres_de_perda_antes" = length(PEW_ANTES)-sum(PEW_ANTES),
    "com_pres_de_perda_antes" = sum(PEW_ANTES),
    # Depois
    "sem_pres_de_perda_apos" = length(PEW_APOS)-sum(PEW_APOS),
    "com_pres_de_perda_apos" = sum(PEW_APOS),
    
    n = length(PEW_ANTES)
  ) %>% 
  bind_cols(
    mc_test$p.value %>% 
      as.data.frame %>% 
      round(3) %>% 
      `colnames<-`("p_valor") %>% 
      mutate(p_valor = ifelse(p_valor < 0.001, "<0.001", p_valor) %>% str_replace("\\.", ","))
  )

typology_tabela <-
  data.frame(
    col_keys = c("marcador","sem_pres_de_perda_antes","com_pres_de_perda_antes","sem_pres_de_perda_apos","com_pres_de_perda_apos","n","p_valor"),
    type = c("", "Presença de perda \n antes do RETP", "Presença de perda \n antes do RETP", "Presença de perda \n depois do RETP", "Presença de perda \n depois do RETP", "", " "),
    what = c("Marcador","Apresentou perda","Não apresentou perda","Apresentou perda","Não apresentou perda","Tamanho da amostra","P-valor \n (Teste de McNemar)"),
    stringsAsFactors = FALSE
  )

tabela_pew <- 
  df_tabela_pew %>%
  map_at(2:6, ~ .x %>% str_replace_all("\\.", ",")) %>%
  as_tibble() %>%
  regulartable() %>%
  set_header_df(mapping = typology_tabela, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  merge_v(j = 1:3) %>% 
  width(width = c(1.1, rep(1.5, 4), 1.1, 1.5)) %>%
  align(align = "center", part = "all") %>%
  # color(part = "header", color = "white") %>%
  border(j = c(1,3,5,6), border.right = border) %>%
  border(j = c(1,3,5,6), i = 1:2, part = "header", border.right = border) %>% 
  border(i = 2, part = "header", border.bot = border) %>% 
  border(i = 1, part = "header", border.top = border)

# Salvando
save_html("tabela_pew", "../man/figures/tabela_pew.png")


# Bioquimicos --------------------------------------------------------------

base_bioquimicos <- 
  list(
    TNFa  = base %>% select(TNFa01, TNFa02)           %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "TNFa"),
    ICAM  = base %>% select(ICAM1, ICAM2)             %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "ICAM"),
    PCR   = base %>% select(PCR01, PCR02)             %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "PCR"),
    IL6   = base %>% select(IL601, IL602)             %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "IL6"),
    VCAM  = base %>% select(VCAM1, VCAM2)             %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "VCAM"),
    ALBUM = base %>% select(Albumina01, albumina02)   %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "Albumina")
  ) %>% 
  map(~ .x %>% as_tibble)


# Análise descritiva ------------------------------------------------------

## Resumo dos marcadores de inflamação
base_bioquimicos %>% map(~ summary(.x))

## Boxplot de cada marcador
base_bioquimicos %<>% 
  map(~ .x %>% mutate_if(is.factor, as.character)) %>% 
  bind_rows() %>% 
  nest(-marcador) %>% 
  mutate(
    boxplot = map2(
      data, marcador,
      ~ .x %>%
        gather() %>% 
        ggpaired(x = "key", y = "value", color = "key", ggtheme = theme_bw(),
                 line.color = "gray", line.size = 0.4, palette = "jco") +
        labs(x = .y, y = "Valor observado", 
             title = paste("Boxplot", .y, "por valor observado antes e depois")) +
        guides(color = F) +
        scale_color_manual(values = c("#800000", "royalblue"))
    )
  )

## Em geral, com base no gráfico, parece que houve uma redução no ICAM após o RETP.

# Salvando
walk2(base_bioquimicos$boxplot, base_bioquimicos$marcador,
      ~ ggsave(
        paste0("../man/figures/", .y, ".png"),
        plot = .x, dpi = "retina", width = w, height = h
      )
)


# Normalidade -------------------------------------------------------------

## Normalidade: TNFa antes
# shapiro.test(base_tnf$TNFa01)
## Com base num nivel de significancia de 5%, rejeitamos a hipótese de normalidade para a variavel.

## Normalidade: TNFa depois
# shapiro.test(base_tnf$TNFa02)
## Com base num nivel de significancia de 5%, rejeitamos a hipótese de normalidade para a variavel.


# Tabela ------------------------------------------------------------------

base_bioquimicos %<>% 
  mutate("p_valor" = map(data,~ t.test(.x$Antes, .x$Depois, paired = T)))

df_tabela_bioquimicos <- 
  base_bioquimicos %>% 
  select(marcador, data) %>% 
  unnest %>% 
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
    base_bioquimicos$p_valor %>% 
      map(~ .x$p.value %>% as.data.frame) %>% 
      bind_rows() %>% 
      round(3) %>% 
      `colnames<-`("p_valor") %>% 
      mutate(p_valor = ifelse(p_valor < 0.001, "<0.001", p_valor) %>% str_replace("\\.", ","))
  )

typology_tabela <-
  data.frame(
    col_keys = c("marcador","media_antes","desv_pad_antes","media_depois","desv_pad_depois","n","p_valor"),
    type = c("", "Antes do RETP", "Antes do RETP", "Depois do RETP", "Depois do RETP", "", " "),
    what = c("Marcador \n Inflamatorio","Media","Desvio Padrao","Media","Desvio Padrao","Tamanho da amostra","P-valor \n (Teste t)"),
    stringsAsFactors = FALSE
  )

tabela_bioquimicos <- 
  df_tabela_bioquimicos %>%
  map_at(2:6, ~ .x %>% str_replace_all("\\.", ",")) %>%
  as_tibble() %>%
  regulartable() %>%
  set_header_df(mapping = typology_tabela, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  merge_v(j = 1:3) %>% 
  width(width = c(rep(1.1, 6), 0.8)) %>%
  align(align = "center", part = "all") %>%
  # color(part = "header", color = "white") %>%
  border(j = c(1,3,5,6), border.right = border) %>%
  border(j = c(1,3,5,6), i = 1:2, part = "header", border.right = border) %>% 
  border(i = 2, part = "header", border.bot = border) %>% 
  border(i = 1, part = "header", border.top = border)

# Salvando
save_html("tabela_bioquimicos", "../man/figures/tabela_bioquimicos.png")


# tabela_bioquimicos %>% 
#   map_at(2:6, ~ .x %>% str_replace_all("\\.", ",")) %>%
#   as_tibble() %>%
#   regulartable() %>%
#   set_header_df(mapping = typology_tabela, key = "col_keys") %>%
#   merge_h(part = "header") %>%
#   merge_v(part = "header") %>%
#   merge_v(j = 1:3) %>%
#   theme_zebra(odd_header = "#800000", even_header = "#800000",
#               odd_body = "#FED9D9") %>%
#   # theme_booktabs() %>%
#   width(width = c(rep(1.1, 6), 0.8)) %>%
#   align(align = "center", part = "all") %>% 
#   color(part = "header", color = "white") %>% 
#   border(j = c(1,3,5,6), border.right = border) %>% 
#   border(j = c(1,3,5,6), i = 2, part = "header", border.right = border)
#   
#   # set_header_labels(
#   #   marcador = "Marcador \n Inflamatório",
#   #   media_antes = "Média",
#   #   desv_pad_antes = "Desvio Padrão",
#   #   media_depois = "Média",
#   #   desv_pad_depois = "Desvio Padrão",
#   #   n = "Tamanho da amostra",
#   #   p_valor = "P-valor (Teste t)"
#   # )


# Antropometricos ---------------------------------------------------------

base_antropometricos <- 
  list(
    IMC = base %>% select(IMC1, IMC2) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "IMC"),
    AMB = base %>% select(AMBc1, AMBc2) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "AMB"),
    # Massa gorda não foi possível encontrar no banco
    M_Magra  = base %>% select(Mas_Magra1, Mas_Magra2)   %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "Massa Magra")
  ) %>% 
  map(~ .x %>% as_tibble)

# Análise descritiva ------------------------------------------------------

## Resumo dos marcadores de inflamação
base_antropometricos %>% map(~ summary(.x))

## Boxplot de cada marcador
base_antropometricos %<>% 
  map(~ .x %>% mutate_if(is.factor, as.character)) %>% 
  bind_rows() %>% 
  nest(-marcador) %>% 
  mutate(
    boxplot = map2(
      data, marcador,
      ~ .x %>%
        gather() %>% 
        ggpaired(x = "key", y = "value", color = "key", ggtheme = theme_bw(),
                 line.color = "gray", line.size = 0.4, palette = "jco") +
        labs(x = .y, y = "Valor observado", 
             title = paste("Boxplot", .y, "por valor observado antes e depois")) +
        guides(color = F) +
        scale_color_manual(values = c("#800000", "royalblue"))
    )
  )


## Em geral, com base no gráfico, parece que houve uma redução no IMC após o RETP.

# Salvando
walk2(base_antropometricos$boxplot, base_antropometricos$marcador,
      ~ ggsave(
        paste0("../man/figures/", .y, ".png"),
        plot = .x, dpi = "retina", width = w, height = h
      )
)


# Normalidade -------------------------------------------------------------

## Normalidade: TNFa antes
# shapiro.test(base_tnf$TNFa01)
## Com base num nivel de significancia de 5%, rejeitamos a hipótese de normalidade para a variavel.

## Normalidade: TNFa depois
# shapiro.test(base_tnf$TNFa02)
## Com base num nivel de significancia de 5%, rejeitamos a hipótese de normalidade para a variavel.


# Tabela ------------------------------------------------------------------

base_antropometricos %<>% 
  mutate("p_valor" = map(data,~ t.test(.x$Antes, .x$Depois, paired = T)))

df_tabela_antropometricos <- 
  base_antropometricos %>% 
  select(marcador, data) %>% 
  unnest %>% 
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
    base_antropometricos$p_valor %>% 
      map(~ .x$p.value %>% as.data.frame) %>% 
      bind_rows() %>% 
      round(3) %>% 
      `colnames<-`("p_valor") %>% 
      mutate(p_valor = ifelse(p_valor < 0.001, "<0.001", p_valor))
  )

typology_tabela <-
  data.frame(
    col_keys = c("marcador","media_antes","desv_pad_antes","media_depois","desv_pad_depois","n","p_valor"),
    type = c("", "Antes do RETP", "Antes do RETP", "Depois do RETP", "Depois do RETP", "", " "),
    what = c("Marcador \n Antropometrico","Madia","Desvio Padrao","Madia","Desvio Padrao","Tamanho da amostra","P-valor \n (Teste t)"),
    stringsAsFactors = FALSE
  )

tabela_antropometricos <- 
  df_tabela_antropometricos %>%
  map_at(2:6, ~ .x %>% str_replace_all("\\.", ",")) %>%
  as_tibble() %>%
  regulartable() %>%
  set_header_df(mapping = typology_tabela, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  merge_v(j = 1:3) %>% 
  width(width = c(rep(1.1, 6), 0.8)) %>%
  align(align = "center", part = "all") %>%
  # color(part = "header", color = "white") %>%
  border(j = c(1,3,5,6), border.right = border) %>%
  border(j = c(1,3,5,6), i = 1:2, part = "header", border.right = border) %>% 
  border(i = 2, part = "header", border.bot = border) %>% 
  border(i = 1, part = "header", border.top = border)

# Salvando
save_html("tabela_antropometricos", "../man/figures/tabela_antropometricos.png")


# Capacidade Física -------------------------------------------------------

base_cf <- 
  list(
    SL10 = base %>% select(SL101, SL102) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "SL10"),
    SL60 = base %>% select(SL601, SL602) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "SL60"),
    torq_ext_E = base %>% select(TorqueextNmE1, TorqueextNmE2) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "Torque Extensor Esquerdo"),
    torq_ext_D = base %>% select(TorqueextNmD1, TorqueextNmD2) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "Torque Extensor Direito"),
    torq_flex_E = base %>% select(TorqueFLX.NmE01, TorqueFLX.NmE02) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "Torque Flexor Esquerdo"),
    torq_flex_D = base %>% select(TorqueFLX.NmD01, TorqueFLX.NmD02) %>% `colnames<-`(c("Antes", "Depois")) %>% na.omit() %>% cbind(marcador = "Torque Flexor Direito")
  ) %>% 
  map(~ .x %>% as_tibble)

# Análise descritiva ------------------------------------------------------

## Resumo dos marcadores de inflamação
base_cf %>% map(~ summary(.x))

## Boxplot de cada marcador
base_cf %<>% 
  map(~ .x %>% mutate_if(is.factor, as.character)) %>% 
  bind_rows() %>% 
  nest(-marcador) %>% 
  mutate(
    boxplot = map2(
      data, marcador,
      ~ .x %>%
        gather() %>% 
        ggpaired(x = "key", y = "value", color = "key", ggtheme = theme_bw(),
                 line.color = "gray", line.size = 0.4, palette = "jco") +
        labs(x = .y, y = "Valor observado", 
             title = paste("Boxplot", .y, "por valor observado antes e depois")) +
        guides(color = F) +
        scale_color_manual(values = c("#800000", "royalblue"))
    )
  )


## Em geral, com base no gráfico, parece que houve uma redução no IMC após o RETP.

# Salvando
walk2(base_cf$boxplot, base_cf$marcador,
      ~ ggsave(
        paste0("../man/figures/", .y, ".png"),
        plot = .x, dpi = "retina", width = w, height = h
      )
)


# Normalidade -------------------------------------------------------------

## Normalidade: TNFa antes
# shapiro.test(base_tnf$TNFa01)
## Com base num nivel de significancia de 5%, rejeitamos a hipótese de normalidade para a variavel.

## Normalidade: TNFa depois
# shapiro.test(base_tnf$TNFa02)
## Com base num nivel de significancia de 5%, rejeitamos a hipótese de normalidade para a variavel.


# Tabela ------------------------------------------------------------------

base_cf %<>% 
  mutate("p_valor" = map(data,~ t.test(.x$Antes, .x$Depois, paired = T)))

df_tabela_cf <- 
  base_cf %>% 
  select(marcador, data) %>% 
  unnest %>% 
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
    base_cf$p_valor %>% 
      map(~ .x$p.value %>% as.data.frame) %>% 
      bind_rows() %>% 
      round(3) %>% 
      `colnames<-`("p_valor") %>% 
      mutate(p_valor = ifelse(p_valor < 0.001, "<0.001", p_valor))
  )

typology_tabela <-
  data.frame(
    col_keys = c("marcador","media_antes","desv_pad_antes","media_depois","desv_pad_depois","n","p_valor"),
    type = c("", "Antes do RETP", "Antes do RETP", "Depois do RETP", "Depois do RETP", "", " "),
    what = c("Marcador \n Capac. Fisica","Media","Desvio Padrao","Media","Desvio Padrao","Tamanho da amostra","P-valor \n (Teste t)"),
    stringsAsFactors = FALSE
  )

tabela_cf <- 
  df_tabela_cf %>%
  map_at(2:6, ~ .x %>% str_replace_all("\\.", ",")) %>%
  as_tibble() %>%
  regulartable() %>%
  set_header_df(mapping = typology_tabela, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  merge_v(j = 1:3) %>% 
  width(width = c(rep(1.1, 6), 0.8)) %>%
  align(align = "center", part = "all") %>%
  # color(part = "header", color = "white") %>%
  border(j = c(1,3,5,6), border.right = border) %>%
  border(j = c(1,3,5,6), i = 1:2, part = "header", border.right = border) %>% 
  border(i = 2, part = "header", border.bot = border) %>% 
  border(i = 1, part = "header", border.top = border)

# Salvando
save_html("tabela_cf", "../man/figures/tabela_cf.png")


# Teste de normalidade nas variaveis --------------------------------------

base_bioquimicos %<>% select(marcador, data) %>% unnest()
base_antropometricos %<>% select(marcador, data) %>% unnest()
base_cf %<>% select(marcador, data) %>% unnest()

base_normalidade <- 
  base_bioquimicos %>% 
  bind_rows(base_antropometricos) %>% 
  bind_rows(base_cf) %>% 
  nest(-marcador) %>% 
  mutate(
    # Antes
    teste_normalidade_Antes = map(
      data,
      ~ .x %$% 
        Antes %>% 
        shapiro.test %>% 
        tidy
    ),
    # Depois
    teste_normalidade_Depois = map(
      data,
      ~ .x %$% 
        Depois %>% 
        shapiro.test %>% 
        tidy
    ),
    # Tamanho da amostra
    n = map(
      data,
      ~ .x %>% nrow()
    )
  )

tabela_normalidade <- 
  base_normalidade %>% 
  select(-data) %>% 
  unnest() %>% 
  select(marcador, p.value, p.value1, n) %>% 
  `colnames<-`(c("Marcador", "P-valor (Teste de Shapiro) \n Antes do RETP",  "P-valor (Teste de Shapiro) \n Depois do RETP", "Tamanho da amostra")) %>% 
  mutate_at(2:3, ~ .x %>% round(3) %>% ifelse(. < 0.001, "<0.001", .) %>% str_replace_all("\\.", ",")) %>% 
  regulartable() %>%
  align(align = "center", part = "all") %>%
  width(width = c(1.3, rep(1.8, 2), 1.1)) %>% 
  # color(part = "header", color = "white") %>%
  border(j = c(1:3), border.right = border) %>% 
  border(j = c(1:3), part = "header", border.right = border)

# Salvando
save_html("tabela_normalidade", "../man/figures/tabela_normalidade.png")
