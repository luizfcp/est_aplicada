library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(magrittr)
library(purrr)


raw_data <- read_csv("basecorrigida.csv")


# Análise Inicial ---------------------------------------------------------

# sexo
p1 <- 
  raw_data %>% select(4) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Sexo", y = "Total de pessoas", title = "Sexo dos respondentes") + 
  geom_label(aes(label = n))

# cidade
p2 <- 
  raw_data %>% select(5) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Cidade", y = "Total de pessoas", title = "Cidade onde mora") + 
  geom_label(aes(label = n))

# passatempo favorito
p3 <- 
  raw_data %>% select(6) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel,n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Passatempo", y = "Total de pessoas", title = "Passatempo favorito") + 
  coord_flip() + geom_label(aes(label = n))

# estilo musical
p4 <- 
  raw_data %>% select(7) %>% `colnames<-`("variavel") %>% 
  mutate(variavel = str_to_title(variavel)) %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Estilo musical", y = "Total de pessoas", title = "Estilo musical preferido") + 
  geom_label(aes(label = n))

# ??????? razao emoçao
p5 <- 
  raw_data %>% select(8) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Sexo", y = "Total de pessoas", title = "Sentimento de razão ou emoção ao fazer a primeira tatuagem") + 
  geom_label(aes(label = n))

# # ??????? momento atual
# p6 <- 
#   raw_data %>% select(9) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
#   ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
#   labs(x = "Sexo", y = "Total de pessoas", title = "Sexo dos respondentes") + 
#   geom_label(aes(label = n))

# motivação para a primeira tattoo
p7 <- 
  raw_data %>% select(10) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Motivação", y = "Total de pessoas", title = "Motivação para a primeira tatuagem") + 
  coord_flip() + geom_label(aes(label = n))

# significado da sua primeira tatuagem
p8 <- 
  raw_data %>% select(11) %>% `colnames<-`("variavel") %>% 
  mutate(
    variavel = case_when(
      str_detect(variavel, "Meu time") ~ str_replace_all(variavel, "Meu time", "Futebol"),
      str_detect(variavel, "Esporte torcida organizada") ~ str_replace_all(variavel, "Esporte torcida organizada", "Futebol"),
      TRUE ~ variavel
      )
  ) %>%
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Significado", y = "Total de pessoas", title = "Significado da sua primeira tatuagem") + 
  coord_flip() + geom_label(aes(label = n))

# quantidade de tatuagem possui
p9 <- 
  raw_data %>% select(12) %>% `colnames<-`("variavel") %>% 
  # mutate(variavel = as.character(variavel)) %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="black") +
  geom_point(size=5, color="black", fill=alpha("orange", 0.7), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Quantidade de tatuagem", y = "Total de pessoas", title = "Quantidade de tatuagens que possui") + 
  geom_label(aes(label = n), hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(0,32,2))

# lugar primeira tatuagem
p10 <- 
  raw_data %>% select(13) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Lugar do corpo da primeira tatuagem") + 
  geom_label(aes(label = n))

# idade primeira tatuagem
p11 <- 
  raw_data %>% select(14) %>% `colnames<-`("variavel") %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="black") +
  geom_point(size=5, color="black", fill=alpha("orange", 0.7), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Idade", y = "Total de pessoas", title = "Idade que fez a primeira tatuagem") + 
  geom_label(aes(label = n), hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(14,32,2))

# ano primeira tatuagem
p12 <- 
  raw_data %>% select(15) %>% `colnames<-`("variavel") %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="black") +
  geom_point(size=5, color="black", fill=alpha("orange", 0.7), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Ano", y = "Total de pessoas", title = "Ano que fez a primeira tatuagem") + 
  geom_label(aes(label = n), hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(2001,2019,1))

# custo
p13 <- 
  raw_data %>% select(16) %>% `colnames<-`("variavel") %>% 
  mutate(value = ifelse(variavel<500, "R$0 a R$499", 
                        ifelse(variavel<1000, "R$500 a R$999",
                               ifelse(variavel<2000, "R$1000 a R$1999", "R$2000 ou mais")
                               ))) %>% 
  mutate(value = factor(value, levels = c("R$0 a R$499","R$500 a R$999","R$1000 a R$1999","R$2000 ou mais"))) %>% 
  group_by(value) %>% count() %>% 
  ggplot(aes(x = value, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Custo, em reais, da tatuagem mais cara") + 
  geom_label(aes(label = n))

# # ?????????????
# p14 <- 
#   raw_data %>% select(17) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
#   ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
#   labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Lugar do corpo da primeira tatuagem") + 
#   geom_label(aes(label = n))

# tatuagens com desenhos ou textos
p15 <- 
  raw_data %>% select(18) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Preferência", y = "Total de pessoas", title = "Preferência entre tatuagem com desenhos ou textos") + 
  geom_label(aes(label = n))

# sofre discriminação
p16 <- 
  raw_data %>% select(19) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Total de pessoas que sofreu/sofre discriminação por ter tatuagem") + 
  geom_label(aes(label = n))

# preconceito no mercado
p17 <- 
  raw_data %>% select(20) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Total de pessoas que acredita ter preconceito no mercado de trabalho com pessoas que possuem tatuagem") + 
  geom_label(aes(label = n))

# cobriu ou penseou em cobrir alguma tattoo
p18 <- 
  raw_data %>% select(21) %>% `colnames<-`("variavel") %>% 
  mutate(value = ifelse(variavel=="Não", "Não", "Sim")) %>% 
  group_by(value) %>% count(variavel) %>% 
  arrange(n) %>% 
  ggplot(aes(x = value, y = n)) + 
  geom_bar(aes(fill = reorder(variavel, -n)), stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Cobriu ou pensou em cobrir alguma das suas tatuagens? Qual o motivo?") + 
  geom_label(aes(label = paste(n, "-", variavel)), position = position_stack(vjust = 0.5)) +
  geom_label(aes(x = 2, y = 30, label = paste("Não:", 33, "\nSim:", sum(n)-33)), fill = "white") +
  theme(legend.position = "none")

# remover ou penseou em remover alguma tattoo
p19 <- 
  raw_data %>% select(22) %>% `colnames<-`("variavel") %>% 
  mutate(value = ifelse(variavel=="Não", "Não", "Sim")) %>% 
  group_by(value) %>% count(variavel) %>% 
  arrange(n) %>% 
  ggplot(aes(x = value, y = n)) + 
  geom_bar(aes(fill = reorder(variavel, -n)), stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Removeu ou pensou em remover alguma das suas tatuagens? Qual o motivo?") + 
  geom_label(aes(label = paste(n, "-", variavel)), position = position_stack(vjust = 0.5)) +
  geom_label(aes(x = 2, y = 30, label = paste("Não:", 40, "\nSim:", sum(n)-40)), fill = "white") +
  theme(legend.position = "none")

# preza pelo estudio
p20 <- 
  raw_data %>% select(23) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Total de pessoas que prezam pelo estúdio de tatuagem") + 
  geom_label(aes(label = n))

# tamanho pelo preço
p21 <- 
  raw_data %>% select(24) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Escolha do tamanho da tatuagem pelo preço") + 
  geom_label(aes(label = n))

# num de tattoo
p22 <- 
  raw_data %>% select(25) %>% `colnames<-`("variavel") %>% 
  mutate(value = ifelse(variavel=="Não", "Não", "Sim")) %>% 
  group_by(value) %>% count(variavel) %>% 
  arrange(n) %>% 
  ggplot(aes(x = value, y = n)) + 
  geom_bar(aes(fill = reorder(variavel, -n)), stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Importância em ter um número ímpar ou par de tatuagens") + 
  geom_label(aes(label = paste(n, "-", variavel)), position = position_stack(vjust = 0.5)) +
  geom_label(aes(x = 2, y = 30, label = paste("Não:", 43, "\nSim:", sum(n)-43)), fill = "white") +
  theme(legend.position = "none")

# tattoo de graça
p23 <- 
  raw_data %>% select(26) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "", y = "Total de pessoas", title = "Total de pessoas que fariam mais tatuagens caso fosse de graça") + 
  geom_label(aes(label = n)) 


# Salvando

plots <- mget(ls()[1:21])

walk(plots, ~ ggsave(
  paste0("img/", .x, ".png"),
  plot = .x, dpi = "retina"))




# sexo x qtd tattoo

# o local que a pessoa fez a primeira tattoo influencia em fazer mais?

# ano da primeira, a partir de 2010 ou algum, fez mais tatto? relação a copa, jogador
# significado da primeira influencia em ter mais?
# custo ta primeira tatto influencia em ter mais?
# quem tem muita ou pouca se fosse de graça teria mais?




# intr breve -> objetivo  ->