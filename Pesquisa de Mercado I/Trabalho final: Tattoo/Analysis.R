library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(magrittr)


raw_data <- read_csv("basecorrigida.csv")


# Análise Inicial ---------------------------------------------------------

# sexo
raw_data %>% select(4) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Sexo", y = "Total de pessoas", title = "Sexo dos respondentes") + 
  geom_label(aes(label = n))

# cidade
raw_data %>% select(5) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Cidade", y = "Total de pessoas", title = "Cidade onde mora") + 
  geom_label(aes(label = n))

# passatempo favorito
raw_data %>% select(6) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel,n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Passatempo", y = "Total de pessoas", title = "Passatempo favorito") + 
  coord_flip() + geom_label(aes(label = n))

# estilo musical
raw_data %>% select(7) %>% `colnames<-`("variavel") %>% 
  mutate(variavel = str_to_title(variavel)) %>% 
  # mutate(
  #   variavel = case_when(
  #     str_detect(variavel, "Pop") ~ str_replace_all(variavel, "^Pop$", "Pop/Eletrônica"),
  #     str_detect(variavel, "Eletrônico") ~ str_replace_all(variavel, "Eletrônico", "Pop/Eletrônica"),
  #     str_detect(variavel, "Diversos") ~ str_replace_all(variavel, "Diversos", "Todos"),
  #     str_detect(variavel, "Sou Eclética") ~ str_replace_all(variavel, "Sou Eclética", "Todos"),
  #     TRUE ~ variavel
  #   )
  # ) %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Estilo musical", y = "Total de pessoas", title = "Estilo musical preferido") + 
  geom_label(aes(label = n))

# ??????? razao emoçao
raw_data %>% select(8) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Sexo", y = "Total de pessoas", title = "Sexo dos respondentes") + 
  geom_label(aes(label = n))

# ??????? momento atual
raw_data %>% select(9) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Sexo", y = "Total de pessoas", title = "Sexo dos respondentes") + 
  geom_label(aes(label = n))

# motivação para a primeira tattoo
raw_data %>% select(10) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Motivação", y = "Total de pessoas", title = "Motivação para a primeira tatuagem") + 
  coord_flip() + geom_label(aes(label = n))

# significado da sua primeira tatuagem
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
raw_data %>% select(12) %>% `colnames<-`("variavel") %>% 
  # mutate(variavel = as.character(variavel)) %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="orange", linetype="dotdash") +
  geom_point(size=5, color="black", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Quantidade de tatuagem", y = "Total de pessoas", title = "Quantidade de tatuagens que possui") + 
  geom_label(aes(label = n), hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(0,32,2))

# lugar primeira tatuagem
raw_data %>% select(13) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Lugar do corpo da primeira tatuagem") + 
  geom_label(aes(label = n))

# idade primeira tatuagem
raw_data %>% select(14) %>% `colnames<-`("variavel") %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="orange", linetype="dotdash") +
  geom_point(size=5, color="black", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Idade", y = "Total de pessoas", title = "Idade que fez a primeira tatuagem") + 
  geom_label(aes(label = n), hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(14,32,2))

# ano primeira tatuagem
raw_data %>% select(15) %>% `colnames<-`("variavel") %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="orange", linetype="dotdash") +
  geom_point(size=5, color="black", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Ano", y = "Total de pessoas", title = "Ano que fez a primeira tatuagem") + 
  geom_label(aes(label = n), hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(2001,2019,1))

# ???????? custo
raw_data %>% select(16) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Lugar do corpo da primeira tatuagem") + 
  geom_label(aes(label = n))

# ?????????????
raw_data %>% select(17) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Lugar do corpo da primeira tatuagem") + 
  geom_label(aes(label = n))

# tatuagens com desenhos ou textos
raw_data %>% select(18) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
  labs(x = "Preferência", y = "Total de pessoas", title = "Preferência entre tatuagem com desenhos ou textos") + 
  geom_label(aes(label = n))

# sofre discriminação
raw_data %>% select(19) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "", y = "Total de pessoas", title = "Total de pessoas que sofreu/sofre discriminação por ter tatuagem") + 
  geom_label(aes(label = n))

# preconceito no mercado
raw_data %>% select(20) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "", y = "Total de pessoas", title = "Total de pessoas que acredita ter preconceito no mercado de trabalho com pessoas que possuem tatuagem") + 
  geom_label(aes(label = n))




raw_data %>% select(26) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
  labs(x = "", y = "Total de pessoas", title = "Total de pessoas que acredita ter preconceito no mercado de trabalho com pessoas que possuem tatuagem") + 
  geom_label(aes(label = n))













# sexo x qtd tattoo

# o local que a pessoa fez a primeira tattoo influencia em fazer mais?

# ano da primeira, a partir de 2010 ou algum, fez mais tatto? relação a copa, jogador
# significado da primeira influencia em ter mais?
# custo ta primeira tatto influencia em ter mais?
# quem tem muita ou pouca se fosse de graça teria mais?




# intr breve -> objetivo  ->