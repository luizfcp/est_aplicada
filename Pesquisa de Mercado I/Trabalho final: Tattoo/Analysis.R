library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(magrittr)
library(purrr)
library(ggsci)


raw_data <- read_csv("basecorrigida.csv")


# Análise Inicial ---------------------------------------------------------

# sexo
p1 <-
  raw_data %>% select(4) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Sexo", y = "Total de pessoas", title = "Sexo dos respondentes") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf", "#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000", "#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# cidade
p2 <- 
  raw_data %>% select(5) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Cidade", y = "Total de pessoas", title = "Cidade onde mora") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# passatempo favorito
p3 <- 
  raw_data %>% select(6) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel,n), y = n)) + geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Passatempo", y = "Total de pessoas", title = "Passatempo favorito") + 
  coord_flip() + geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# estilo musical
p4 <- 
  raw_data %>% select(7) %>% `colnames<-`("variavel") %>% 
  mutate(variavel = str_to_title(variavel)) %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Estilo musical", y = "Total de pessoas", title = "Estilo musical preferido") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "plain"))

# ??????? razao emoçao
p5 <- 
  raw_data %>% select(8) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Sexo", y = "Total de pessoas", title = "Sentimento de razão ou emoção ao fazer a primeira tatuagem") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# # ??????? momento atual
# p6 <- 
#   raw_data %>% select(9) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
#   ggplot(aes(x = variavel, y = n)) + geom_bar(stat = 'identity') +
#   labs(x = "Sexo", y = "Total de pessoas", title = "Sexo dos respondentes") + 
#   geom_label(aes(label = n))

# motivação para a primeira tattoo
p7 <- 
  raw_data %>% select(10) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Motivação", y = "Total de pessoas", title = "Motivação para a primeira tatuagem") + 
  # coord_flip() + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

p7 + scale_x_discrete(labels = function(x){str_replace(x, " ", "\n")})

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
  ggplot(aes(x = reorder(variavel, n), y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Significado", y = "Total de pessoas", title = "Significado da sua primeira tatuagem") + 
  coord_flip() + geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# quantidade de tatuagem possui
p9 <- 
  raw_data %>% select(12) %>% `colnames<-`("variavel") %>% 
  # mutate(variavel = as.character(variavel)) %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="black") +
  # geom_point(size=5, color="black", fill=alpha("orange", 0.7), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Quantidade de tatuagem", y = "Total de pessoas", title = "Quantidade de tatuagens que possui") + 
  geom_label(aes(label = n), size = 8)+#, hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(0,32,2)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "plain"))

# p9+ geom_label(aes(label = "teste legal",x = Inf, y = Inf), vjust = 'inward', hjust = 'inward')

# lugar primeira tatuagem
p10 <- 
  raw_data %>% select(13) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Lugar do corpo da primeira tatuagem") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "plain"))

# idade primeira tatuagem
p11 <- 
  raw_data %>% select(14) %>% `colnames<-`("variavel") %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="black") +
  # geom_point(size=5, color="black", fill=alpha("orange", 0.7), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Idade", y = "Total de pessoas", title = "Idade que fez a primeira tatuagem") + 
  geom_label(aes(label = n), size = 8)+#, hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(14,32,2)) + theme_bw()

# ano primeira tatuagem
p12 <- 
  raw_data %>% select(15) %>% `colnames<-`("variavel") %>% 
  group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_segment(aes(x=variavel, xend=variavel, y=0, yend=n), color="black") +
  # geom_point(size=5, color="black", fill=alpha("orange", 0.7), alpha=0.7, shape=21, stroke=2) + 
  labs(x = "Ano", y = "Total de pessoas", title = "Ano que fez a primeira tatuagem") + 
  geom_label(aes(label = n), size = 8)+#, hjust = -0.3, vjust = -0.1) +
  scale_x_continuous(breaks = seq(2001,2019,1)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "plain"))

# custo
p13 <- 
  raw_data %>% select(16) %>% `colnames<-`("variavel") %>% 
  mutate(value = ifelse(variavel<500, "R$0 a R$499", 
                        ifelse(variavel<1000, "R$500 a R$999",
                               ifelse(variavel<2000, "R$1000 a R$1999", "R$2000 ou mais")
                               ))) %>% 
  mutate(value = factor(value, levels = c("R$0 a R$499","R$500 a R$999","R$1000 a R$1999","R$2000 ou mais"))) %>% 
  group_by(value) %>% count() %>% 
  ggplot(aes(x = value, y = n)) + 
  geom_bar(aes(fill = value, color = value), stat = 'identity') +
  labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Custo, em reais, da tatuagem mais cara") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# # ?????????????
# p14 <- 
#   raw_data %>% select(17) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
#   ggplot(aes(x = reorder(variavel, n), y = n)) + geom_bar(stat = 'identity') +
#   labs(x = "Lugar do corpo", y = "Total de pessoas", title = "Lugar do corpo da primeira tatuagem") + 
#   geom_label(aes(label = n))

# tatuagens com desenhos ou textos
p15 <- 
  raw_data %>% select(18) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = reorder(variavel, n), y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Preferência", y = "Total de pessoas", title = "Preferência entre tatuagem com desenhos ou textos") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# sofre discriminação
p16 <- 
  raw_data %>% select(19) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Total de pessoas que sofreu/sofre \ndiscriminação por ter tatuagem") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# preconceito no mercado
p17 <- 
  raw_data %>% select(20) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Total de pessoas que acredita ter preconceito no \nmercado de trabalho com pessoas que possuem tatuagem") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# cobriu ou penseou em cobrir alguma tattoo
p18 <- 
  raw_data %>% select(21) %>% `colnames<-`("variavel") %>% 
  mutate(value = ifelse(variavel=="Não", "Não", "Sim")) %>% 
  group_by(value) %>% count(variavel) %>% 
  arrange(n) %>% 
  ggplot(aes(x = value, y = n)) + 
  geom_bar(aes(fill = reorder(variavel, -n)), stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Cobriu ou pensou em cobrir alguma das suas tatuagens? \nQual o motivo?") + 
  geom_label(aes(label = paste(n, "-", variavel)), position = position_stack(vjust = 0.5)) +
  geom_label(aes(x = 2, y = 30, label = paste("Não:", 33, "\nSim:", sum(n)-33)), fill = "white") +
  theme(legend.position = "none") + scale_fill_jco() + theme_bw()

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
  theme(legend.position = "none") + scale_fill_jco() + theme_bw()

# preza pelo estudio
p20 <- 
  raw_data %>% select(23) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Total de pessoas que prezam pelo estúdio de tatuagem") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

# tamanho pelo preço
p21 <- 
  raw_data %>% select(24) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "Resposta", y = "Total de pessoas", title = "Escolha do tamanho da tatuagem pelo preço") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")

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
  theme(legend.position = "none") + scale_fill_jco() + theme_bw()

# tattoo de graça
p23 <- 
  raw_data %>% select(26) %>% `colnames<-`("variavel") %>% group_by(variavel) %>% count() %>% 
  ggplot(aes(x = variavel, y = n)) + 
  geom_bar(aes(fill = variavel, color = variavel), stat = 'identity') +
  labs(x = "", y = "Total de pessoas", title = "Total de pessoas que fariam mais tatuagens \ncaso fosse de graça") + 
  geom_label(aes(label = n)) +
  scale_fill_manual(values = alpha(c("#bfbfbf","#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf", "#bfbfbf","#bfbfbf"), 0.7)) +
  scale_color_manual(values = alpha(c("#000000","#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000", "#000000","#000000"), 0.8)) +
  theme_bw() +
  theme(legend.position = "none")


# Salvando
# p7,p8,
plots <- list(p1,p2,p3,p4,p5,p9,p10,p11,p12,p13,p15,p16,p17,p18,p19,p20,p21,p22,p23)
# plots <- mget(ls()[1:21])

walk2(plots, 1:length(plots),
      ~ ggsave(
        paste0("img/fig_", .y, ".png"),
        plot = .x +
          theme(legend.position = "none",
                text = element_text(size=20)), 
        dpi = "retina", device = "png", 
        width = 10, height = 8))



# sexo x qtd tattoo
wt <- raw_data %>% 
  select(`Qual seu sexo?`, `Quantas tatuagens você possui? (Responda apenas o número.)`) %>% 
  `colnames<-`(c("x", "y"))

wilcox.test(y~x, data = wt) # mediana diferente alpha = 5%

q1 <- 
  raw_data %>% select(`Qual seu sexo?`, `Quantas tatuagens você possui? (Responda apenas o número.)`) %>% 
  `colnames<-`(c("Sexo", "qtd")) %>% 
  ggplot(aes(x = Sexo, y = qtd)) + 
  geom_boxplot(aes(fill = Sexo), outlier.alpha = 0) +
  geom_point(aes(fill = Sexo), size = 3, shape = 21, position = position_jitterdodge()) +
  scale_y_continuous(breaks = seq(0, 32, 2)) +
  labs(y = "Quantidade de tatuagens", title = "Quantidade de tatuagens por Sexo") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_label(aes(x = 1, y = 26, label = paste("Wilcoxon teste para mediana \np-valor:", wilcox.test(y~x, data = wt)$p.value %>% round(3)))) + 
  scale_fill_jco() +
  theme(text = element_text(size=20))


ggsave(plot = q1, "img/qtd_sexo.png",
       dpi = "retina", device = "png", 
       width = 10, height = 8)



# o local que a pessoa fez a primeira tattoo influencia em fazer mais?

# ano da primeira, a partir de 2010 ou algum, fez mais tatto? relação a copa, jogador
# significado da primeira influencia em ter mais?
# custo ta primeira tatto influencia em ter mais?
# quem tem muita ou pouca se fosse de graça teria mais?




# intr breve -> objetivo  ->