
library(ggplot2)

# OBS: --------------------------------------------------------------------

#  Não estamos falando de mapa de calor georeferenciado e sim um mapa de calor 
# genérico, isto é, considerando um plano de projeção qualquer. No nosso exemplo 
# consideramos um plano cartesiano dado pelo cruzamento de duas variáveis categóricas 
# ordinais: dias da semana versus meses do ano.

# http://rpubs.com/LucianeA/Mapa_de_Calor

# -------------------------------------------------------------------------

# Gerando os dados e armazenando em df.
dt=seq(as.Date("2019-01-01"), as.Date("2019-12-31"),31)
mes=strftime(dt,format="%B")
eixoy <- factor(mes, levels = c(mes)) 
eixox <- c("seg", "ter", "qua", "qui", "sex") 
df <- data.frame(Y = eixoy, matrix(runif(60, 12, 30),nrow = length(eixoy), ncol = length(eixox)))
names(df)[2:(length(eixox)+1)] <- eixox
df

# Reorganizar o dataframe
require(reshape2)
df_heatmap <- melt(df, id.vars = "Y")
names(df_heatmap)[2:3] <- c("X", "Temperatura")
head(df_heatmap)

ggplot(df_heatmap, aes(X, Y)) +
  geom_tile(aes(fill = Temperatura), color = "white") +
  scale_fill_gradient(low = "white", high = "yellowgreen") +
  ylab("Meses") +
  xlab("Dia da Semana") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title = element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Temperatura")


# Exercícios --------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)

# 1)

dt=seq(as.Date("2019-01-01"), as.Date("2019-12-31"),31)
mes=strftime(dt,format="%B")

tibble(
   meses=factor(mes, levels = c(mes)),
  "2013"=c(241819,226442,251780,252433,257809,236713,241256,232019,229689,228200,213130,221300),
  "2014"=c(243962,234148,255859,255448,261667,242868,251391,238561,241299,230859,221993,235066),
  "2015"=c(250971,236110,273369,259960,264088,247914,247717,238888,244629,233283,222800,233240),
  "2016"=c(242824,237725,263755,253823,258810,242426,237487,228057,222093,211093,196938,208049),
  "2017"=c(223936,220800,264513,250096,266619,251839,245760,240659,233353,228288,222734,225869)
) %>% 
  gather(ano, value, -meses) %>% 
  ggplot(aes(x=ano, y=meses)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient(low = "white", high = "chocolate") +
  ylab("Meses") +
  xlab("Ano") +
  ggtitle("Número de Nascidos Vivos") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title = element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Nascidos Vivos")

# 3)

dias <- c("qua", "qui", "sex", "sab", "dom", "seg", "ter")

dados <- read_csv("chuva_maio.txt")

chuva <- 
  dados %>% 
  group_by(data) %>% 
  summarise(precip_chuva = sum(precipitacao))

chuva %>% 
  mutate(dia_semana = c(rep(dias, 4), dias[1:3]) %>% 
           factor(levels = c("dom", "seg", "ter", "qua", "qui", "sex", "sab")),
         semana = paste("Semana", c(rep(1,4), rep(2,7), rep(3,7), rep(4,7), rep(5,6)))) %>% 
  ggplot(aes(x=dia_semana, y=semana)) +
  geom_tile(aes(fill = precip_chuva), color = "white") +
  scale_fill_gradient(low = "white", high = "royalblue") +
  ylab("Semana") +
  xlab("Dia da Semana") +
  ggtitle("Nível de Precipitação no Mês de Maio de 2019 em Niterói") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title = element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Nível de Precipitação")















