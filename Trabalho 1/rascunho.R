
library(readxl)
library(ggplot2)
library(dplyr)

base <- read_excel("IPCA15_marco2019.xlsx",  skip = 2, n_max = 10)

base %>% 
  ggplot(aes(x = `Índice geral e grupos de produtos e serviços`, y = )) +
  geom_bar(stat = "identity")

# barplot(x = base$`Índice geral e grupos de produtos e serviços`, y = base$`Variação mensal (%)`)

paises <- c("França", "Alemanha", "Áustria-Hungria", "Rússia", "Grã-Bretanha", "Itália", "Império Otomano", "Sérvia")
mortos <- c(1400000, 2000000, 1400000, 2000000, 960000, 600000, 800000, 130000)

tibble(paises, mortos) %>% 
  ggplot(aes(x = paises, y = mortos)) +
  geom_bar(stat = 'identity') +
  theme_bw()
