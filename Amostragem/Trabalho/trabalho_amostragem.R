
library(readr)
library(readxl)
library(dplyr)

munic = read.csv2("dados_municipioV.csv")
dados = read.csv2("dados_renda.csv")

base = inner_join(munic, dados)


N = nrow(base)
N

D = (0.05)^2/(qnorm(0.975))^2
D

base1 = base %>% filter(renda>=1500)
p = nrow(base1)/nrow(base) 

n = N/( ( (N-1)*D/p*(1-p) ) + 1 )
n = 675



# algoritmo hajek ---------------------------------------------------------

gabrielmiranda = runif(5000,0,1)

x = cbind(base, gabrielmiranda) %>% arrange(gabrielmiranda) %>% .[1:n,]
x %>% View


#   -----------------------------------------------------------------------]

x1 = x %>% filter(renda>=1500)

phat = nrow(x1)/n


# ---------------------------------------------------------
VarHat = (1-n/N)*( phat*(1-phat)/(n-1)  )






#QUESTAO 2 ---------------------------------------------------------------------------------

phat

zphat = qnorm(0.975)*sqrt( (1-n/N)*( (phat*(1-phat))/(n-1) ) )

ic = paste("IC(p, 0.95%):", "[", phat-zphat %>% round(4), ";", phat+zphat %>% round(4), "]")
ic

phat


# Questao 3 ---------------------------------------------------------------

h = base %>% select(localidade) %>% distinct() %>% nrow()

h1 = base %>% filter(localidade=="A") 
h2 = base %>% filter(localidade=="B") 
h3 = base %>% filter(localidade=="C") 

sum(nrow(h1), nrow(h2), nrow(h3))

#alt+shift+seta pra baixo

x1 = cbind(h1, "Hajek"=runif(nrow(h1),0,1)) %>% arrange(Hajek) %>% .[1:nrow(h1),]


N1 = nrow(h1)
N2 = nrow(h2)
N3 = nrow(h3)


W1 = N1/N
W1 = N1/N
W1 = N1/N













