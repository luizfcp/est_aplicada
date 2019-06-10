
library(ggplot2)

# calendario escolar - junho e julho --------------------------------------

dfr <- data.frame(date=seq(as.Date('2019-06-03'),as.Date('2019-07-31'),by=1))
dfr$day <- factor(strftime(dfr$date,format="%a"),levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
dfr$week <- factor(strftime(dfr$date,format="%V"))
dfr$month <- factor(strftime(dfr$date,format="%B"),levels=c("June","July","August"))
dfr$ddate <- factor(strftime(dfr$date,format="%d"))
head(dfr)

dfr$comment <- "Disponível"
dfr$comment[dfr$date>=as.Date('2019-06-03') & dfr$date<=as.Date('2019-07-22')] <- "Em aula"
dfr$comment[dfr$date>=as.Date('2019-07-23') & dfr$date<=as.Date('2019-07-31')] <- "Férias"
dfr$comment[dfr$date==as.Date('2019-06-14')] <- "Em prova"
dfr$comment[dfr$date==as.Date('2019-07-10')] <- "Em prova"
dfr$comment[dfr$date==as.Date('2019-07-15')] <- "Em prova"
dfr$comment[dfr$date==as.Date('2019-07-19')] <- "Em prova"
dfr$comment[dfr$day=="Sat" | dfr$day=="Sun"] <- "Fim de semana"

dfr$comment <- factor(dfr$comment,levels=c("Disponível","Em aula","Em prova", "Férias", "Fim de semana"))

ggplot(dfr,aes(x=week,y=day))+
  geom_tile(aes(fill=comment))+
  geom_text(aes(label=ddate))+
  scale_fill_manual(values=c("#8dd3c7","#ffffb3","#fb8072","#DAA520","#BC8F8F" ))+
  facet_grid(~month,scales="free",space="free")+
  labs(x="Semana",y="")+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        strip.background=element_blank(),
        legend.position="top",
        legend.justification="right",
        legend.direction="horizontal",
        legend.key.size=unit(0.3,"cm"),
        legend.spacing.x=unit(0.2,"cm"))

# ocorrencia de chuva -----------------------------------------------------

library(readr)
library(dplyr)

dados <- read_csv("dados.txt")

dados

chuva <- 
  dados %>% 
  group_by(data) %>% 
  summarise(precip_chuva = sum(precipitacao)) %>% 
  mutate(chuva = ifelse(precip_chuva!=0, 1, 0))

chuva$precip_chuva

# chuva %>% 
#   mutate(
#     day = factor(strftime(dfr$data, format="%a"),
#                  levels=rev(c("Wed","Thu","Fri","Sat","Sun","Mon","Tue"))),
#     week = factor(strftime(dfr$data, format="%V")),
#     month = factor(strftime(dfr$data, format="%B"),levels=c("May")),
#     dday = factor(strftime(dfr$data,format="%d"))
#   )

dfr <- data.frame(date=seq(as.Date('2019-05-01'),as.Date('2019-05-31'),by=1))
dfr$day <- factor(strftime(dfr$date,format="%a"),levels=rev(c("Wed","Thu","Fri","Sat","Sun","Mon","Tue")))
dfr$week <- factor(strftime(dfr$date,format="%V"))
dfr$month <- factor(strftime(dfr$date,format="%B"),levels=c("May"))
dfr$ddate <- factor(strftime(dfr$date,format="%d"))
head(dfr)

dfr$comment <- "Não Choveu"
dfr$comment[dfr$date==as.Date('2019-05-07') & dfr$date==as.Date('2019-05-08')] <- "Choveu"
dfr$comment[dfr$date==as.Date('2019-05-15') & dfr$date==as.Date('2019-05-17')] <- "Choveu"
dfr$comment[dfr$date==as.Date('2019-05-18') & dfr$date==as.Date('2019-05-19')] <- "Choveu"

dfr$comment <- factor(dfr$comment,levels=c("Não Choveu","Choveu"))

ggplot(dfr,aes(x=week,y=day))+
  geom_tile(aes(fill=comment))+
  geom_text(aes(label=ddate))+
  scale_fill_manual(values=c("#8dd3c7","#ffffb3"))+
  facet_grid(~month,scales="free",space="free")+
  labs(x="Semana",y="")+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        strip.background=element_blank(),
        legend.position="top",
        legend.justification="right",
        legend.direction="horizontal",
        legend.key.size=unit(0.3,"cm"),
        legend.spacing.x=unit(0.2,"cm"))

# referencia --------------------------------------------------------------

# http://www.roymfrancis.com/calendar-plot-with-ggplot2/

## Base chuvas
# http://www.inmet.gov.br/sonabra/pg_dspDadosCodigo_sim.php?QTYyNw==
