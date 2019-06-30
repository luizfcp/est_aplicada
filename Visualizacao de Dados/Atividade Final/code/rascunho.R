series <- 
  base_gr %>% 
  map_dfr(~.x %>% 
        gather(meses, value, -ano, -GR) %>% 
        transmute(data = paste("01",meses,ano, sep="/") %>% lubridate::dmy(), value, GR)
      )

base_gr %>% 
  nest(-GR) %$% 
  data %>% 
  .[[1]] %>% 
  gather(meses, value, -ano) %>% 
  transmute(data = paste("01",meses,ano, sep="/") %>% lubridate::dmy(), value)


teste %>% 
  spread(meses, value) %>% 
  select(-one_of(c("GR", "ano"))) %>% 
  GGally::ggpairs()

teste %>% 
  ggplot(aes(x=data, y=value)) +
  geom_line()


  
# Graficos de series usando facetwrap ou cores

# usando series


base_gr %>% 
  gather(meses, value, -ano, -GR) %>% 
  group_by(GR, meses) %>% 
  summarise(soma = sum(value)) %>% 
  ungroup() %>% 
  spread(GR, soma) %>% 
  select(-meses) %>% 
  ggpairs()
  
  
base_gr %>% 
  # select(-ano) %>% 
  gather(meses, value, -ano, -GR) %>% 
  # select(-meses) %>% 
  spread(GR, value) %>% 
  select(-meses) %>% 
  tail(36) %>% 
  mutate(ano = as.character(ano)) %>% 
  # group_by(ano, GR) %>% 
  # summarise(soma = sum(value)) %>% 
  # spread(GR, soma) %>% 
  ggpairs(
    columns = 2:6, mapping = aes(color = ano),
    lower = list( continuous = wrap(ggally_smooth_loess, size = 1, color = "darkblue")), 
    diag = list(continuous=wrap(ggally_barDiag, color="darkblue")), 
    upper = list( continuous = wrap("cor", size = 4.0, alignPercent = 1) )
  ) + 
  theme(panel.background = ggplot2::element_rect(fill = "lightgray")) 
  
  
    
library("GGally") 
library("ggplot2") 
data(iris) 
colnames(iris)=c("Medida1", "Medida2", "Medida3", "Medida4", "Tipo") 
levels(iris$Tipo)<-c("Tipo1", "Tipo2", "Tipo3") 
capa=ggpairs( iris, 1:4, mapping = aes(color = Tipo), 
              lower = list( continuous = wrap(ggally_smooth_loess, size = 1, color = "darkblue")), 
              diag = list(continuous=wrap(ggally_barDiag, color="darkblue")), 
              upper = list( continuous = wrap("cor", size = 4.0, alignPercent = 1) ) ) 
capa+ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightgray")) 
  
  
  
