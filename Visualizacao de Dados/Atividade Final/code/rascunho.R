series <- 
  base_gr %>% 
  map_dfr(~.x %>% 
        gather(meses, value, -ano, -GR) %>% 
        transmute(data = paste("01",meses,ano, sep="/") %>% lubridate::dmy(), value, GR)
      )
base_gr[[1]]$GR

teste <- 
base_gr %>% 
  map_dfr(~.x %>% 
            gather(meses, value, -ano, -GR)
  )


teste %>% 
  spread(meses, value) %>% 
  select(-one_of(c("GR", "ano"))) %>% 
  GGally::ggpairs()



  
# Graficos de series usando facetwrap ou cores

# usando series


series %>% spread(regioes, value, -data)

