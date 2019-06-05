# library(readr)
# library(googleVis)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(lubridate)
# library(ggplot2)
# 
# read.delim2("series_historicas.xls", encoding="UTF-8", stringsAsFactors=FALSE) %>% 
#   select(-Brasil) %>% 
#   gather(date, value) %>% 
#   mutate(date = str_extract(date, "[:number:]{1,}") %>% as.numeric()) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x=date, y=value)) +
#   geom_line()