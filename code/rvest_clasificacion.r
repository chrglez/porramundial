
library(rvest)
library(dplyr)
library(purrr)

dict_countries <- read.csv("https://gist.githubusercontent.com/ideaalab/7caf20bc8fd6c625163927e2d478e05f/raw/b49e20c16ed35e3295c0442d4b85a13eea645cee/paises.csv",encoding = "UTF-8")


contenido <- rvest::read_html("https://es.wikipedia.org/wiki/Copa_Mundial_de_F%C3%BAtbol_de_2022")

tablas_a <- contenido %>%
html_elements('.noprint.AP.rellink + table') %>%
html_table()

tablas_b <- contenido %>%
    html_elements('.thumb.tright + table') %>%
    html_table()

tablas <- c(tablas_b, tablas_a)

octavos <- contenido %>%
    html_elements('.vevent + table')  %>%
    html_table()

partidos_octavos <- map_df(octavos[49:56],~ .x %>% select(c(2:4)) %>% slice(1))

partidos_cuartos <- map_df(octavos[57:60],~ .x %>% select(c(2:4)) %>% slice(1))

equipos_cuartos <- unlist(partidos_cuartos[,c(1,3)]) %>% set_names(rep("",8)) %>% stringr::str_trim() %>% 
    stringr::str_replace("Estados Unidos", "EEUU")

partidos_semis <- map_df(octavos[61:62],~ .x %>% select(c(2,4)) %>% slice(1))
