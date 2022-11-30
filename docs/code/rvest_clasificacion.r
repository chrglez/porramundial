
library(rvest)
library(dplyr)
library(purrr)


contenido <- rvest::read_html("https://es.wikipedia.org/wiki/Copa_Mundial_de_F%C3%BAtbol_de_2022")

tablas <- contenido %>%
html_elements('.noprint.AP.rellink + table') %>%
html_table()
