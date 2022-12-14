
library(rvest)
library(dplyr)
library(purrr)
library(emoji)
library(kableExtra, quietly = TRUE)

bandera <- function(pais){
    dict_countries %>% filter(espanyol == pais) %>% pull(english) %>% flag()
}

dict_countries <- read.csv("https://gist.githubusercontent.com/ideaalab/7caf20bc8fd6c625163927e2d478e05f/raw/b49e20c16ed35e3295c0442d4b85a13eea645cee/paises.csv",encoding = "UTF-8") %>% setNames(c("espanyol", "english","iso2","iso3","phone_code"))





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

partidos_semis <- map_df(octavos[61:62],~ .x %>% select(c(2:4)) %>% slice(1))

equipos_semis <- unlist(partidos_semis[,c(1,3)]) %>% set_names(rep("",4)) %>% stringr::str_trim() %>% 
    stringr::str_replace("Estados Unidos", "EEUU")

partidos_final <- map_df(octavos[64],~ .x %>% select(c(2:4)) %>% slice(1))

equipos_final <- unlist(partidos_final[,c(1,3)]) %>% set_names(rep("",2)) %>% stringr::str_trim() %>% 
    stringr::str_replace("Estados Unidos", "EEUU")



sims <- rbind(expand.grid(equipos_semis[c(1,3)],equipos_semis[c(2,4)], stringsAsFactors = FALSE),
              expand.grid(equipos_semis[c(2,4)],equipos_semis[c(1,3)]), stringsAsFactors = FALSE)


porra <- readxl::read_excel("data/porra_Qatar.xlsx",
                            sheet = "apuestas",
                            range = "B1:Z32")

porra_list <- list(octavos = porra %>% slice(1:16),
                   cuartos = porra %>% slice(17:24),
                   semis = porra %>% slice(25:28),
                   final = porra %>% slice(29:30),
                   campeon = porra %>% slice(31))



equipos_octavos <- map_df(tablas, ~ .x %>% slice(1:2)) %>% 
    pull(1) %>% 
    stringr::str_remove("[A-Z]{3}") %>% 
    stringr::str_trim() %>% 
    stringr::str_replace("Estados Unidos", "EEUU")




puntos_octavos <- map_int(porra_list[["octavos"]], ~ .x %in% equipos_octavos %>% sum)
puntos_cuartos <- map_int(porra_list[["cuartos"]], ~ .x %in% equipos_cuartos %>% sum)*2
puntos_semis <- map_int(porra_list[["semis"]], ~ .x %in% equipos_semis %>% sum)*4
puntos_final <- map_int(porra_list[["final"]], ~ .x %in% equipos_final %>% sum)*6


puntos <- puntos_octavos + puntos_cuartos + puntos_semis


clas <-  data.frame(Puntos = puntos)

sims <- rbind(expand.grid(equipos_semis[c(1,3)],equipos_semis[c(2,4)], stringsAsFactors = FALSE),expand.grid(equipos_semis[c(2,4)],equipos_semis[c(1,3)], stringsAsFactors = FALSE)) %>% set_names(c("campeon",'finalista'))




sumas <- function(equipos_sim){
    puntos_final <- map_int(porra_list[["final"]], ~ .x %in% equipos_sim %>% sum)*6
    puntos_campeon <- map_int(porra_list[["campeon"]], ~.x == equipos_sim[[1]])*8
    clas_sims <- clas %>% mutate(puntos_final = puntos_final,
                                 puntos_campeon = puntos_campeon) %>% 
        mutate(PuntosTot = Puntos + puntos_final + puntos_campeon) %>% 
        arrange(desc(PuntosTot), desc(puntos_campeon), desc(puntos_final))
    
}

#sims_lista <- map(list(sims, simsf), ~ transpose(.x) %>% simplify_all())

sims_lista <- sims %>%  transpose %>% simplify_all

sims_lista_puntos <- map(sims_lista, sumas)

calcular_prob <- function(pos){
    probs = sims_lista %>%  keep(~ .x["campeon"] %in% equipos_final | .x["finalista"] %in% equipos_final) %>%  map(sumas) %>% map(~ .x %>% mutate(nombre = row.names(.x))) %>% map_dfr(slice,pos) %>% count(nombre) %>% mutate(prop = round(n/sum(n),4)*100) %>% select(nombre, prop)
} 

probs_lista <- map(list(1,2,3), calcular_prob) %>% set_names(c("1o","2o","3o"))

enpremios <- sims_lista %>%  keep(~ .x["campeon"] %in% equipos_final | .x["finalista"] %in% equipos_final) %>%  map(sumas) %>% map(~ .x %>% mutate(nombre = row.names(.x))) %>% map(slice,1:3) %>% map_df(~.x %>% group_by(nombre) %>% count() %>% ungroup()) %>% group_by(nombre) %>% count() %>% ungroup() %>% mutate(prop = round(n/4*100,2)) %>% select(nombre, prop)

clas <-  data.frame(Puntos = puntos) %>% mutate(nombre = row.names(.))

clas_prob <- c(list(clas), probs_lista, list(enpremios)) %>% reduce(left_join, by = "nombre") %>%  set_names("Puntos","nombre", "n1o", "n2o", "n3o", "En premios") %>% replace(is.na(.),0) %>% 
    mutate(across(n1o:`En premios`, ~ paste(.x,"%"), .names = "t{.col}")) %>% arrange(desc(Puntos)) %>% 
    tibble::column_to_rownames('nombre') 

puntuacion <- clas_prob  %>% 
    select(1,6:9) %>% 
    set_names(c("Puntos","1o","2o","3o","En premios")) %>% 
    kbl() %>% 
    kable_paper(full_width = T) %>% 
    column_spec(3, 
                color = spec_color(clas_prob$n1o, end = 0.5)) %>% 
    column_spec(4, 
                color = spec_color(clas_prob$n2o, end = 0.5)) %>% 
    column_spec(5, 
                color = spec_color(clas_prob$n3o, end = 0.5)) %>% 
    column_spec(6, 
                color = spec_color(clas_prob$`En premios`, end = 1)) %>% 
    footnote(general = paste("Actualizado a", Sys.time()))


