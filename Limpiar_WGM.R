library(magrittr)

#cargo WGM2018
base <- rio::import(here::here("wgm2018.xlsx"))

#Filtramos para Uruguay
wgm <- dplyr::filter(base, base$WP5 == "194")

#Nos quedamos con las variables de interés para el análisis y para ponderar
wgm <- dplyr::select(wgm, one_of(c("Q24", "Q25", "Q26", "Age",
                                   "Gender", "Education")))
names(wgm) <- c("conf1", "conf2", "conf3", "edad", "genero", "educacion")

#Borramos las observaciones en las que no tengo respuesta a las variables de interés
wgm <- wgm %>%
  dplyr::filter(!is.na(conf3))%>% #los elimino
  dplyr::filter(!conf1 %in% c("99","98"))%>%
  dplyr::filter(!conf2 %in% c("99","98"))%>%
  dplyr::filter(!conf3 %in% c("99","98"))%>%
  dplyr::filter(!edad<18)%>% 
  dplyr::filter(!is.na(educacion))
wgm$genero <- gsub("1", "0", wgm$genero) #Recodificamos la variable género para facilitar el análisis
wgm$genero <- gsub("2", "1", wgm$genero)

#Creamos variables dicotómicas
wgm$conf_general <- ifelse((wgm$conf1 == 1 | wgm$conf1 == 2) &
                                        (wgm$conf2 == 1 | wgm$conf2 == 2) &
                                        (wgm$conf3 == 1 | wgm$conf3 == 2), "1", "0")%>%
  as.numeric()

wgm$conf1_dummy <- ifelse(wgm$conf1 <= 2, 1, 0)
wgm$conf2_dummy <- ifelse(wgm$conf2 <= 2, 1, 0)
wgm$conf3_dummy <- ifelse(wgm$conf3 <= 2, 1, 0)

wgm$conf3_polar <- ifelse(wgm$conf3 %in% c(1,5), 1, 0)

library(writexl)
write_xlsx(wgm, "wgm_uy.xlsx") #Exportamos la base reducida
