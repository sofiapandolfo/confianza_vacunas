library(magrittr)

#Cargamos los datos
paie <- rio::import(here::here("Link Facebook todo .xlsx"))

#Limpiamos la base PAIE ---------------------------------
paie <- paie[-1,10:75]
names(paie) <- c(paste0("Q", 1:(length(paie))))

#Variable dependiente CONF
#conf1
paie$conf1 <- ifelse(paie$Q1 == "De acuerdo", paie$Q2,
                     ifelse(paie$Q1 == "En desacuerdo", paie$Q3, 
                            "Ni de acuerdo ni en desacuerdo"))
paie$conf1 <- ifelse(paie$conf1 == "Totalmente de acuerdo", "1",
              ifelse(paie$conf1 =="Algo de acuerdo", "2",
              ifelse(paie$conf1 == "Ni de acuerdo ni en desacuerdo", "3",
              ifelse(paie$conf1 == "Algo en desacuerdo", "4",
                               "5" ))))
paie$conf1 <- as.numeric(paie$conf1)

#conf2
paie$conf2 <- ifelse(paie$Q4 == "De acuerdo", paie$Q5,
                     ifelse(paie$Q4 == "En desacuerdo", paie$Q6, 
                            "Ni de acuerdo ni en desacuerdo"))
paie$conf2 <- ifelse(paie$conf2 == "Totalmente de acuerdo", "1",
                     ifelse(paie$conf2 =="Algo de acuerdo", "2",
                            ifelse(paie$conf2 == "Ni de acuerdo ni en desacuerdo", "3",
                                   ifelse(paie$conf2 == "Algo en desacuerdo", "4",
                                          "5" ))))
paie$conf2 <- as.numeric(paie$conf2)

#conf3
paie$conf3 <- ifelse(paie$Q7 == "De acuerdo", paie$Q8,
                     ifelse(paie$Q7 == "En desacuerdo", paie$Q9, 
                            "Ni de acuerdo ni en desacuerdo"))
paie$conf3 <- ifelse(paie$conf3 == "Totalmente de acuerdo", "1",
                     ifelse(paie$conf3 =="Algo de acuerdo", "2",
                            ifelse(paie$conf3 == "Ni de acuerdo ni en desacuerdo", "3",
                                   ifelse(paie$conf3 == "Algo en desacuerdo", "4",
                                          "5" ))))
paie$conf3 <- as.numeric(paie$conf3)

#conf_global
paie$conf_global <- ifelse((paie$conf1 == 1 | paie$conf1 == 2) &
                             (paie$conf2 == 1 | paie$conf2 == 2) &
                             (paie$conf3 == 1 | paie$conf3 == 2), "1", "0")
paie$conf_global <- as.numeric(paie$conf_global)

#reordenar preguntas de respuesta multiple
paie$vacunas2 <- tidyr::unite(paie [11:22], col = "vacunas2", sep = "\n", na.rm = TRUE)
paie$vacunas3 <- tidyr::unite(paie [23:34], col = "vacunas3", sep = "\n", na.rm = TRUE)
paie$efectos3 <- tidyr::unite(paie [41:46], col = "efectos3", sep = "\n", na.rm = TRUE)
paie$ocup <- tidyr::unite(paie [62:63], col = "ocup", sep = "\n", na.rm = TRUE)

paie <- paie[, -c(1:9, 11:34, 41:46, 62:63)]
paie <- paie[, c(26:29, 1, 30:31, 2:7, 32, 8:22, 33, 23:25)]
names(paie) <- c("conf1", "conf2", 
                "conf3","conf_global", "vacunas1","vacunas2", "vacunas3", 
                 "info1", "cuest_dummy", "cuest", "info2", "efectos1", 
                 "efectos2", "efectos3", "gob1", "gob2", "oblig", "c_vecinos",
                 "c_presidente", "c_msp", "c_cientificos", "c_medicos",
                 "c_oms", "c_inf", "c_redes","depto", "localidad", "barrio",
                 "educacion", "ocup", "genero", "edad", "voto")
sum(is.na(paie$edad)) #217 personas no terminaron la encuesta
paie <- paie %>%
  dplyr::filter(!is.na(edad)) #los elimino

paie$ID <- rownames(paie)
paie <- paie[,c(34, 1:33)]

#pasamos datos a numérico
paie$vacunas1 <- as.numeric(ifelse(paie$vacunas1 == "Ninguna dosis", "0",
                            ifelse(paie$vacunas1 == "Una dosis", "1",
                            ifelse(paie$vacunas1 == "Dos dosis", "2",
                            ifelse(paie$vacunas1 == "Tres dosis", "3",
                                                    "4")))))

paie$info1 <- as.numeric(ifelse(paie$info1 == "Sí", "1", 
                         ifelse(paie$info1 == "No", "0", "99")))

paie$cuest_dummy <- as.numeric(ifelse(paie$cuest_dummy == "Sí", "1", 
                               ifelse(paie$cuest_dummy == "No", "0", "99")))                     

paie$info2 <- as.numeric(ifelse(paie$info2 == "Sí, y me vacunaba", "1",
                         ifelse(paie$info2 == "Sí, y no me vacunaba", "2",
                         ifelse(paie$info2 == "No, y me vacunaba", "3", 
                                              "4"))))

paie$efectos1 <- as.numeric(ifelse(paie$efectos1 == "Sí", "1", 
                            ifelse(paie$efectos1 == "No", "0", "99")))  

paie$gob1 <- as.numeric(ifelse(paie$gob1 == "Sí", "1", 
                               ifelse(paie$gob1 == "No", "0", "99"))) 

paie$gob2 <- as.numeric(ifelse(paie$gob2 == "Mucho", "1",
                        ifelse(paie$gob2 == "Algo", "2",
                        ifelse(paie$gob2 == "Poco", "3",
                               "4"))))

paie$oblig <- as.numeric(ifelse(paie$oblig == "Sí", "1", 
                               ifelse(paie$oblig == "No", "0", "99"))) 


paie$c_vecinos <- as.numeric(ifelse(paie$c_vecinos == "Mucho", "1",
                             ifelse(paie$c_vecinos == "Algo", "2",
                             ifelse(paie$c_vecinos == "Poco", "3",
                                                      "4")))) 

paie$c_presidente <- as.numeric(ifelse(paie$c_presidente == "Mucho", "1",
                                ifelse(paie$c_presidente == "Algo", "2",
                                ifelse(paie$c_presidente == "Poco", "3",
                                                            "4")))) 

paie$c_msp <- as.numeric(ifelse(paie$c_msp == "Mucho", "1",
                         ifelse(paie$c_msp == "Algo", "2",
                         ifelse(paie$c_msp == "Poco", "3",
                                              "4")))) 
paie$c_cientificos <- as.numeric(ifelse(paie$c_cientificos == "Mucho", "1",
                                 ifelse(paie$c_cientificos == "Algo", "2",
                                 ifelse(paie$c_cientificos == "Poco", "3",
                                                              "4")))) 
paie$c_medicos <- as.numeric(ifelse(paie$c_medicos == "Mucho", "1",
                             ifelse(paie$c_medicos == "Algo", "2",
                             ifelse(paie$c_medicos == "Poco", "3",
                                                  "4"))))
paie$c_oms <- as.numeric(ifelse(paie$c_oms == "Mucho", "1",
                         ifelse(paie$c_oms == "Algo", "2",
                         ifelse(paie$c_oms == "Poco", "3",
                                              "4"))))

paie$c_inf <- as.numeric(ifelse(paie$c_inf == "Mucho", "1",
                         ifelse(paie$c_inf == "Algo", "2",
                         ifelse(paie$c_inf == "Poco", "3",
                                              "4"))))

paie$c_redes <- as.numeric(ifelse(paie$c_redes == "Mucho", "1",
                           ifelse(paie$c_redes == "Algo", "2",
                           ifelse(paie$c_redes == "Poco", "3",
                                                  "4")))) 

paie$depto_recod <- as.numeric(ifelse(paie$depto == "Montevideo", "1", "0"))

paie$edu_recod <- ifelse(paie$educacion %in% c("Primaria incompleto", 
                                                    "Primaria completo", 
                                                    "Ciclo básico incompleto", 
                                                    "Ciclo básico completo"),
                        "Hasta ciclo basico",
                        ifelse(paie$educacion %in% c("Bachillerato incompleto",
                                                     "Bachillerato completo"),
                         "Bachillerato", 
                         "Terciario"))

paie$edu_recod <- as.numeric(ifelse(paie$edu_recod == "Hasta ciclo basico", "1",
                             ifelse(paie$edu_recod == "Bachillerato", "2",
                                                      "3")))

paie$genero <- as.numeric(ifelse(paie$genero %in% c("Mujer", "Mujer trans"), "1",
                          ifelse(paie$genero %in% c("Varón", "Varón trans"), "0",
                                 "99")))

paie$edad <- paie$edad%>%
  gsub("años| |\\.", "", .)%>%
  gsub("8o", "80", .)%>%
  gsub("4p", "40", .)%>%
  gsub("-70", "70", .)%>%
  gsub("720", "72", .)%>%
  as.numeric()

paie <- paie %>%
  dplyr::filter(!is.na(edad))
paie <- dplyr::filter(paie, paie$edad >= 18)
paie <- dplyr::filter(paie, paie$genero != 99)

paie <- paie[, c(1:26, 36, 27:29, 35, 31:34)]

paie$conf1_dummy <- ifelse(paie$conf1 <= 2, 1, 0)
paie$conf2_dummy <- ifelse(paie$conf2 <= 2, 1, 0)
paie$conf3_dummy <- ifelse(paie$conf3 <= 2, 1, 0)
paie$conf3_polar <- ifelse(paie$conf3 %in% c(1,5), 1, 0)
paie$Vac <- ifelse(paie$vacunas1=="0",1,2)

library(writexl)
write_xlsx(paie, "paie_limpio.xlsx")

#Como la variable "cuestionamientos generados por las vacunas contra el COVID-19"
#presentaba 40 NA para realizar los modelos de probabilidad lineal 
#se genera una segunda base sin las observaciones que no tienen esta variable
paie_cuest <- paie%>%
  dplyr::filter(paie$cuest_dummy != 99)
write_xlsx(paie_cuest, "paie_cuest.xlsx")

#cargo WGM-------------------------------------------------
base <- rio::import(here::here("wgm2018.xlsx"))
wgm <- dplyr::filter(base, base$WP5 == "194") #Solo Uruguay

wgm <- dplyr::select(wgm, one_of(c("Q24", "Q25", "Q26", "Age",
                                   "Gender", "Education")))
names(wgm) <- c("conf1", "conf2", "conf3", "edad", "genero", "educacion")
wgm <- wgm %>%
  dplyr::filter(!is.na(conf3))%>% #los elimino
  dplyr::filter(!conf1 %in% c("99","98"))%>%
  dplyr::filter(!conf2 %in% c("99","98"))%>%
  dplyr::filter(!conf3 %in% c("99","98"))%>%
  dplyr::filter(!edad<18)%>%
  dplyr::filter(!is.na(educacion))
wgm$genero <- gsub("1", "0", wgm$genero)
wgm$genero <- gsub("2", "1", wgm$genero)

wgm$conf_general <- ifelse((wgm$conf1 == 1 | wgm$conf1 == 2) &
                                        (wgm$conf2 == 1 | wgm$conf2 == 2) &
                                        (wgm$conf3 == 1 | wgm$conf3 == 2), "1", "0")%>%
  as.numeric()

wgm$conf1_dummy <- ifelse(wgm$conf1 <= 2, 1, 0)
wgm$conf2_dummy <- ifelse(wgm$conf2 <= 2, 1, 0)
wgm$conf3_dummy <- ifelse(wgm$conf3 <= 2, 1, 0)

wgm$conf3_polar <- ifelse(wgm$conf3 %in% c(1,5), 1, 0)

library(writexl)
write_xlsx(wgm, "wgm_uy.xlsx")
