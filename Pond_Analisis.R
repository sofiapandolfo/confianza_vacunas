library(tidyverse)
library(anesrake)
library(srvyr)
library(paletteer)
library(ggplot2)
library(patchwork)

data <- rio::import("wgm_uy.xlsx")

data <- data %>%
  rename(sd_edad = edad,
         sd_sexo = genero) %>% 
  mutate(sexedad = case_when(
    sd_edad >= 18 & sd_edad <= 39  & sd_sexo == 0 ~ "Hombres_18_39",
    sd_edad >= 40 & sd_edad <= 59  & sd_sexo == 0 ~ "Hombres_40_59",
    sd_edad >= 60 & sd_edad <= 110 & sd_sexo == 0 ~ "Hombres_60_mas",
    sd_edad >= 18 & sd_edad <= 39  & sd_sexo == 1 ~ "Mujeres_18_39",
    sd_edad >= 40 & sd_edad <= 59  & sd_sexo == 1 ~ "Mujeres_40_59",
    sd_edad >= 60 & sd_edad <= 110 & sd_sexo == 1 ~ "Mujeres_60_mas",
  ))  %>% 
  mutate(sexedad = as.factor(sexedad)) %>% mutate(educacion = as.factor(educacion)) %>%
  mutate(id = row_number())

sexedad <- c(0.212, 0.156, 0.107,0.209,0.167,0.149)
educacion<- c(0.511, 0.297,0.191)
names(sexedad) <- c("Hombres_18_39", "Hombres_40_59", "Hombres_60_mas", 
                    "Mujeres_18_39", "Mujeres_40_59",  "Mujeres_60_mas")
names(educacion) <- c("1","2","3")

targets <- list(sexedad, educacion)
names(targets) <- c("sexedad", "educacion")
class(data)
data<-as.data.frame(data)
outsave <- anesrake(targets, # Parametros 
                    data, # Base
                    caseid = data$id,
                    cap = 5, # Lo trunco en 5 
                    pctlim = 0.05, # Diferencia de al menos 5
                    choosemethod = "total", 
                    verbose = FALSE, 
                    type = "nolim",
                    iterate = TRUE , 
                    force1 = TRUE)

# Pego ponderador a la base  
data <- data %>% 
  mutate(pond_sd = unlist(outsave[1]))

# Declarar como encuesta con srvyr (incluyendo ponderador)
svy <- as_survey_design(data, weights = pond_sd) 

###Empezamos a trabjar con la base del PAIE -------------------------------
##Ponderación
PAIE <- rio::import("paie_limpio.xlsx")

PAIE <- PAIE %>%
  rename(sd_edad = edad,
         sd_sexo = genero) %>% 
  mutate(sexedad = case_when(
    sd_edad >= 18 & sd_edad <= 24  & sd_sexo == 0 ~ "Hombres_18_24",
    sd_edad >= 25 & sd_edad <= 44  & sd_sexo == 0 ~ "Hombres_25_44",
    sd_edad >= 45 & sd_edad <= 64  & sd_sexo == 0 ~ "Hombres_45_64",
    sd_edad >= 65 & sd_sexo == 0 ~ "Hombres_65_mas",
    sd_edad >= 18 & sd_edad <= 24  & sd_sexo == 1 ~ "Mujeres_18_24",
    sd_edad >= 25 & sd_edad <= 44  & sd_sexo == 1 ~ "Mujeres_25_44",
    sd_edad >= 45 & sd_edad <= 64  & sd_sexo == 1 ~ "Mujeres_45_64",
    sd_edad >= 65  & sd_sexo == 1 ~ "Mujeres_65_mas",
  ))  %>% 
  mutate(sexedad = as.factor(sexedad)) %>% mutate(educacion = as.factor(edu_recod)) %>% 
  mutate(Vac = as.factor(Vac)) %>% mutate(depto_recod = as.factor(depto_recod))%>%
  mutate(id = row_number())

##Creamos los vectores para la ponderación
sexedad <- c(0.07, 0.18, 0.15, 0.08, 0.06 ,0.18, 0.16, 0.12)
names(sexedad) <- c("Hombres_18_24", "Hombres_25_44", "Hombres_45_64", "Hombres_65_mas", "Mujeres_18_24",
                    "Mujeres_25_44", "Mujeres_45_64", "Mujeres_65_mas")
depto_recod <- c(0.39,0.61)
names(depto_recod) <- c("1","0")
Vac <- c(0.08,0.92)
names(Vac) <- c("1","2")
educacion <- c(0.519,0.28, 0.202)
names(educacion) <- c("1", "2", "3")

targets <- list(sexedad, educacion, Vac, depto_recod)
names(targets) <- c("sexedad", "educacion", "Vac", "depto_recod")
class(PAIE)
PAIE <-as.data.frame(PAIE)
outsave_PAIE <- anesrake(targets, # Parametros 
                         PAIE, # Base
                         caseid = PAIE$id,
                         cap = 5, # Lo trunco en 5 
                         pctlim = 0.05, # Diferencia de al menos 5
                         choosemethod = "total", 
                         verbose = FALSE, 
                         type = "nolim",
                         iterate = TRUE , 
                         force1 = TRUE)

PAIE <- PAIE %>% 
  mutate(pond_sd = unlist(outsave_PAIE[1]))

# Declarar como encuesta con srvyr (incluyendo ponderador)
svy_PAIE <- as_survey_design(PAIE, weights = pond_sd) 

### COMPARACIÓN DE MEDIAS -----------------------------
#conf1
mean_1_2022 <- weighted.mean(x = PAIE$conf1_dummy, w = PAIE$pond_sd, na.rm = TRUE)
var_1_2022 <- mean_1_2022*(1-mean_1_2022)

mean_1_2018 <- weighted.mean(x = data$conf1_dummy, w = data$pond_sd, na.rm = TRUE)
var_1_2018 <- mean_1_2018*(1-mean_1_2018)

dif1 <- mean_1_2018 - mean_1_2022
IC_1_pos <- mean_1_2022 - mean_1_2018 + 1.96*sqrt(var_1_2022/nrow(PAIE) + var_1_2018/nrow(data))
IC_1_neg <- mean_1_2022 - mean_1_2018 - 1.96*sqrt(var_1_2022/nrow(PAIE) + var_1_2018/nrow(data))

#conf2
mean_2_2022 <- weighted.mean(x = PAIE$conf2_dummy, w = PAIE$pond_sd, na.rm = TRUE)
var_2_2022 <- mean_2_2022*(1-mean_2_2022)

mean_2_2018 <- weighted.mean(x = data$conf2_dummy, w = data$pond_sd, na.rm = TRUE)
var_2_2018 <- mean_2_2018*(1-mean_2_2018)

dif2 <- mean_2_2018 - mean_2_2022
IC_2_pos <- mean_2_2022 - mean_2_2018 + 1.96*sqrt(var_2_2022/nrow(PAIE) + var_2_2018/nrow(data))
IC_2_neg <- mean_2_2022 - mean_2_2018 - 1.96*sqrt(var_2_2022/nrow(PAIE) + var_2_2018/nrow(data))

#conf3
mean_3_2022 <- weighted.mean(x = PAIE$conf3_dummy, w = PAIE$pond_sd, na.rm = TRUE)
var_3_2022 <- mean_3_2022*(1-mean_3_2022)

mean_3_2018 <- weighted.mean(x = data$conf3_dummy, w = data$pond_sd, na.rm = TRUE)
var_3_2018 <- mean_3_2018*(1-mean_3_2018)

dif3 <- mean_3_2018 - mean_3_2022
IC_3_pos <- mean_3_2022 - mean_3_2018 + 1.96*sqrt(var_3_2022/nrow(PAIE) + var_3_2018/nrow(data))
IC_3_neg <- mean_3_2022 - mean_3_2018 - 1.96*sqrt(var_3_2022/nrow(PAIE) + var_3_2018/nrow(data))

#polarización en conf3
mean_3polar_2022 <- weighted.mean(x = PAIE$conf3_polar, w = PAIE$pond_sd, na.rm = TRUE)
var_3polar_2022 <- mean_3polar_2022*(1-mean_3polar_2022)

mean_3polar_2018 <- weighted.mean(x = data$conf3_polar, w = data$pond_sd, na.rm = TRUE)
var_3polar_2018 <- mean_3polar_2018*(1-mean_3polar_2018)

dif3polar <- mean_3polar_2018 - mean_3polar_2022
IC_3polar_pos <- mean_3polar_2022 - mean_3polar_2018 + 1.96*sqrt(var_3polar_2022/nrow(PAIE) + var_3polar_2018/nrow(data))
IC_3polar_neg <- mean_3polar_2022 - mean_3polar_2018 - 1.96*sqrt(var_3polar_2022/nrow(PAIE) + var_3polar_2018/nrow(data))


# VISUALIZACIONES  -----------------------------
#conf1
conf1dummy_2018 <- svy %>%     #2018
  group_by(conf1_dummy) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))
conf1dummy_2018$base <- "2018"

conf1dummy_2022 <- svy_PAIE %>%     #2022
  group_by(conf1_dummy) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))
conf1dummy_2022$base <- "2022"

dummy1 <- rbind(conf1dummy_2018, conf1dummy_2022)

ggplot(dummy1,
                aes(x = as.factor(base), y=per, fill = as.factor(conf1_dummy))) +
  geom_col() +
  theme_minimal() + 
  labs(fill = element_blank(),
       x = element_blank(),
       y = element_blank()) + 
  scale_fill_manual(labels = c("En desacuerdo \no Ni de acuerdo \nni en desacuerdo", "Acuerdo"), 
                    values=c("#FFBF69", "#82bdb9")) +
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.position = "none")
  ggtitle("Porcentaje de personas \nque acuerdan con que es importante \nque los niños se vacunen.")

#conf2
conf2dummy_2018 <- svy %>%     #2018
  group_by(conf2_dummy) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))
conf2dummy_2018$base <- "2018"

conf2dummy_2022 <- svy_PAIE %>%     #2022
  group_by(conf2_dummy) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))
conf2dummy_2022$base <- "2022"

dummy2 <- rbind(conf2dummy_2018, conf2dummy_2022)

ggplot(dummy2,
                aes(x = as.factor(base), y=per, fill = as.factor(conf2_dummy))) +
  geom_col() +
  theme_minimal() + 
  labs(fill = element_blank(),
       x = element_blank(),
       y = element_blank()) + 
  scale_fill_manual(labels = c("En desacuerdo \no Ni de acuerdo \nni en desacuerdo", "Acuerdo"), 
                    values=c("#FFBF69", "#82bdb9")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none")
  ggtitle("Porcentaje de personas \nque acuerdan con que las vacunas \nson seguras.")

#conf3
conf3dummy_2018 <- svy %>%     #2018
  group_by(conf3_dummy) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))
conf3dummy_2018$base <- "2018"

conf3dummy_2022 <- svy_PAIE %>%     #2022
  group_by(conf3_dummy) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))
conf3dummy_2022$base <- "2022"

dummy3 <- rbind(conf3dummy_2018, conf3dummy_2022)

ggplot(dummy3,
                aes(x = as.factor(base), y=per, fill = as.factor(conf3_dummy))) +
  geom_col() +
  theme_minimal() + 
  labs(fill = element_blank(),
       x = element_blank(),
       y = element_blank()) + 
  scale_fill_manual(labels = c("En desacuerdo \no Ni de acuerdo \nni en desacuerdo", "Acuerdo"), 
                    values=c("#FFBF69", "#82bdb9")) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none")
  ggtitle("Porcentaje de personas \nque acuerdan con que las vacunas \nson efectivas.")

#conf 3 polarización
conf3polar_2018 <- svy %>%     #2018
  group_by(conf3_polar) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))
conf3polar_2018$base <- "2018"

conf3polar_2022 <- svy_PAIE %>%     #2022
  group_by(conf3_polar) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))
conf3polar_2022$base <- "2022"

conf3polar <- rbind(conf3polar_2018, conf3polar_2022)

ggplot(conf3polar,
                    aes(x = as.factor(base), y=per, fill = as.factor(conf3_polar))) +
  geom_col() +
  theme_minimal() + 
  labs(fill = element_blank(),
       x = element_blank(),
       y = element_blank()) + 
  scale_fill_manual(labels = c("Opiniones moderadas", "Totalmente de acuerdo \n+ Totalmente en \ndesacuerdo "), 
                    values=c("#FFBF69", "#82bdb9")) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none")
  ggtitle("Porcentaje de personas \ncon opiniones extremas respecto \na la efectividad de las vacunas.")

#### Visualizaciones de variables relacionadas con la confianza global

#confianza científicos
cientificos_pond <- svy_PAIE %>% 
  group_by(conf_global, c_cientificos) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))

ggplot(cientificos_pond,
                    aes(x = factor(c_cientificos, level = c(4, 3, 2, 1)), y=per, fill = as.factor(conf_global))) +
  geom_col(position = "fill") +
  theme_minimal() + 
  labs(fill= "Confianza general \nen la vacunación", 
       x = "Confía en los científicos",
       y = element_blank()) + 
  scale_fill_manual(labels = c("Baja", "Alta"), values = c("#FFBF69", "#82bdb9"))+ 
  scale_x_discrete(labels = c("Nada", 
                              "Poco", 
                              "Algo",
                              "Mucho")) + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Confianza en los científicos según \nconfianza general en la vacunación (2022).")

#Cuestionamientos
cuest_pond <- svy_PAIE %>% 
  group_by(conf_global, cuest_dummy) %>% 
  summarise(frec_pond = sum(pond_sd)) %>%
  mutate(per = frec_pond / sum(frec_pond))

ggplot(cuest_pond,
       aes(x = as.factor(conf_global), y=per, fill = as.factor(cuest_dummy))) +
  geom_col(position = "fill") +
  theme_minimal() + 
  labs(fill = "Percepción de cuestionamientos \nsobre las vacunas generados \npor la discusión sobre vacunas \ncontra el COVID-19", 
       x = "Confianza general en la vacunación",
       y = element_blank()) + 
  scale_fill_manual(labels = c("No tiene nuevos cuestionamientos", 
                               "Tiene nuevos cuestionamientos", "No responde"), values=c("#FFBF69", "#82bdb9", "#517573"))+ 
  scale_x_discrete(labels = c("Baja", "Alta")) + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Percepción de cuestionamientos sobre las vacunas \ngenerados por la discusión sobre vacunas contra el COVID-19 \nsegún nivel de confianza general en la vacunación.")

# MODELOS DE PROBABILIDAD
# Usamos la base reducida, restando las 40 observaciones en las que "cuest_dummy" == NA
paie_cuest <- rio::import("paie_cuest.xlsx")

paie_cuest <- paie_cuest %>%
  rename(sd_edad = edad,
         sd_sexo = genero) %>% 
  mutate(sexedad = case_when(
    sd_edad >= 18 & sd_edad <= 24  & sd_sexo == 0 ~ "Hombres_18_24",
    sd_edad >= 25 & sd_edad <= 44  & sd_sexo == 0 ~ "Hombres_25_44",
    sd_edad >= 45 & sd_edad <= 64  & sd_sexo == 0 ~ "Hombres_45_64",
    sd_edad >= 65 & sd_sexo == 0 ~ "Hombres_65_mas",
    sd_edad >= 18 & sd_edad <= 24  & sd_sexo == 1 ~ "Mujeres_18_24",
    sd_edad >= 25 & sd_edad <= 44  & sd_sexo == 1 ~ "Mujeres_25_44",
    sd_edad >= 45 & sd_edad <= 64  & sd_sexo == 1 ~ "Mujeres_45_64",
    sd_edad >= 65  & sd_sexo == 1 ~ "Mujeres_65_mas",
  ))  %>% 
  mutate(sexedad = as.factor(sexedad)) %>% mutate(educacion = as.factor(edu_recod)) %>% 
  mutate(Vac = as.factor(Vac)) %>% mutate(depto_recod = as.factor(depto_recod))%>%
  mutate(id = row_number())

##Creamos los vectores para la ponderación
sexedad <- c(0.07, 0.18, 0.15, 0.08, 0.06 ,0.18, 0.16, 0.12)
names(sexedad) <- c("Hombres_18_24", "Hombres_25_44", "Hombres_45_64", "Hombres_65_mas", "Mujeres_18_24",
                    "Mujeres_25_44", "Mujeres_45_64", "Mujeres_65_mas")
depto_recod <- c(0.39,0.61)
names(depto_recod) <- c("1","0")
Vac <- c(0.08,0.92)
names(Vac) <- c("1","2")
educacion <- c(0.519,0.28, 0.202)
names(educacion) <- c("1", "2", "3")

targets <- list(sexedad, educacion, Vac, depto_recod)
names(targets) <- c("sexedad", "educacion", "Vac", "depto_recod")
class(paie_cuest)
paie_cuest <-as.data.frame(paie_cuest)
outsave_paie_cuest <- anesrake(targets, # Parametros 
                               paie_cuest, # Base
                         caseid = paie_cuest$id,
                         cap = 5, # Lo trunco en 5 
                         pctlim = 0.05, # Diferencia de al menos 5
                         choosemethod = "total", 
                         verbose = FALSE, 
                         type = "nolim",
                         iterate = TRUE , 
                         force1 = TRUE)

paie_cuest <- paie_cuest %>% 
  mutate(pond_sd = unlist(outsave_paie_cuest[1]))

# Declarar como encuesta con srvyr (incluyendo ponderador)
svy_paie2 <- as_survey_design(paie_cuest, weights = pond_sd) 

#Modelos de probabilidad lineal
reg <- svyglm(conf_global ~ info1 + depto_recod +
                sd_sexo + efectos1  + cuest_dummy + gob2 + c_msp  + as.numeric(sd_edad) +
                c_medicos + c_presidente + as.numeric(educacion) + c_cientificos +
                c_oms,
              design=svy_paie2, family=gaussian())
summary(reg)
y_hat <- fitted(reg)


reg1<- svyglm(conf1_dummy ~ conf3  + conf2 + c_cientificos  + depto_recod +
                sd_sexo  + efectos1 + gob2  + cuest_dummy + c_msp  + as.numeric(sd_edad)  +
                c_presidente + as.numeric(educacion) +
                c_oms, design=svy_paie2, family=gaussian())
summary(reg1)



reg2<- svyglm(conf2_dummy ~   depto_recod  + sd_sexo +  c_cientificos + 
                efectos1 + gob2  + cuest_dummy + c_msp  + as.numeric(sd_edad) + c_medicos + c_presidente + 
                as.numeric(educacion) +  c_oms ,
              design=svy_paie2, family=gaussian())
summary(reg2)


reg3<- svyglm(conf3_dummy ~  depto_recod + gob2 +
                sd_sexo +  c_cientificos + efectos1  + cuest_dummy + c_msp +  +
                as.numeric(sd_edad) + c_medicos + c_presidente + as.numeric(educacion) +
                c_oms,
              design=svy_paie2, family=gaussian())
summary(reg3)