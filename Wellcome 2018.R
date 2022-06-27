dt <-  wgm2018_dataset_crosstabs_all_countries_1_
dt_u <- filter(dt, WP5 == 194)

#Filtramos por las variables que vamos a trabajar
dt_u <- select(dt_u, Q24, Q25, Q26, Q11A, Q11B, Q11C, Q11D, Q11G, Q12, Q13, Q14A, Q14B, Q20, Q21, Q29, Q30)


#Registro los valores NA como NA
dt_u[ dt_u > 90] <- NA

str(dt_u) ##Vemos que los valores son nÃºmericos cuando deberian ser factores, esto atrae un problema hay que ver como pasarlas a factor sin transformar NA a un factor

##Primera visualizaciÃ³n grÃ¡fica de las variables

ggplot(dt_u) + geom_bar(aes(x = Q24, y = ..prop..), stat="count", fill = "red", alpha = 0.3) + theme_bw()
ggplot(dt_u) + geom_bar(aes(x = Q25, y = ..prop..), stat="count", fill = "purple", alpha = 0.3) + theme_bw()
ggplot(dt_u) + geom_bar(aes(x = Q26, y = ..prop..), stat="count", fill = "blue", alpha = 0.3) + theme_bw()
