dt <-  wgm2018_dataset_crosstabs_all_countries_1_
dt_u <- filter(dt, WP5 == 194)
dt_u <- select(dt_u, Q24, Q25, Q26, Q11A, Q11B, Q11C, Q11D, Q11G, Q12, Q13, Q14A, Q14B, Q20, Q21, Q29, Q30)


#Registro los valores NA como NA
dt_u$Q24[ dt_u$Q24 > 90] <- NA
dt_u$Q25[ dt_u$Q25 > 90] <- NA
dt_u$Q26[ dt_u$Q26 > 90] <- NA

##Primera visualización gráfica de las variables

ggplot(dt_u) + geom_bar(aes(x = Q24), fill = "red", alpha = 0.3) + theme_bw()
ggplot(dt_u) + geom_bar(aes(x = Q25), fill = "purple", alpha = 0.3) + theme_bw()
ggplot(dt_u) + geom_bar(aes(x = Q25), fill = "blue", alpha = 0.3) + theme_bw()

