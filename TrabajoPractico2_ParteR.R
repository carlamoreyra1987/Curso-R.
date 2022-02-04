
library(readr)
library(tidyverse)
#Seteo el directorio de trabajo
setwd("~/Curso_R")
#Leo la base de datos a utilizar
EPH <- read_delim("EPH4T.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)
#Selecciono las variables a utilizar
EPH_SELECT  <- select(EPH, TRIMESTRE, AGLOMERADO, ESTADO, CH04, CH06, PONDERA, CAT_OCUP, CAT_INAC, INTENSI,PP03J, NIVEL_ED, PONDII, P47T, P21 )
aglomerado_GR <- (filter(EPH_SELECT, AGLOMERADO==4,))aglomerado_GP <- (filter(EPH_SELECT, AGLOMERADO==6,))
#calculo de tasas aglomerados Gran Rosario
Poblacion_ocupados_GR <- aglomerado_GR %>%
  summarise(poblacioN = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]))
Poblacion_ocupados_GR

Tasa_de_Actividad_GR <- aglomerado_GR %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = ocupados + desocupados,
            Tasa_Actividad_GR =    PEA/poblacion*100
  )
Tasa_de_Actividad_GR
  
Tasa_de_Empleo_GR <-aglomerado_GR %>%
  summarise(poblacion = sum(PONDERA),
            ocupados= sum(PONDERA[CAT_OCUP == 3]),
            Tasa_Empleo = ocupados/poblacion*100
                       )
Tasa_de_Empleo_GR

Tasa_de_Desocupacion_GR <-aglomerado_GR %>%
  summarise(poblacion = sum(PONDERA),
            ocupados= sum(PONDERA[ESTADO == 2]),
            Tasa_Desocupacion = ocupados/poblacion*100
           )
            
Tasa_de_Desocupacion_GR

Tasa_de_Subocupacion_GR <-aglomerado_GR %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J]),
            Tasa_Subocupacion_GR = Suboc_demandante + Suboc_no_demand,
            Tasa_subocupado_total = Tasa_Subocupacion_GR/PEA*100
           )
Tasa_de_Subocupacion_GR
#union de tablas de tasas (utilice este método para poder hacer un merge en el siguiente paso, para poder graficar correctamente)
tasas_1_GR <- select(Tasa_de_Desocupacion_GR, Tasa_Desocupacion)
tasas_2_GR <- select(Tasa_de_Subocupacion_GR, Tasa_subocupado_total)
tasas_3_GR <- select(Tasa_de_Empleo_GR,  Tasa_Empleo)
tasas_4_GR <- select(Tasa_de_Actividad_GR,  Tasa_Actividad_GR)
#merge para unir todas las variables en un solo dataset y poder graficar
Tasas_GR <- merge (tasas_1_GR, merge (tasas_2_GR, merge(tasas_3_GR, tasas_4_GR)))
library(base)
barplot(as.matrix(Tasas_GR), col = rainbow(3), ylim = c(0,50))

#calculo de tasas aglomerados Gran Paraná

Poblacion_ocupados_GP <- aglomerado_GP %>%
  summarise(poblacioN = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]))
Poblacion_ocupados_GP

Tasa_de_Actividad_GP <- aglomerado_GP %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = ocupados + desocupados,
            Tasa_Actividad_GP =    PEA/poblacion*100
  )
Tasa_de_Actividad_GP

Tasa_de_Empleo_GP <-aglomerado_GP %>%
  summarise(poblacion = sum(PONDERA),
            ocupados= sum(PONDERA[CAT_OCUP == 3]),
            Tasa_Empleo = ocupados/poblacion*100
  )
Tasa_de_Empleo_GP

Tasa_de_Desocupacion_GP <-aglomerado_GP %>%
  summarise(poblacion = sum(PONDERA),
            ocupados= sum(PONDERA[ESTADO == 2]),
            Tasa_Desocupacion = ocupados/poblacion*100
  )

Tasa_de_Desocupacion_GP

Tasa_de_Subocupacion_GP <-aglomerado_GP %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J]),
            Tasa_Subocupacion_GP = Suboc_demandante + Suboc_no_demand,
            Tasa_subocupado_total_GP = Tasa_Subocupacion_GP/PEA*100
  )
Tasa_de_Subocupacion_GP
#union de tablas de tasas (utilice este método para poder hacer un merge en el siguiente paso, para poder graficar correctamente)
tasas_1_GP <- select(Tasa_de_Desocupacion_GP, Tasa_Desocupacion)
tasas_2_GP <- select(Tasa_de_Subocupacion_GP,   Tasa_subocupado_total_GP )
tasas_3_GP <- select(Tasa_de_Empleo_GP,  Tasa_Empleo)
tasas_4_GP <- select(Tasa_de_Actividad_GP,  Tasa_Actividad_GP)
#merge para unir todas las variables en un solo dataset y poder graficar
Tasas_GP <- merge (tasas_1_GP, merge (tasas_2_GP, merge(tasas_3_GP, tasas_4_GP)))
library(base)
barplot(as.matrix(Tasas_GP), col = rainbow(3), ylim = c(0,50))

#tasas por 
Genero_F <- (filter(EPH_SELECT, CH04==2,))
Genero_M <- (filter(EPH_SELECT, CH04==1,))

#calculo de tasas por genero. Genero Femenino

Poblacion_ocupados_F <- Genero_F %>%
  summarise(poblacion = sum(PONDERA),
            ocupados_f = sum(PONDERA[ESTADO == 1 & CH04 == 2]))
Poblacion_ocupados_F
Tasa_de_Actividad_F <- Genero_F %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            Tasa_Actividad_F = ocupados/poblacion*100
  )

Tasa_de_Actividad_F <- Genero_F %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = ocupados + desocupados,
            Tasa_Actividad_F =    PEA/poblacion*100
  )
Tasa_de_Actividad_F


Tasa_de_Empleo_F <- Genero_F %>%
  summarise(poblacion = sum(PONDERA),
            ocupados= sum(PONDERA[CAT_OCUP == 3]),
            Tasa_Empleo_F = ocupados/poblacion*100
  )
Tasa_de_Empleo_F

Tasa_de_Desocupacion_F <-Genero_F %>%
  summarise(poblacion = sum(PONDERA),
         ocupados= sum(PONDERA[ESTADO == 2]),
        Tasa_Desocupacion_F = ocupados/poblacion*100
  )

Tasa_de_Desocupacion_F

Tasa_de_Subocupacion_F <-Genero_F %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J]),
            Tasa_Subocupacion_F = Suboc_demandante + Suboc_no_demand,
            Tasa_subocupado_total_F = Tasa_Subocupacion_F/PEA*100
  )
Tasa_de_Subocupacion_F

#union de tablas de tasas (utilice este método para poder hacer un merge en el siguiente paso, para poder graficar correctamente)
tasas_1_F <- select(Tasa_de_Desocupacion_F, Tasa_Desocupacion_F)
tasas_2_F <- select(Tasa_de_Subocupacion_F,  Tasa_subocupado_total_F)
tasas_3_F <- select(Tasa_de_Empleo_F,  Tasa_Empleo_F)
tasas_4_F <- select(Tasa_de_Actividad_F,  Tasa_Actividad_F)
#merge para unir todas las variables en un solo dataset y poder graficar
Tasas_femenino <- merge (tasas_1_F, merge (tasas_2_F, merge(tasas_3_F, tasas_4_F)))
library(base)
barplot(as.matrix(Tasas_femenino), col = rainbow(3), ylim = c(0,50))

#calculo de tasas por genero. Genero Masculino

Poblacion_ocupados_M <- Genero_M %>%
  summarise(poblacion = sum(PONDERA),
            ocupados_M = sum(PONDERA[ESTADO == 1 & CH04 == 1]))
Poblacion_ocupados_M

Tasa_de_Actividad_M<- Genero_M %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = ocupados + desocupados,
            Tasa_Actividad_M =    PEA/poblacion*100
  )

Tasa_de_Actividad_M

Tasa_de_Empleo_M <-Genero_M %>%
  summarise(poblacion = sum(PONDERA),
            ocupados= sum(PONDERA[CAT_OCUP == 3]),
            Tasa_Empleo_M = ocupados/poblacion*100
  )
Tasa_de_Empleo_M

Tasa_de_Desocupacion_M <-Genero_M %>%
  summarise(poblacion = sum(PONDERA),
            ocupados= sum(PONDERA[ESTADO == 2]),
            Tasa_Desocupacion_M = ocupados/poblacion*100
  )

Tasa_de_Desocupacion_M

Tasa_de_Subocupacion_M <-Genero_M %>%
  summarise(poblacion = sum(PONDERA),
            ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J]),
            Tasa_Subocupacion_M = Suboc_demandante + Suboc_no_demand,
            Tasa_subocupado_total_M = Tasa_Subocupacion_M/PEA*100
  )
Tasa_de_Subocupacion_M
  

#union de tablas de tasas (utilice este método para poder hacer un merge en el siguiente paso, para poder graficar correctamente)
tasas_1_M <- select(Tasa_de_Desocupacion_M, Tasa_Desocupacion_M)
tasas_2_M <- select(Tasa_de_Subocupacion_M,  Tasa_subocupado_total_M)
tasas_3_M <- select(Tasa_de_Empleo_M,  Tasa_Empleo_M)
tasas_4_M <- select(Tasa_de_Actividad_M,  Tasa_Actividad_M)
#merge para unir todas las variables en un solo dataset y poder graficar
Tasas_masculino <- merge (tasas_1_M, merge (tasas_2_M, merge(tasas_3_M, tasas_4_M)))
library(base)
barplot(as.matrix(Tasas_masculino), col = rainbow(3), ylim = c(0,100))

#categorizar la variable Edad
EPH_SELECT <- mutate(EPH_SELECT, CH06= case_when (
CH06 <= 30 ~ 1 ,
(CH06 > 30 & CH06 <= 64) ~ 2  ,
CH06 > 64 ~ 3 ,
TRUE ~ 0
))

EPH_SELECT$CH06 <- factor(EPH_SELECT$CH06, levels = c(1,2,3), labels = c("Menor de 30", "entre 30 y 64", "mayor de 64"),
                                   ordered = T)

#representacion de distribución de actividad
ACTIVIDAD_GENERAL <- table(EPH_SELECT$ESTADO)


#representación de distribución de condicion de actividad por género.
tabla_distribucion <- table(EPH_SELECT$ESTADO, EPH_SELECT$CH04)
chisq.test(tabla_distribucion) #no existe asociacion entre variables.

#representar distribucion por actividad y por grupo de edad

tabla_distribucion_edad_actividad <- table(EPH$ESTADO, EPH_SELECT$CH06)
chisq.test(tabla_distribucion_edad_actividad)

#representar distribucion nivel educativo por condicion de actividad

tabla_distribucion_educ_actividad <- table(EPH_SELECT$NIVEL_ED, EPH_SELECT$ESTADO)

#representar distribucion ingreso total individual a nivel general, POR GENERO Y POR EDAD

ingreso_individual <- EPH_SELECT%>%group_by(P47T)%>%summarise(cantidad = n())
ggplot(ingreso_individual, aes(x=P47T, fill=cantidad)) + geom_histogram(color="orange", alpha=0.8, bins = 15)

ingreso_porgenero <- EPH_SELECT%>%group_by(P47T, CH04)%>%summarise(cantidad = n())
ggplot(ingreso_porgenero, aes(x=P47T, fill=CH04)) + geom_histogram(color="orange", alpha=0.8, bins = 15)

ingreso_poredad <- EPH_SELECT%>%group_by(P47T, CH06)%>%summarise(cantidad = n())
ggplot(ingreso_poredad, aes(x=P47T, fill=CH06)) + geom_histogram(color="orange", alpha=0.8, bins = 15)


