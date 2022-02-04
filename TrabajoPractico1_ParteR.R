
library(readr)
library(tidyverse)
#Seteo el directorio de trabajo
setwd("~/Curso_R")
#Leo la base de datos a utilizar
bordet <- read_csv("Gustavo_Bordet.csv")
#Selecciono las variables a utilizar en mi base de datos limpia
bordet <- select(bordet, Name, Type, Permalink, Date, Shares, Comments,
                   "Reactions - Like", "Reactions - Love", "Reactions - Wow", "Reactions - Haha", "Reactions - Sad", "Reactions - Angry", "Reactions - Thankful", "Reactions - Pride" , "Reactions - Care")
#Renombro variables
bordet <- rename(bordet, Nombre="Name", Tipo="Type", LinkPermanente="Permalink", Fecha="Date", Compartidos="Shares", Comentarios="Comments", "Reacciones - Me_gusta" ="Reactions - Like", "Reacciones - Me_encanta" = "Reactions - Love", "Reacciones - Me_Fascina" = "Reactions - Wow", "Reacciones_MedaRisa" = "Reactions - Haha", "Reacciones_Triste" = "Reactions - Sad", "Reacciones_Meenoja" = "Reactions - Angry", "Reacciones_agradecido" = "Reactions - Thankful", "Reacciones_Orgullo" = "Reactions - Pride", "Reacciones_MePreocupa" = "Reactions - Care")
bordet <- rename(bordet, Megusta="Reacciones - Me_gusta", Meencanta="Reacciones - Me_encanta", Mefascina="Reacciones - Me_Fascina")
#Creación de variables
bordet <-mutate(bordet, Reacciones=Megusta+Meencanta+Mefascina+Reacciones_MedaRisa+Reacciones_Triste+Reacciones_Meenoja+Reacciones_agradecido+Reacciones_Orgullo+Reacciones_MePreocupa)
bordet <-mutate(bordet, Engagement=Reacciones+Comentarios+Compartidos)

#Conversión de fechas
#primero separo fecha y hora
bordet <- separate(bordet,Fecha, into = c("Fecha", "Hora"), sep = " ")
#Convierto la variable a formato fecha
class(bordet$Fecha)
bordet$Fecha <- ymd(bordet$Fecha)


#library(lubridate)
class(bordet$Fecha)

#exploración de la variable Engagement
resumen_bordet <- bordet %>% filter(Nombre=="Gustavo Bordet") %>%
  summarise(Eng_medio = mean(Engagement),
          Eng_median = median(Engagement),
        Eng_quantile = quantile(Engagement, probs=0.5))
resumen_bordet

#Categorización de Engagement
summary(bordet$Engagement)
bordet <- mutate(bordet, Eng_categ = if_else(Engagement<200,1,2))
bordet$Eng_categ <- as.factor(bordet$Eng_categ)
bordet <- mutate(bordet, Eng_categ = case_when(
  Engagement <= 200 ~ 1,
  Engagement > 200 ~ 2,
  TRUE ~  0
))
bordet$Eng_categ <- factor(bordet$Eng_categ, levels = c(1,2), labels = c("Bajo", "Alto"),
                            ordered = T)
#Exportar a Excel 1                  
library(writexl)
write_xlsx(bordet, "Carla_Moreyra_1.xlsx")

#obtención de un nuevo data set con posteos con Engagement Alto
bordet_Engagement_alto <- bordet
engagement_alto <- filter(bordet_Engagement_alto, Eng_categ=="Alto")


#group by tipo de post
group_by_post <- engagement_alto %>% group_by (Tipo) %>%
  summarise(Reacc_medio = mean(Reacciones),
            coment_medio = mean(Comentarios),
            Compart_medio = mean(Compartidos))

#segundo export de datos a archivo xlsx
library(writexl)
write_xlsx(group_by_post, "Carla_Moreyra_2.xlsx")
