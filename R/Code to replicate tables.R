
#--------------------------------------------------------------------------------
# Tema:       Generar rw y deff
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      agosto 2022
# Datos:      Muestra censal 2020, INEGI.
#             https://www.inegi.org.mx/programas/ccpv/2020/
     

# Contenido

#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#           3.1 Etiquetar variables
#           3.2 Etiquetar datos
#       4. Graficas (ggplot)
#           4.1 Con una variable discreta
#           4.2 Con dos variables discretas
#           4.3 Con una variable continua
#           4.4 Una variable continua y una discreta

#--------------------------------------------------------------------------------


# 1. Cargar librerias


rm(list=ls()); graphics.off(); options(warn=-1)              
paquetes=c("foreign", "stats", "tidyverse", "data.table", "data.table", "dplyr", "base", "dplyr", "ggplot2", "PracTools")
for (i in paquetes) {if (!require(i, character.only = TRUE)) {install.packages(i);library(i, character.only = TRUE)} else {library(i, character.only = TRUE)}}


datos="C:/Users/jcmartinez/OneDrive - El Colegio de México A.C/1 Proyectos/2023/66 Census/1 Datos/INEGI/Censo2020/CVS"
resultados="C:/Users/jcmartinez/OneDrive - El Colegio de México A.C/1 Proyectos/2023/66 Census/3 Resultados"


setwd(datos)

vivienda=read_csv("Viviendas00.CSV")
personas=read_csv("Personas00.CSV")

censo=personas %>% select("ID_VIV","ID_PERSONA","ENT", "ESTRATO", "UPM", "FACTOR", "SEXO", "EDAD", "AFRODES", "DIS_VER", "HLENGUA", "NIVACAD", "CONACT", "SERVICIO_MEDICO")




library(ggplot2)

# Create the boxplot
ggplot(viv, aes(x=COBERTURA, y=FACTOR)) + 
  geom_boxplot() +
  labs(title = "Boxplot of FACTOR by COBERTURA", 
       x = "COBERTURA", 
       y = "FACTOR")

viviendas_sample <- vivienda %>%
  sample_n(10000) %>%
  select(COBERTURA, FACTOR)
setwd(resultados)

write.csv(viviendas_sample, "viviendas_sample.csv")

  max(vivienda$FACTOR)

# Table 1: Overview of the Distribution of Primary Sampling Units (PSU) and Number of Households per PSU

#--------------------------------------------------------------------------------------------------------------------------
  
# Table 1. Summary of the distribution of the PSU and observations in the census sample

upm <- vivienda %>%
  group_by(ESTRATO) %>%
  summarise(n_upm = n_distinct(UPM), .groups = "drop") %>%
  summarise(
    min = min(n_upm, na.rm = TRUE),
    max = max(n_upm, na.rm = TRUE),
    mean = round(mean(n_upm, na.rm = TRUE),0),
    median = round(median(n_upm, na.rm = TRUE),0)
  ) %>%
  mutate(group = "PSU in Strata")%>%
  select(group, min, max, mean, median)

upm2 <- vivienda %>%
  group_by(UPM) %>%
  summarise(n_upm = n_distinct(ID_VIV), .groups = "drop") %>%
  summarise(
    min = min(n_upm, na.rm = TRUE),
    max = max(n_upm, na.rm = TRUE),
    mean = round(mean(n_upm, na.rm = TRUE),0),
    median = round(median(n_upm, na.rm = TRUE),0)
  ) %>%
  mutate(group = "Households in PSU")%>%
  select(group, min, max, mean, median)

tabla_1=rbind(upm, upm2)


setwd(resultados)

write.csv(tabla_1, "tabla_1.csv")



#--------------------------------------------------------------------------------------------------------------------------

# Table 3. Comparison of weighted and unweighted frequencies.




t1=censo %>%
  mutate(AFRODES = as.numeric(AFRODES)) %>%
  filter(AFRODES %in% c(1)) %>%
  mutate(AFRODES = recode(AFRODES, `1` = "Afodescendiente")) %>%
  rename(indicador = AFRODES) %>% 
  group_by(indicador) %>%
  summarise(
    unweighted = n() / nrow(censo) * 100,
    weighted = sum(FACTOR) / sum(censo$FACTOR) * 100,  
    rw = unweighted / weighted,
    sdeff = deffK(FACTOR) %>% round(3),
    .groups = "drop"
  )

t2=censo %>%
  mutate(HLENGUA = as.numeric(HLENGUA)) %>%
  filter(HLENGUA %in% c(1)) %>%
  mutate(HLENGUA = recode(HLENGUA, `1` = "Lengua_indigena")) %>%
  rename(indicador = HLENGUA) %>% 
  group_by(indicador) %>%
  summarise(
    unweighted = n() / nrow(censo) * 100,
    weighted = sum(FACTOR) / sum(censo$FACTOR) * 100,  
    rw = unweighted / weighted,
    sdeff = deffK(FACTOR) %>% round(3),
    .groups = "drop"
  )

t3=censo %>%
  mutate(SEXO = as.numeric(SEXO)) %>%
  filter(SEXO %in% c(1,3)) %>%
  mutate(SEXO = recode(SEXO, `1` = "Hombre", `3` = "Mujer")) %>%
  rename(indicador = SEXO) %>% 
  group_by(indicador) %>%
  summarise(
    unweighted = n() / nrow(censo) * 100,
    weighted = sum(FACTOR) / sum(censo$FACTOR) * 100,  
    rw = unweighted / weighted,
    sdeff = deffK(FACTOR) %>% round(3),
    .groups = "drop"
  )

t4=censo %>%
  mutate(DIS_VER = as.numeric(DIS_VER)) %>%
  filter(DIS_VER %in% c(1)) %>%
  mutate(DIS_VER = recode(DIS_VER, `1` = "Dificultad_ver")) %>%
  rename(indicador = DIS_VER) %>% 
  group_by(indicador) %>%
  summarise(
    unweighted = n() / nrow(censo) * 100,
    weighted = sum(FACTOR) / sum(censo$FACTOR) * 100,  
    rw = unweighted / weighted,
    sdeff = deffK(FACTOR) %>% round(3),
    .groups = "drop"
  )



censo <- censo %>%
  mutate(EDAD = as.numeric(EDAD),
         r_edad = case_when(
           EDAD >= 0  & EDAD <= 9  ~ 1,
           EDAD >= 10 & EDAD <= 19 ~ 2,
           EDAD >= 20 & EDAD <= 29 ~ 3,
           EDAD >= 30 & EDAD <= 39 ~ 4,
           EDAD >= 40 & EDAD <= 49 ~ 5,
           EDAD >= 50 & EDAD <= 59 ~ 6,
           EDAD >= 60  ~ 7
           ))

t5=censo %>%
  mutate(r_edad = as.numeric(r_edad)) %>%
  filter(r_edad %in% c(1,2,3,4,5,6,7)) %>%
  mutate(r_edad = recode(r_edad, `1` = "0-9", `2` = "10-19", `3` = "20-29", `4` = "30-39", `5` = "40-49", `6` = "50-59", `7` = "60+")) %>%
  rename(indicador = r_edad) %>% 
  group_by(indicador) %>%
  summarise(
    unweighted = n() / nrow(censo) * 100,
    weighted = sum(FACTOR) / sum(censo$FACTOR) * 100,  
    rw = unweighted / weighted,
    sdeff = deffK(FACTOR) %>% round(3),
    .groups = "drop"
  )


censo <- censo %>%
  mutate(NIVACAD = as.numeric(NIVACAD),
         r_nivacad = case_when(
           NIVACAD >= 1  & NIVACAD <= 3  ~ 1,
           NIVACAD >= 4  & NIVACAD <= 9  ~ 2,
           NIVACAD >= 10 & NIVACAD <= 14 ~ 3
))


t6=censo %>%
  mutate(r_nivacad = as.numeric(r_nivacad)) %>%
  filter(r_nivacad %in% c(1,2,3)) %>%
  mutate(r_nivacad = recode(r_nivacad, `1` = "Basico", `2` = "Medio", `3` = "Superior")) %>%
  rename(indicador = r_nivacad) %>% 
  group_by(indicador) %>%
  summarise(
    unweighted = n() / nrow(censo) * 100,
    weighted = sum(FACTOR) / sum(censo$FACTOR) * 100,  
    rw = unweighted / weighted,
    sdeff = deffK(FACTOR) %>% round(3),
    .groups = "drop"
  )


t7=censo %>%
  mutate(CONACT = as.numeric(CONACT)) %>%
  filter(CONACT %in% c(10)) %>%
  mutate(CONACT = recode(CONACT, `10` = "Ocupado")) %>%
  rename(indicador = CONACT) %>% 
  group_by(indicador) %>%
  summarise(
    unweighted = n() / nrow(censo) * 100,
    weighted = sum(FACTOR) / sum(censo$FACTOR) * 100,  
    rw = unweighted / weighted,
    sdeff = deffK(FACTOR) %>% round(3),
    .groups = "drop"
  )


t8=censo %>%
  mutate(SERVICIO_MEDICO = as.numeric(SERVICIO_MEDICO)) %>%
  filter(SERVICIO_MEDICO %in% c(5)) %>%
  mutate(SERVICIO_MEDICO = recode(SERVICIO_MEDICO, `5` = "Serv_medico")) %>%
  rename(indicador = SERVICIO_MEDICO) %>% 
  group_by(indicador) %>%
  summarise(
    unweighted = n() / nrow(censo) * 100,
    weighted = sum(FACTOR) / sum(censo$FACTOR) * 100,  
    rw = unweighted / weighted,
    sdeff = deffK(FACTOR) %>% round(3),
    .groups = "drop"
  )

t_final=rbind(t1,t2,t3,t4,t5,t6,t7,t8)

rm("t1","t2","t3","t4","t5","t6","t7","t8")


setwd(resultados)

write.csv(t_final, "tabla_final.csv")

#-----------------------------------------------------------


