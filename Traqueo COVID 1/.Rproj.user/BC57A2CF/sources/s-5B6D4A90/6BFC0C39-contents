#### Instalacion de librerias
install.packages("dplyr")
install.packages("skimr")
install.packages("janitor")
install.packages("tidyverse")

##### Abrir librerias ####
library(dplyr) ### Es una libreria para la gestion de datos
library(skimr) ### Descripcion de datos continuos
library(janitor) ### Descripcion de datos cualitativos
library(readxl) ### Leer el excel de la base de datos
library(ggplot2) ### Para hacer dibujos bonitos

##### Limpiar el cache
rm(list = ls())

#### Cargar la base de datos
traqueo <- read_excel("Data/Base Datos TRAQUEO analisis.xlsx", 
                                          sheet = "Hoja1")

#### Limpiar la base de datos
traqueo <- traqueo %>%
  filter(Sexo >= 0)


##### Analisis exploratorio

#### Variables CUALITATIVAS
### Se describen como frecuencias absolutas y relativas = n (%)
traqueo %>%
  tabyl(Sexo)


##### Variables CUANTITATIVAS
#### Se describen como media o mediana y desviacion estandar o rango intercuartil 
traqueo %>%
  skim(Edad)

###3 Talla
traqueo$Talla <- as.numeric(traqueo$Talla)

traqueo %>%
  skim(Talla)
### Peso
traqueo$Peso <- as.numeric(traqueo$Peso)

traqueo %>%
  skim(Peso)

traqueo %>%
  tabyl(`Ant. HTA`)

### obesidad
traqueo %>%
  tabyl(`Ant. Obesidad`)

traqueo$`Ant. Obesidad`[traqueo$`Ant. Obesidad` == "NA"] <- 0
class(traqueo$`Ant. Obesidad`)
### DM
traqueo %>%
  tabyl(`Ant. DM`)

### EPOC
traqueo %>%
  tabyl(`Ant. EPOC/Asma`)

### Tabaquismo
traqueo%>%
  tabyl(Tabaquismo)

### severidad APACHE
traqueo %>%
  skim(`Grado de severidad`)

### IMC
traqueo$IMC <- as.numeric(traqueo$IMC)

traqueo %>%
  skim(IMC)

###sato2 al ingreso
traqueo %>%
  skim(`SO2 al ingreso`)

### sofa en intubacion
traqueo$`SOFA día intubación` <-as.numeric(traqueo$`SOFA día intubación`)
traqueo %>%
  skim(`SOFA día intubación`)

### sofa en traqueo
traqueo %>%
  skim(`SOFA día traqueostomía`)

### goteo vcasoactivo
traqueo %>%
  tabyl(`Goteo de vasoactivos`)

### vasoactivos POP
traqueo %>%
  tabyl(`Vasoactivos POP`)

### medicamentos sedacion
traqueo %>%
  tabyl(`Medicamentos de sedación`)

### aumento de parametros
traqueo %>%
  tabyl(`Aumento de los parámetros ventilatorios`)

### tiempo intub-traqueo
traqueo$`Tiempo entre la intubación y la traqueostomía` <- as.numeric(traqueo$`Tiempo entre la intubación y la traqueostomía`)

traqueo %>%
  skim(`Tiempo entre la intubación y la traqueostomía`)


traqueo %>% 
  group_by(mortal) %>% 
  skim(`Tiempo entre la intubación y la traqueostomía`)


hist(traqueo$`Tiempo entre la intubación y la traqueostomía`, breaks = 12)

### peep previo
traqueo %>%
  skim(`PEEP previo a la traqueostomía`)

### peep posterior
traqueo %>%
  skim(`PEEP posterior a la traqueostomía`)

### so2 previo
traqueo %>%
  skim(`SO2 previo a la traqueostomía`)

### so2 posterior
traqueo$`SO2 inmediatamente posterior a la traqueostomía` <- as.numeric(traqueo$`SO2 inmediatamente posterior a la traqueostomía`)

traqueo %>%
  skim(`SO2 inmediatamente posterior a la traqueostomía`)

### tipo de canula
traqueo$`Tipo de cánula` <- as.numeric(traqueo$`Tipo de cánula`)

traqueo %<%
  tabyl(`Tipo de cánula`)

###tecnica quirurgica
traqueo %>%
  tabyl(`Técnica quirúrgica`)

### calibre canula
traqueo %>%
  skim(`Calibre de la cánula (French)`)

### metodo de identificacion traquea
traqueo %>%
  tabyl(`Método de identificación de la tráquea`)

### complicaciones intraquirurgicas
traqueo$`Complicaciones intraoperatorias` <- as.numeric(traqueo$`Complicaciones intraoperatorias`)

traqueo %>%
  tabyl(`Complicaciones intraoperatorias`)

### pafi dia antes
traqueo %>%
  skim(`PAFi día anterior a la traqueostomía`)

### pafi dia despues 
traqueo$`PAFi día posterior a la traqueostomía` <-as.numeric(traqueo$`PAFi día posterior a la traqueostomía`)

traqueo %>%
  skim(`PAFi día posterior a la traqueostomía`)

### pafi 3 dias
traqueo %>%
  skim(`PAFi a las 72 posteriores a la traqueostomía`)

### fallece antes retiro ventilador
traqueo$`Fallece antes de retiro del ventilador` <- as.numeric(traqueo$`Fallece antes de retiro del ventilador`)

traqueo %>%
  tabyl(`Fallece antes de retiro del ventilador`)

### aun continua canula
traqueo%>%
  tabyl(`Aún continúa con cánula de traqueostomía`)

### tiempo que continua con canula
traqueo%>%
  tabyl(`Tiempo que continúa con la cánula de traqueostomía`)

### fallece antes de decanulacion
traqueo%>%
  tabyl(`Fallece antes de decanulación`)

### tiempo traqueo-retiro ventilador
traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador` <-as.numeric(traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador`)

traqueo %>%
  skim(`Tiempo entre la traqueostomía y el retiro del ventilador`)

### tiempo traqueostomia-decanulacion
traqueo$`Tiempo entre la traqueostomía y la decanulación` <- as.numeric(traqueo$`Tiempo entre la traqueostomía y la decanulación`)

traqueo %>%
  skim(`Tiempo entre la traqueostomía y la decanulación`)

### complicaciones estancia hospitalaria
traqueo %>%
  tabyl(`Complicaciones a largo plazo`)

###estancia global
traqueo$`Estancia global` <- as.numeric(traqueo$`Estancia global`)
traqueo %>%
  skim(`Estancia global`)

###estancia en UCI
traqueo %>%
  skim(`Estancia en UCI`)

### estado egreso UCI
traqueo%>%
  tabyl(`Estado del egreso  UCI`)

### estado egreso hospi
traqueo%>%
  tabyl(`Estado egreso hospitalario`)


################################
###### ANALISIS EXPLORATORIO DE PLAN DE ANALISIS

##### DELTA DE PAFI

# CREAR LA VARIABE DELTA_PAFI

### Numero
traqueo$pafi_pre <- as.numeric(traqueo$`PAFi día anterior a la traqueostomía`)
traqueo$pafi_post <- as.numeric(traqueo$`PAFi día posterior a la traqueostomía`)



traqueo <- traqueo %>%
  mutate(delta_pafi_post = (pafi_post - pafi_pre) / pafi_pre)  

ggplot( data = traqueo, aes(y = delta_pafi_post))+
  geom_boxplot()+
  facet_wrap(~ `Estado egreso hospitalario`)

traqueo$pafi_72 <- as.numeric(traqueo$`PAFi a las 72 posteriores a la traqueostomía`)



traqueo <- traqueo %>%
  mutate(delta_pafi_72 = (pafi_72 - pafi_pre) / pafi_pre)  

ggplot( data = traqueo, aes(y = delta_pafi_post))+
  geom_boxplot()+
  facet_wrap(~ `Estado egreso hospitalario`)

ggplot( data = traqueo, aes(y = delta_pafi_72))+
  geom_boxplot()+
  facet_wrap(~ `Estado egreso hospitalario`)

##### Mdelo predictivo

with(data = traqueo, t.test(delta_pafi_72 ~ mortal))

traqueo <- traqueo %>%
  filter(`Estado egreso hospitalario` <= 1) %>%
  mutate(mortal = ifelse(`Estado egreso hospitalario` == 1,1,0))

model1 <- glm(mortal  ~  delta_pafi_72, family = binomial(link = "logit"),
              data = traqueo)

library(ResourceSelection)

summary(model1)

hoslem.test(model1$model$delta_pafi_72, fitted(model1))

plot(model1$model$delta_pafi_72, predict(model1, type = "response"))

hist(traqueo$delta_pafi_post)

summary(traqueo$delta_pafi_post)

summary(traqueo$`PAFi día anterior a la traqueostomía`)
summary(traqueo$`PAFi a las 72 posteriores a la traqueostomía`)


#########

traqueo$`SOFA día traqueostomía`


traqueo <- traqueo %>%
  mutate (pafi_sofa = case_when(
    pafi_pre <= 100 ~ 4,
    pafi_pre > 100 & pafi_pre <= 200 ~ 3,
    pafi_pre > 200 & pafi_pre <= 300 ~ 2,
    pafi_pre > 300 & pafi_pre <= 400 ~ 1,
    pafi_pre > 400 ~ 0
  ))


traqueo %>%
  group_by(pafi_sofa) %>%
  skim(`Tiempo entre la intubación y la traqueostomía`) 

########ISA NUEVO########
#### Cargar la base de datos
traqueo <- read_excel("Data/Base Datos TRAQUEO analisis.xlsx", 
                      sheet = "Hoja1")

#### Limpiar la base de datos
traqueo <- traqueo %>%
  filter(Sexo >= 0)

#############APACHE###############
### Apache vs. mortalidad ###
traqueo$`Grado de severidad` <- as.numeric(traqueo$`Grado de severidad`)
traqueo$`Estado egreso hospitalario` <- as.numeric(traqueo$`Estado egreso hospitalario`)
##boxplot apache vs. mortalidad##
  ggplot( data = traqueo, aes(y = `Grado de severidad`)) + geom_boxplot() + facet_wrap(~`Estado egreso hospitalario`)
##numerico apache vs mortalidad##
  traqueo%>%group_by(`Estado egreso hospitalario`) %>%skim(`Grado de severidad`, )

###Apache vs. retiro de ventilador###
x <- traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador`
y <- traqueo$`Grado de severidad`
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
###Apache vs. estancia UCI###
x <- traqueo$`Estancia en UCI`
y <- traqueo$`Grado de severidad`
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
###Apache vs. tiempo decanulacion###
x <-traqueo$`Tiempo entre la traqueostomía y la decanulación`
y <- traqueo$`Grado de severidad`
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
#############tiempo IOT-traqueo###############
###tiempo IOT-traqueo vs. tiempo decanulacion###
x <- traqueo$`Tiempo entre la traqueostomía y la decanulación`
y <- traqueo$`Tiempo entre la intubación y la traqueostomía`
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
### tiempo IOT-traqueo vs. tiempo retiro ventilador###
traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador` <- as.numeric(traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador`)
x <- traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador`
y <- traqueo$`Tiempo entre la intubación y la traqueostomía`
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
###tiempo IOT-traqueo vs. estancia UCI###
x <- traqueo$`Estancia en UCI`
y <- traqueo$`Tiempo entre la intubación y la traqueostomía`
  plot(x, y) 
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")


#############DELTA PAFI POST###############
##########Creacion de variable DELTA PAFI POST #############
traqueo$pafi_pre <- as.numeric(traqueo$`PAFi día anterior a la traqueostomía`)
traqueo$pafi_post <- as.numeric(traqueo$`PAFi día posterior a la traqueostomía`)
  
  traqueo <- traqueo %>%
    mutate(delta_pafi_post = (pafi_post - pafi_pre) / pafi_pre) 
  
###DELTA PAFI POST vs. Mortalidad###
##Boxplot delta pafi post vs. mortalidad##
  ggplot(data = traqueo, aes(y = delta_pafi_post)) + geom_boxplot() + facet_wrap(~`Estado egreso hospitalario`)
##numerica delta_pafi_post vs. mortalidad##
  traqueo%>%group_by(`Estado egreso hospitalario`) %>%skim(delta_pafi_post)

###DELTA PAFI POST vs. Decanulacion###
##Boxplot delta pafi post vs. decanulacion##
  ggplot(data = traqueo, aes(y = delta_pafi_post)) + geom_boxplot() + facet_wrap(~`Aún continúa con cánula de traqueostomía`)
##numerico delta pafi post vs. decanulacion##
  traqueo%>%group_by(`Aún continúa con cánula de traqueostomía`) %>%skim(delta_pafi_post)

###DELTA PAFI POST vs. tiempo decanulacion ###
traqueo$`Tiempo entre la traqueostomía y la decanulación` <- as.numeric(traqueo$`Tiempo entre la traqueostomía y la decanulación`)
x <- traqueo$`Tiempo entre la traqueostomía y la decanulación`
y <- traqueo$delta_pafi_post
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
###DELTA PAFI POST vs. retiro ventilador###
x <- traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador`
y <- traqueo$delta_pafi_post
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
  
###DELTA PAFI POST vs. estancia UCI###
x <- traqueo$`Estancia en UCI`
y <- traqueo$delta_pafi_post
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")


##########Creacion de variable DELTA PAFI 72 #############
traqueo$pafi_72 <- as.numeric(traqueo$`PAFi a las 72 posteriores a la traqueostomía`)
  
  traqueo <- traqueo %>%
    mutate(delta_pafi_72 = (pafi_72 - pafi_pre) / pafi_pre)  
###DELTA PAFI 72 vs. mortalidad###
  ggplot(data = traqueo, aes(y = `delta_pafi_72`)) + geom_boxplot() + facet_wrap(~`Estado egreso hospitalario`)

###DELTA PAFI 72 vs. decanulacion ##
  ggplot(data = traqueo, aes(y = `delta_pafi_72`)) + geom_boxplot() + facet_wrap(~`Aún continúa con cánula de traqueostomía`)

###DELTA PAFI 72 vs. tiempo decanulacion ##
x <- traqueo$`Tiempo entre la traqueostomía y la decanulación`
y <- traqueo$delta_pafi_72
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")

###DELTA PAFI 72 vs. retiro ventilador###
x <- traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador`
y <- traqueo$delta_pafi_72
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")

###DELTA PAFI 72 vs. estancia en UCI###
x <- traqueo$`Estancia en UCI`
y <- traqueo$delta_pafi_72
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
  
###DELTA PAFI 72 vs. fallece antes de retiro ventilador###
traqueo$`Fallece antes de retiro del ventilador` <- as.numeric(traqueo$`Fallece antes de retiro del ventilador`)
ggplot(data = traqueo, aes(y = delta_pafi_72)) + geom_boxplot() + facet_wrap(~`Fallece antes de retiro del ventilador`)
hist(traqueo$`Fallece antes de retiro del ventilador`)

t.test(traqueo$delta_pafi_72 ~ traqueo$`Fallece antes de retiro del ventilador`)

#########SOFA PULMONAR#############
traqueo$`SOFA día traqueostomía`
  
  
traqueo <- traqueo %>%
    mutate (pafi_sofa = case_when(
      pafi_pre <= 100 ~ 4,
      pafi_pre > 100 & pafi_pre <= 200 ~ 3,
      pafi_pre > 200 & pafi_pre <= 300 ~ 2,
      pafi_pre > 300 & pafi_pre <= 400 ~ 1,
      pafi_pre > 400 ~ 0))
###SOFA pulmonar vs. tiempo decanulacion###
##boxplot sofa pulmonar vs. tiempo decanulacion##
ggplot(data = traqueo, aes(y = `Tiempo entre la traqueostomía y la decanulación`)) + geom_boxplot() + facet_wrap(~`pafi_sofa`)
##numerico sofa pulmonar vs. tiempo decanulacion##
traqueo%>%group_by(pafi_sofa) %>%skim(`Tiempo entre la traqueostomía y la decanulación`)

###SOFA pulmonar vs. retiro de ventilador###
##boxplot sofa pulmonar vs. tiempo decanulacion##
ggplot(data = traqueo, aes(y = `Tiempo entre la traqueostomía y el retiro del ventilador`)) + geom_boxplot() + facet_wrap(~pafi_sofa)
##numerico sofa pulmonar vs. retiro ventilador##
traqueo%>%group_by(pafi_sofa) %>%skim(`Tiempo entre la traqueostomía y el retiro del ventilador`)

###SOFA pulmonar vs. estancia UCI###
##boxplot sofa pulmonar##
ggplot(data = traqueo, aes(y = `Estancia en UCI`)) + geom_boxplot() + facet_wrap(~`pafi_sofa`)
##numerico sofa pulmonar vs. estancia UCI##
traqueo%>%group_by(pafi_sofa) %>%skim(`Estancia en UCI`)
##cuantos hay de cada variable##
hist(traqueo$pafi_sofa)

############YA NO SE QUE ESTOY HACIENDO############
###IMC vs. tiempo decanulacion###
traqueo$IMC <- as.numeric(traqueo$IMC)
x <- traqueo$`Tiempo entre la traqueostomía y la decanulación`
y <- traqueo$IMC
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
###IMC vs. tiempo retiro ventilador###
x <- traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador`
y <- traqueo$IMC
  plot(x, y)
  cor(x, y, method = "pearson", use = "pairwise.complete.obs")
  
###IMC vs. decanul###
  ggplot(data = traqueo, aes(y = `IMC`)) + geom_boxplot() + facet_wrap(~`Aún continúa con cánula de traqueostomía`)
  traqueo%>%group_by(`Aún continúa con cánula de traqueostomía`) %>%skim(`IMC`)

###Tiempo entre IOT-traqueo vs. decanulacion###
  ggplot(data = traqueo, aes(y = `Tiempo entre la intubación y la traqueostomía`)) + geom_boxplot() + facet_wrap(~`Aún continúa con cánula de traqueostomía`)
  
###goteo vasoactivo vs. retiro de ventilador##
##boxplot vasoactivo vs. retiro ventilador##
  ggplot(data = traqueo, aes(y = `Tiempo entre la traqueostomía y el retiro del ventilador`)) + geom_boxplot() + facet_wrap(~`Goteo de vasoactivos`)
##numerico vasoactivo vs. retiro ventilador##
traqueo%>%group_by(`Goteo de vasoactivos`) %>%skim(`Tiempo entre la traqueostomía y el retiro del ventilador`)


######Temprana vs. tardia 14 dias#####
####Variable nueva####
traqueo$`Tiempo entre la intubación y la traqueostomía` <- as.numeric(traqueo$`Tiempo entre la intubación y la traqueostomía`)

traqueo <- traqueo %>%
  mutate(tiempo_traqueo = case_when(`Tiempo entre la intubación y la traqueostomía` <= 14 ~ 1, `Tiempo entre la intubación y la traqueostomía` > 14 ~ 2))
  
###tiempo traqueo###
## tiempo traqueo vs. mortalidad ##
##tiempo decanulacion tipo variable##
class(traqueo$`Tiempo entre la traqueostomía y la decanulación`)

##tiempo traqueo vs. tiempo decanulacion##
traqueo$`Tiempo entre la traqueostomía y la decanulación` <- as.numeric(traqueo$`Tiempo entre la traqueostomía y la decanulación`)
ggplot(data = traqueo, aes(y = `Tiempo entre la traqueostomía y la decanulación`)) + geom_boxplot() + facet_wrap(~ `tiempo_traqueo`)

traqueo%>%group_by(tiempo_traqueo) %>%skim(`Tiempo entre la traqueostomía y la decanulación`)


##tiempo traqueo vs. tiempo retiro vent##
traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador` <- as.numeric(traqueo$`Tiempo entre la traqueostomía y el retiro del ventilador`)
ggplot(data = traqueo, aes(y = `Tiempo entre la traqueostomía y la decanulación`)) + geom_boxplot() + facet_wrap(~ `tiempo_traqueo`)

##tiempo traqueo vs. estancia uci##
ggplot(data =  traqueo, aes(y = `Estancia en UCI`)) + geom_boxplot() + facet_wrap(~`tiempo_traqueo`)

t.test(traqueo$`Estancia en UCI` ~ traqueo$tiempo_traqueo)
