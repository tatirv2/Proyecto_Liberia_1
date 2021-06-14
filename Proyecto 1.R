#Datos liberia proyecto 1
clm <- read.csv("liberia_datos_climaticos.csv", sep = ",", na.strings = "", dec =",")
clm [!complete.cases(clm),]
clm <- na.omit(clm)

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(grid)
library(tidyr)
library(gridExtra)
#Renombrar
clm <- clm %>% 
  rename(Temperatura = Temperatura..Celsius.,
         Fecha = Date,
         Humedad = HumedadRelativa....,
         Velocidad = VelocidadViento..m.s.,
         Lluvia = Lluvia..mm.,
         Irradiacion = Irradiacion..W.m2.,
         Evaporacion = EvapoTranspiracion..mm.
  )

clm <-
  clm %>%
  mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y"))

#Gráfico De Histograma
a<- ggplot(clm,aes(x=Lluvia,group = 1))+
  geom_histogram(
  col="#4682B4",
  fill= "#66CDAA"
  ) +
  ggtitle("Gráfico de Lluvia")+
  xlab("Lluvia mm")+
  ylab("Frecuencia")
b<- ggplot(clm,aes(x=Temperatura,group = 1))+
  geom_histogram(
  col="#8B0000",
  fill= "#B22222"
  ) +
  ggtitle("Gráfico de Temperatura")+
  xlab("Temperatura °C")+
  ylab("Frecuencia")
c<- ggplot(clm,aes(x=Humedad,group = 1))+
  geom_histogram(
    col="#4682B4",
    fill= "#87CEEB"
    ) +
  ggtitle("Gráfico de Humedad")+
  xlab("Humedad %")+
  ylab("Frecuencia")
d<- ggplot(clm,aes(x=Velocidad,group = 1))+
  geom_histogram(
    col="#8B4513",
    fill= "#CD853F"
  )+
  ggtitle("Gráfico de Velocidad")+
  xlab("Velocidad m/s")+
  ylab("Frecuencia")
e<- ggplot(clm,aes(x=Evaporacion,group = 1))+
  geom_histogram(
    col="#556B2F",
    fill= "#8FBC8F"
  )+
  ggtitle("Gráfico de Evaporacion")+
  xlab("Evaporacion mm")+
  ylab("Frecuencia")
f<- ggplot(clm,aes(x=Irradiacion,group = 1))+
  geom_histogram(
    col="#FA8072",
    fill= "#FF6347"
  )+
  ggtitle("Gráfico de Irradiacion")+
  xlab("Irradiacion wm2")+
  ylab("Frecuencia")

grid.arrange(a,b,c,d,e,f,nrow = 6, ncol = 1)


##Lineas Promediadas

Datos_prom <-
  clm %>%
  select(Fecha, Temperatura, Humedad, Velocidad, Lluvia, Irradiacion, Evaporacion)%>%
  mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y"))%>%
  group_by (Fecha = format(Fecha,"%m"))%>%
  summarise(Lluvia = sum(Lluvia), Evaporacion = sum(Evaporacion),
            Temperatura = mean(Temperatura), Velocidad = mean(Velocidad),
            Irradiacion = mean(Irradiacion), Humedad = mean(Humedad))

g<- ggplot(Datos_prom,aes(x=Fecha,y=Lluvia,group = 1,colour = Lluvia))+
  geom_line(
    col="#5F9EA0",
    fill= "#5F9EA0"
  )+
  ggtitle("Gráfico de Lluvia")+
  ylab("Lluvia mm")
h<- ggplot(Datos_prom,aes(x=Fecha,y=Humedad,group = 1,colour = Humedad))+
  geom_line(
    col="#48D1CC",
    fill= "#4682B4"
  )+
  ggtitle("Gráfico de Humedad")+
  ylab("Humedad %")
i<- ggplot(Datos_prom,aes(x=Fecha,y=Temperatura,group = 1,colour = Temperatura))+
  geom_line(
    col="#4682B4",
    fill= "#40E0D0"
  )+
  ggtitle("Gráfico de Temperatura")+
  ylab("Temperatura °C")
j<- ggplot(Datos_prom,aes(x=Fecha,y=Velocidad,group = 1,colour = Velocidad))+
  geom_line(
    col="#CD5C5C",
    fill= "#DC143C"
  )+
  ggtitle("Gráfico de Velocidad")+
  ylab("Velocidad m/s")
k<- ggplot(Datos_prom,aes(x=Fecha,y=Evaporacion,group = 1,colour = Evaporacion))+
  geom_line(
    col="#CD5C5C",
    fill= "#FFA07A"
  )+
  ggtitle("Gráfico de Evaporacion")+
  ylab("Evaporacion mm")
l<- ggplot(Datos_prom,aes(x=Fecha,y=Irradiacion,group = 1,colour = Irradiacion))+
  geom_line(
    col="#B22222",
    fill= "#F08080"
  )+
  ggtitle("Gráfico de Irradiacion")+
  ylab("Irradiacion wm2")

grid.arrange(g, h, i, j, k, l,nrow = 6, ncol = 1)


##Gráfico de nubes de puntos 
m<- ggplot(clm,aes(x=Lluvia,y=Fecha, group = 1))+
  geom_point(
    col="#008080",
    fill= "#8FBC8F"
  )+
  ggtitle("Gráfico de Lluvia")+
  xlab("Lluvia mm")
n<- ggplot(clm,aes(x=Temperatura,y=Fecha, group = 1))+
  geom_point(
    col="#8B0000",
    fill= "#B22222"
  )+
  ggtitle("Gráfico de Temperatura")+
  xlab("Temperatura °C")
o<- ggplot(clm,aes(x=Humedad,y=Fecha, group = 1))+
  geom_point(
    col="#66CDAA",
    fill= "#AFEEEE"
  )+
  ggtitle("Gráfico de Humedad")+
  xlab("Humedad %")
p<- ggplot(clm,aes(x=Velocidad,y=Fecha, group = 1))+
  geom_point(
    col="#D2691E",
    fill= "#D2691E"
  )+
  ggtitle("Gráfico de Velocidad")+
  xlab("Velocidad m/s")
q<- ggplot(clm,aes(x=Evaporacion,y=Fecha, group = 1))+
  geom_point(
    col="#00CED1",
    fill= "#AFEEEE"
  )+
  ggtitle("Gráfico de Evaporacion")+
  xlab("Evaporacion mm")
r<- ggplot(clm,aes(x=Irradiacion,y=Fecha, group = 1))+
  geom_point(
    col="#BC8F8F",
    fill= "#A0522D"
  )+
  ggtitle("Gráfico de Irradiacion")+
  xlab("Irradiacion wm2")

grid.arrange(m, n, o, p, q, r, nrow = 2,ncol = 3)

