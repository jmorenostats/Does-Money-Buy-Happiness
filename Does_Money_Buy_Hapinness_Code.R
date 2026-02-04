rm(list=ls())
#####LIBRRIAS######
library(dplyr)
library(readxl)
library(ggplot2)
library(stargazer)
library(glmtoolbox)
library(xtable)
#Lecura base de datos----

tabla_total<-read_excel("C:/Users/jufem/OneDrive/Documentos/Econometrica/Tabla_Total.xlsx")
tabla_total<-within(tabla_total,Percent.Poverty.rate<-as.numeric(Percent.Poverty.rate) )

#Graficos----
promedio_por_anno <- tabla_total %>%
  group_by(year) %>%
  summarise(
    Avg_Med_Income = mean(Med_Income, na.rm = TRUE),
    Avg_Deaths = mean(Deaths, na.rm = TRUE)
  )

plot(y = promedio_por_anno$Avg_Med_Income, x = promedio_por_anno$year, 
     type = "o", # o para línea, p para puntos, b para ambos
     main = "Promedio del ingreso mediano y \n promedio de numero de suicidios \n por Año", # Título del gráfico
     xlab = "Año", # Etiqueta del eje x
     ylab = "Promedio de ingreso mediano" # Etiqueta del eje y
     ,col='red'
)

ggplot(data = promedio_por_anno, aes(x = (year), y = Avg_Med_Income)) +
  geom_line(aes(group = 1),col = "salmon") + # Utilizar líneas para conectar los puntos
  geom_point(col = "salmon") + # Agregar puntos
  labs(title = "Promedio del ingreso mediano y \n promedio de numero de suicidios \n por Año", # Título del gráfico
       x = "Año", # Etiqueta del eje x
       y = "Promedio de ingreso mediano"
  )+theme(
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"),  
    panel.grid.major = element_line(color = "gray"),  
    panel.grid.minor = element_line(color = "lightgray")  
  )
#graficando ambos ingreso mediano en miles y decesos por suicidio
Med_Income_x1000<-tabla_total$Med_Income/1000 #Ingreso mediano por hogares en miles de dolares Y MUERTES POR SUICIDIO
promedio_por_anno <- merge(aggregate( Med_Income_x1000~ year, data = tabla_total, FUN = mean),
                           aggregate(Deaths ~ year, data = tabla_total, FUN = mean),
                           by = "year")

ggplot(data = promedio_por_anno, aes(x = year)) +
  geom_line(aes(y = Med_Income_x1000, color = "Med_Income_x1000", group = 1)) + 
  geom_point(aes(y = Med_Income_x1000, color = "Med_Income_x1000")) +
  geom_line(aes(y = Deaths, color = "Deaths", group = 1)) + 
  geom_point(aes(y = Deaths, color = "Deaths")) +
  labs(title = "Promedio del ingreso mediano y \n promedio de numero de suicidios \n por Año",
       x = "Año",
       y = "Promedio de ingreso mediano (en miles) y decesos",
       color = "Leyenda") +  # Título de la leyenda
  scale_color_manual(values = c("Med_Income_x1000" = "salmon", "Deaths" = "navyblue"),
                     labels = c("Med_Income_x1000" = "Ingreso Mediano (x1000)", "Deaths" = "Decesos")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Promedio de decesos")) +
  theme(legend.position = "bottom")+theme(
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"),  
    panel.grid.major = element_line(color = "gray"),  
    panel.grid.minor = element_line(color = "lightgray")  
  )

#graduados universitarios & sucidios
promedio_por_anno_edu <- merge(aggregate( Bachelor.s.degree.or.higher~ year, data = tabla_total, FUN = mean),
                           aggregate(Deaths ~ year, data = tabla_total, FUN = mean),
                           by = "year")
ggplot(data = promedio_por_anno_edu, aes(x = year)) +
  geom_line(aes(y = Bachelor.s.degree.or.higher, color = "Graduados Universitarios", group = 1)) + 
  geom_point(aes(y = Bachelor.s.degree.or.higher, color = "Graduados Universitarios")) +
  geom_line(aes(y = Deaths, color = "Decesos", group = 1)) + 
  geom_point(aes(y = Deaths, color = "Decesos")) +
  labs(title = "Promedio de graduados universitarios y \n promedio de número de suicidios \n por año",
       x = "Año",
       y = "Promedio de graduados universitarios o más y decesos",
       color = "Leyenda") +  # Título de la leyenda
  scale_color_manual(values = c("Graduados Universitarios" = "salmon", "Decesos" = "navyblue")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Promedio de decesos")) +
  theme(legend.position = "bottom")+theme(
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"),  
    panel.grid.major = element_line(color = "gray"),  
    panel.grid.minor = element_line(color = "lightgray")  
  )

#tasa de pobreza y suicidios
promedio_por_anno_pov <- merge(aggregate(Percent.Poverty.rate  ~ year, data = tabla_total, FUN = mean),
                               aggregate(Deaths ~ year, data = tabla_total, FUN = mean),
                               by = "year")
ggplot(data = promedio_por_anno_pov, aes(x = year)) +
  geom_line(aes(y = Percent.Poverty.rate, color = "Porcentaje tasa de pobreza", group = 1)) + 
  geom_point(aes(y = Percent.Poverty.rate, color = "Porcentaje tasa de pobreza")) +
  geom_line(aes(y = Deaths, color = "Decesos", group = 1)) + 
  geom_point(aes(y = Deaths, color = "Decesos")) +
  labs(title = "Promedio del porcentaje de tasa de pobreza y \n promedio de número de suicidios \n por año",
       x = "Año",
       y = "Promedio tasa de pobreza y decesos",
       color = "Leyenda") +  # Título de la leyenda
  scale_color_manual(values = c("Tasa de pobreza" = "salmon", "Decesos" = "navyblue")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Promedio de decesos")) +
  theme(legend.position = "bottom")+theme(
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"),  
    panel.grid.major = element_line(color = "gray"),  
    panel.grid.minor = element_line(color = "lightgray")  
  )


##########################  MODELAMIENTO ################################################----

#SEM Modelos de ecuaciones generealizadas----

tabla_total_2 <- within(tabla_total,{Ctime <- factor(year)
 Ctime <- relevel(Ctime,ref="2015")})
Formula_1<-Deaths~Med_Income+Total.population..Male+Total..White+Total..Black.or.African.American+Total..American.Indian.and.Alaska.Native+Total..Asian+Total..One.race..Native.Hawaiian.and.Other.Pacific.Islander+  
  Total..HISPANIC.OR.LATINO.AND.RACE + Med_Income* Bachelor.s.degree.or.higher+Percent.Poverty.rate
fit1<-glmgee(Formula_1,id=County,family=poisson(),corstr="Independence",data=tabla_total)
summary(fit1)
Formula_1_1<-Deaths~Med_Income+Total.population..Male+Total..White+Total..Black.or.African.American+Total..American.Indian.and.Alaska.Native+Total..Asian+Total..One.race..Native.Hawaiian.and.Other.Pacific.Islander+  
  Total..HISPANIC.OR.LATINO.AND.RACE + Med_Income* Bachelor.s.degree.or.higher
fit2<-glmgee(Formula_1_1,id=County,family=poisson(),corstr="Independence",data=tabla_total)
anova(fit2,fit1) #Como la variable Percent.Poverty.rate no salió significativa, no la incluimos en el modelo.


##Cambiando estructura de correlación entre modelos:
fit3<-update(fit2,corstr= "Non-Stationary-M-dependent(2)")
fit4<-update(fit2,corstr= "Stationary-M-dependent(1)")
fit5<-update(fit2,corstr= "User-defined") 


##selección de modelo por estructura de correlación:
 a <- CIC(fit2,fit3,fit4 ,fit5, verbose=FALSE)
 b <- QIC(fit2,   fit3,fit4 ,fit5, verbose=FALSE)
 cbind(a,QIC=b[,"QIC"]) #Se selecciona fit 4 como modelo escogido
 
 
 ####Usando logaritmo del ingreso y usando los mismos predictores del modelo escogido 
 Formula_2<-Deaths~log(Med_Income)* log(Bachelor.s.degree.or.higher)+Total.population..Male+Total..White+Total..Black.or.African.American+Total..American.Indian.and.Alaska.Native+Total..Asian+Total..One.race..Native.Hawaiian.and.Other.Pacific.Islander+  
   Total..HISPANIC.OR.LATINO.AND.RACE 
 
 fit6<-glmgee(formula=Formula_2,family=poisson(),corstr="Stationary-M-dependent(1)",data=tabla_total)
 summary(fit6) 
 
 
##ANALISIS DE RESIDUALES DEL MODELO ESCOGIDO: 
 
summary(fit4)
stargazer::stargazer(summary(fit4),type="text")
 
 residuals(fit4, type="deviance", plot.it=TRUE, pch=16)
 residuals(fit4, type="mahalanobis", plot.it=TRUE, pch=16)

 
 
 
 
















