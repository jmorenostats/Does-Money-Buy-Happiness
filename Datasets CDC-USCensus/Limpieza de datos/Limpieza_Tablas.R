library(readr)
library(dplyr)
############SUICIDIOS DB----
  #####cARGAR BASES DE DATPS DE SUICIDIOS DEL WORDER DATABSE DEL CDC(CENTER FOR DECEASE AND PREVENTION)
setwd("C:/Users/jufem/OneDrive/Documentos/Econometrica/Datasets CDC-USCensus")
suicides_1 <- read_delim("Suicide cause of Death 2010-2014 .txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
suicides_2 <- read_delim("Suicide cause of Death 2015-2019 .txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
suicides<-bind_rows(suicides_1,suicides_2)[,-1]
View(suicides)
#Data de suicidios por condado, 
  #  obtenido de https://wisqars.cdc.gov/explore/?o=MORT&y1=2010&y2=2019&g=00&t=0&i=2&m=20810&d=&s=0&r=0&me=0&ry=0&yp=65&e=0&a=ALL&a1=0&a2=199&g1=0&g2=199
  #data de 2010-2019  
summary(suicides$Deaths) #Tabla de sucidios
############INCOME DB----
  #####cARGAR BASES DE DATOS DEL US BUREAU CENSUS
setwd("C:/Users/jufem/OneDrive/Documentos/Econometrica/Datasets CDC-USCensus/productDownload_2024-04-13T201354")
#Leer tablas de los datasets para cada anno (2010-2019)
tab2010<-read.csv("ACSDT1Y2010.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2011<-read.csv("ACSDT1Y2011.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2012<-read.csv("ACSDT1Y2012.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2013<-read.csv("ACSDT1Y2013.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2014<-read.csv("ACSDT1Y2014.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2015<-read.csv("ACSDT1Y2015.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2016<-read.csv("ACSDT1Y2016.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2017<-read.csv("ACSDT1Y2017.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2018<-read.csv("ACSDT1Y2018.B07011-Data.csv",skip=1)[,c(1,2,3)]
tab2019<-read.csv("ACSDT1Y2019.B07011-Data.csv",skip=1)[,c(1,2,3)]
#Funcion para limpiar los ID por county
clean_county_id <- function(GEO_ID) { as.numeric(sub(".*US(\\d+).*", "\\1", GEO_ID))
}# Definir la función para limpiar los nombres de los condados
for (i in 2010:2019) {
  tab_name <- paste0("tab", i)
  assign(tab_name, transform(get(tab_name), GEO_ID = clean_county_id(Geography)))
}# Aplicar la función a cada dataframe
#La variable Estimate..Median.income.in.the.past.12.months..Total original de la base de datos le renombramos como Med_Income para todas las tablas
######
for (i in 2010:2019) {
  tab_name <- paste0("tab", i)
  # Acceder al dataframe por su nombre almacenado en tab_name
  df <- get(tab_name)
  # Convertir la tercera columna a numérico y renombrarla como Med_Income
  df$Med_Income <- df[, 3]
  # Eliminar la columna antigua para evitar duplicados si ya no la necesitas
  df <- df[ , !(names(df) %in% names(df)[3])]
  # Reasignar el dataframe modificado al nombre original
  assign(tab_name, df)
}

########
library(dplyr)
tab2010 <- tab2010 %>% mutate(year = '2010')
tab2011 <- tab2011 %>% mutate(year = '2011')
tab2012 <- tab2012 %>% mutate(year = '2012')
tab2013 <- tab2013 %>% mutate(year = '2013')
tab2014 <- tab2014 %>% mutate(year = '2014')
tab2015 <- tab2015 %>% mutate(year = '2015')
tab2016 <- tab2016 %>% mutate(year = '2016')
tab2017 <- tab2017 %>% mutate(year = '2017')
tab2018 <- tab2018 %>% mutate(year = '2018')
tab2019 <- tab2019 %>% mutate(year = '2019')
################# US-CENSUS-Educational Atteinacy################## 
setwd("C:/Users/jufem/OneDrive/Documentos/Econometrica/Datasets CDC-USCensus/Educational Atteinacy")
  #[,c(1,2,9,15,21,27,39,45)]
edu2010<-read.csv("ACSST1Y2010.S1501-Data.csv", skip=1)[,c("Geography","Geographic.Area.Name","Total..Estimate..Percent.bachelor.s.degree.or.higher" )]
edu2011<-read.csv("ACSST1Y2011.S1501-Data.csv",skip=1)[,c("Geography","Geographic.Area.Name","Total..Estimate..Percent.bachelor.s.degree.or.higher" )]
edu2012<-read.csv("ACSST1Y2012.S1501-Data.csv",skip=1)[,c("Geography","Geographic.Area.Name","Total..Estimate..Percent.bachelor.s.degree.or.higher" )]
edu2013<-read.csv("ACSST1Y2013.S1501-Data.csv",skip=1)[,c("Geography","Geographic.Area.Name","Total..Estimate..Percent.bachelor.s.degree.or.higher" )]
edu2014<-read.csv("ACSST1Y2014.S1501-Data.csv",skip=1)[,c("Geography","Geographic.Area.Name","Total..Estimate..Percent.bachelor.s.degree.or.higher" )]
#Nommbres en las tablas no son los mismo ni mismas posiciones
edu2015<-read.csv("ACSST1Y2015.S1501-Data.csv",skip=1)[,c("Geography","Geographic.Area.Name","Percent..Estimate..Percent.bachelor.s.degree.or.higher" )] #1,2,173
edu2016<-read.csv("ACSST1Y2016.S1501-Data.csv",skip=1)[,c("Geography","Geographic.Area.Name","Percent..Estimate..Percent.bachelor.s.degree.or.higher" )]
edu2017<-read.csv("ACSST1Y2017.S1501-Data.csv",skip=1)[,c("Geography","Geographic.Area.Name","Percent..Estimate..Percent.bachelor.s.degree.or.higher" )]
#Aqui vuelve a cambiar
#Edu2018
edu2018<-read.csv("ACSST1Y2018.S1501-Data.csv",skip=1)[,c(1,2,13,31)]
edu2018$Percent..Estimate..Percent.bachelor.s.degree.or.higher<-round(100*(edu2018[,4]/edu2018[,3]),1)
edu2018<-edu2018[,c("Geography","Geographic.Area.Name","Percent..Estimate..Percent.bachelor.s.degree.or.higher" )]
#Edu2019
edu2019<-read.csv("ACSST1Y2019.S1501-Data.csv",skip=1)[,c(1,2,13,31)]
edu2019$Percent..Estimate..Percent.bachelor.s.degree.or.higher<-round(100*(edu2019[,4]/edu2019[,3]),1)
edu2019<-edu2019[,c("Geography","Geographic.Area.Name","Percent..Estimate..Percent.bachelor.s.degree.or.higher" )]
for (i in 2010:2019) {
  tab_name <- paste0("edu", i)
  assign(tab_name, transform(get(tab_name), GEO_ID = clean_county_id(Geography)))
  }
for (i in 2010:2019) {
  tab_name <- paste0("edu", i)
  df <- get(tab_name)
  colnames(df)[3]<- "Bachelor.s.degree.or.higher"
  assign(tab_name, df)
}

#######################################################
###################demograficas "C:\Users\jufem\OneDrive\Documentos\Econometrica\Datasets CDC-USCensus\Demograficas\ACSCP1Y2019.CP05-Data.csv"



# Read the CSV file into a dataframe
setwd("C:/Users/jufem/OneDrive/Documentos/Econometrica/Datasets CDC-USCensus/Demograficas")
demograficas <- read.csv("ACSCP1Y2014.CP05-Data.csv", skip=1)
demograficas2<- read.csv("ACSCP1Y2019.CP05-Data.csv",skip=1)
View(demograficas)


names2010<-c("Geography","Geographic.Area.Name","X2010.Estimate..SEX.AND.AGE..Total.population..Male","X2010.Estimate..SEX.AND.AGE..Total.population..Female","X2010.Estimate..RACE..One.race..White","X2010.Estimate..RACE..One.race..Black.or.African.American" ,"X2010.Estimate..RACE..One.race..American.Indian.and.Alaska.Native","X2010.Estimate..RACE..One.race..Asian",  
             "X2010.Estimate..RACE..One.race..Native.Hawaiian.and.Other.Pacific.Islander" ,"X2010.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")
names2011 <- c("Geography", "Geographic.Area.Name", 
               "X2011.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2011.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2011.Estimate..RACE..One.race..White", 
               "X2011.Estimate..RACE..One.race..Black.or.African.American", 
               "X2011.Estimate..RACE..One.race..American.Indian.and.Alaska.Native", 
               "X2011.Estimate..RACE..One.race..Asian",  
               "X2011.Estimate..RACE..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2011.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")

names2012 <- c("Geography", "Geographic.Area.Name", 
               "X2012.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2012.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2012.Estimate..RACE..One.race..White", 
               "X2012.Estimate..RACE..One.race..Black.or.African.American", 
               "X2012.Estimate..RACE..One.race..American.Indian.and.Alaska.Native", 
               "X2012.Estimate..RACE..One.race..Asian",  
               "X2012.Estimate..RACE..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2012.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")

names2013 <- c("Geography", "Geographic.Area.Name", 
               "X2013.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2013.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2013.Estimate..RACE..One.race..White", 
               "X2013.Estimate..RACE..One.race..Black.or.African.American", 
               "X2013.Estimate..RACE..One.race..American.Indian.and.Alaska.Native", 
               "X2013.Estimate..RACE..One.race..Asian",  
               "X2013.Estimate..RACE..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2013.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")

names2014 <- c("Geography", "Geographic.Area.Name", 
               "X2014.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2014.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2014.Estimate..RACE..One.race..White", 
               "X2014.Estimate..RACE..One.race..Black.or.African.American", 
               "X2014.Estimate..RACE..One.race..American.Indian.and.Alaska.Native", 
               "X2014.Estimate..RACE..One.race..Asian",  
               "X2014.Estimate..RACE..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2014.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")


names2015 <- c("Geography", "Geographic.Area.Name", 
               "X2015.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2015.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2015.Estimate..RACE..Total.population..One.race..White", 
               "X2015.Estimate..RACE..Total.population..One.race..Black.or.African.American", 
               "X2015.Estimate..RACE..Total.population..One.race..American.Indian.and.Alaska.Native", 
               "X2015.Estimate..RACE..Total.population..One.race..Asian",  
               "X2015.Estimate..RACE..Total.population..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2015.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")

names2016 <- c("Geography", "Geographic.Area.Name", 
               "X2016.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2016.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2016.Estimate..RACE..Total.population..One.race..White", 
               "X2016.Estimate..RACE..Total.population..One.race..Black.or.African.American", 
               "X2016.Estimate..RACE..Total.population..One.race..American.Indian.and.Alaska.Native", 
               "X2016.Estimate..RACE..Total.population..One.race..Asian",  
               "X2016.Estimate..RACE..Total.population..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2016.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")

names2017 <- c("Geography", "Geographic.Area.Name", 
               "X2017.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2017.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2017.Estimate..RACE..Total.population..One.race..White", 
               "X2017.Estimate..RACE..Total.population..One.race..Black.or.African.American", 
               "X2017.Estimate..RACE..Total.population..One.race..American.Indian.and.Alaska.Native", 
               "X2017.Estimate..RACE..Total.population..One.race..Asian",  
               "X2017.Estimate..RACE..Total.population..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2017.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")

names2018 <- c("Geography", "Geographic.Area.Name", 
               "X2018.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2018.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2018.Estimate..RACE..Total.population..One.race..White", 
               "X2018.Estimate..RACE..Total.population..One.race..Black.or.African.American", 
               "X2018.Estimate..RACE..Total.population..One.race..American.Indian.and.Alaska.Native", 
               "X2018.Estimate..RACE..Total.population..One.race..Asian",  
               "X2018.Estimate..RACE..Total.population..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2018.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")

names2019 <- c("Geography", "Geographic.Area.Name", 
               "X2019.Estimate..SEX.AND.AGE..Total.population..Male", 
               "X2019.Estimate..SEX.AND.AGE..Total.population..Female", 
               "X2019.Estimate..RACE..Total.population..One.race..White", 
               "X2019.Estimate..RACE..Total.population..One.race..Black.or.African.American", 
               "X2019.Estimate..RACE..Total.population..One.race..American.Indian.and.Alaska.Native", 
               "X2019.Estimate..RACE..Total.population..One.race..Asian",  
               "X2019.Estimate..RACE..Total.population..One.race..Native.Hawaiian.and.Other.Pacific.Islander", 
               "X2019.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population")


demo2010<-demograficas[,names2010]
demo2011<-demograficas[,names2011]
demo2012<-demograficas[,names2012]
demo2013<-demograficas[,names2013]
demo2014<-demograficas[,names2014]
demo2015<-demograficas2[,names2015]
demo2016<-demograficas2[,names2016]
demo2017<-demograficas2[,names2017]
demo2018<-demograficas2[,names2018]
demo2019<-demograficas2[,names2019]


for (year in c("2010", "2011", "2012", "2013", "2014","2015","2016","2017","2018","2019")) {
  dat <- get(paste0("demo", year))
  colnames(dat) <- c("Geography", "Geographic.Area.Name", "Total.population..Male", "Total.population..Female", "Total..White", "Total..Black.or.African.American",
                     "Total..American.Indian.and.Alaska.Native", "Total..Asian", "Total..One.race..Native.Hawaiian.and.Other.Pacific.Islander", "Total..HISPANIC.OR.LATINO.AND.RACE")
  assign(paste0("demo", year), dat)
}

demo2010<-demo2010%>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2011 <- demo2011 %>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2012 <- demo2012%>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2013 <- demo2013 %>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2014 <- demo2014%>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2015 <- demo2015%>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2016 <- demo2016 %>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2017 <- demo2017 %>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2018 <- demo2018 %>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))
demo2019 <- demo2019 %>%mutate(across(-c(1, 2), ~ ifelse(. == "N", 0, .))) %>%
  mutate(across(-c(1, 2), as.numeric))

for (i in 2010:2019) {
  tab_name <- paste0("demo", i)
  assign(tab_name, transform(get(tab_name), GEO_ID = clean_county_id(Geography)))
}


###################################
#uniendo tablas por año


for (i in 2010:2019) {
  tab_name <- paste0("tab", i)
  edu_name <- paste0("edu", i)
  demo_name <- paste0("demo", i)
  
  # Unir tab y edu
  tab_edu_joined <- inner_join(get(tab_name), get(edu_name), by = c("GEO_ID" = "GEO_ID")) %>%
    select(GEO_ID, year, Med_Income, Bachelor.s.degree.or.higher)
  
  # Unir el resultado con demo
  final_result <- inner_join(tab_edu_joined, get(demo_name), by = c("GEO_ID" = "GEO_ID"))
  
  # Asignar el resultado final a la variable tab correspondiente
  assign(tab_name, final_result)
}


###########UNIENDO TABLAS----
tabla_total=bind_rows(tab2010, tab2011,tab2012,tab2013,tab2014,tab2015,tab2016,tab2017,tab2018,tab2019)
tabla_total$GEO_ID<-as.character(tabla_total$GEO_ID)
tabla_total$year<-as.character(tabla_total$year)
tabla_total$Med_Income<-as.numeric(tabla_total$Med_Income)
suicides$Year<-as.character(suicides$Year)
suicides$County_Code<-suicides$`County Code`
Suicides_mod<-suicides%>%group_by(County_Code,County,Year) %>%
summarise(Deaths = sum(Deaths, na.rm = TRUE))
tabla_total=tabla_total %>%
  inner_join(Suicides_mod, by = c("GEO_ID" = "County_Code", "year" = "Year")) %>%
  select("County","Deaths","year","Med_Income","Bachelor.s.degree.or.higher","Total.population..Male" ,"Total.population..Female"
         , "Total..White" ,"Total..Black.or.African.American"    ,                       
          "Total..American.Indian.and.Alaska.Native"   ,                 "Total..Asian"     ,                                          
          "Total..One.race..Native.Hawaiian.and.Other.Pacific.Islander", "Total..HISPANIC.OR.LATINO.AND.RACE"  )

View(tabla_total)

#Poverty rate:

tab2010 <- tab2010 %>% mutate(year = '2010')
tab2011 <- tab2011 %>% mutate(year = '2011')
tab2012 <- tab2012 %>% mutate(year = '2012')
tab2013 <- tab2013 %>% mutate(year = '2013')
tab2014 <- tab2014 %>% mutate(year = '2014')
tab2015 <- tab2015 %>% mutate(year = '2015')
tab2016 <- tab2016 %>% mutate(year = '2016')
tab2017 <- tab2017 %>% mutate(year = '2017')
tab2018 <- tab2018 %>% mutate(year = '2018')
tab2019 <- tab2019 %>% mutate(year = '2019')


#saveRDS(tabla_total,"C:/Users/jufem/OneDrive/Documentos/Econometrica/Tabla_Total")

