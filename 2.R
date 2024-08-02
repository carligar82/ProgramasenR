#instala paquetes necesarios (por si no lo estuvieran)
install.packages("pkgconfig")
install.packages("Rcpp")
install.packages("readxl")
install.packages("pillar")
install.packages("graphics")
#carga los paquetes
library(pkgconfig)
library(Rcpp)
library(readxl)
library(pillar)
library(graphics)
#Crea el dataframe con los datos del excel
congreso <- read_excel ("02_201606_1.xlsx",skip=5)
#Crea una nueva dataframe con los municipios andaluces y con su respectiva participación
Andalucia <- congreso[congreso$`Nombre de Comunidad`=="Andalucía",]
Andalucia$'Participación' <- Andalucia$'Total votantes'/Andalucia$'Total censo electoral'*100
x <- Andalucia$'Nombre de Municipio'
y <- Andalucia$'Participación'
tabla <- data.frame (cbind(x,y))
#Representación de la participación
hist(y,main='Distribución de participación en Andalucía',col='blue',ylab='Municipios')
boxplot(y,horizontal=FALSE,main='Distribución de participación en Andalucía',col='green')
#Mensajes salientes
show('Media Paticipación (%):')
mean(y)
tabla2 <- tabla[order(y,decreasing=TRUE),]
x <-tabla2$'x'
y <- tabla2$'y'
show ('Municipio con mayor participación')
x[1]
y[1]
show ('Municipio con menor participación')
x[nrow(tabla2)]
y[nrow(tabla2)]
