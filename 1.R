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
#Cremos un vector con los nombres de las comunidades de cada municipio
provincia <- congreso$`Nombre de Provincia`
#Creamos el factor para el vector de las provincias
factor.provincia <- factor(provincia)
#Hacemos la representación en barras
tabla <- table(factor.provincia)
barplot(tabla,main='Nª de municipios por provincia',col='red',ylab='Municipios',ylim=c(0,400),las=2)
pie(tabla,main='Nª de municipios por provincia')
