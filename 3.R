#instala paquetes necesarios (por si no lo estuvieran)
install.packages("pkgconfig")
install.packages("Rcpp")
install.packages("readxl")
install.packages("pillar")
install.packages("graphics")
install.packages("dplyr")
#carga los paquetes
library(pkgconfig)
library(Rcpp)
library(readxl)
library(pillar)
library(graphics)
library(dplyr)
#Apartado a
#Crea el dataframe con los datos del excel
congreso <- read_excel ("02_201606_1.xlsx",skip=5)
#Construye en vector de partidos y el dataframe del voto por comunidades de 4 partidos
partidos <- names(congreso)
d <- 0
ind <- 1
partidos2 <- vector ()
for (i in partidos)
{
  if (d!=0)
    partidos2[ind]=i
    ind <- ind+1
  if (i=='Votos nulos')
    d <- 1
}
partidos <- partidos2[!is.na(partidos2)]
remove(partidos2)
congreso$'Comunidad'<- congreso$`Nombre de Comunidad`
congreso$'Votantes'<- congreso$`Total votantes` 
i <- partidos[1]
congreso$'a' <- congreso[i]
i <- partidos[2]
congreso$'b' <- congreso[i]
i <- partidos[3]
congreso$'c' <- congreso[i]
i <- partidos[4]
congreso$'d' <- congreso[i]
congresocom <- congreso %>% group_by (Comunidad) %>% summarize(Votantes=sum(Votantes),aa=sum(a),bb=sum(b),cc=sum(c),dd=sum(d))
#Comienzan las representaciones
while (d<=nrow(congresocom))
{
  porcentaje <- c(congresocom[d,3],congresocom[d,4],congresocom[d,5],congresocom[d,6],congresocom[d,2]-(congresocom[d,3]+congresocom[d,4]+congresocom[d,5]+congresocom[d,6]))
  names(porcentaje)<-c(partidos[1],partidos[2],partidos[3],partidos[4],'OTROS')
  pie(as.integer(porcentaje), labels=names (porcentaje), clockwise=TRUE, main = congresocom[d,1],col=c("#000099","#FF0000","#660066","#FF3300","#FFFF66"))
  cat ("Porcentaje de votos:\n")
  cat(sprintf("%s %.2f\n",partidos[1],congresocom[d,3]/congresocom[d,2]*100))
  cat(sprintf("%s %.2f\n",partidos[2],congresocom[d,4]/congresocom[d,2]*100))
  cat(sprintf("%s %.2f\n",partidos[3],congresocom[d,5]/congresocom[d,2]*100))
  cat(sprintf("%s %.2f\n",partidos[4],congresocom[d,6]/congresocom[d,2]*100))
  cat(sprintf("OTROS %.2f\n",100-congresocom[d,3]/congresocom[d,2]*100-congresocom[d,4]/congresocom[d,2]*100-congresocom[d,5]/congresocom[d,2]*100-congresocom[d,6]/congresocom[d,2]*100))
  print ("Presione INTRO para continuar")
  number <- scan(n=1)
  d <- d+1
}
#Apartado b
#Crea el dataframe de la participación por provincia y nos da la mayor
congreso$'Provincia'<- congreso$`Nombre de Provincia`
congreso$'Censo' <- congreso$'Total censo electoral'
congresocom <- congreso %>% group_by (Provincia) %>% summarize(Votantes=sum(Votantes),censo=sum(Censo))
congresocom$'Participación' <- congresocom$'Votantes'/congresocom$'censo'*100
congresosort <- congresocom[order(congresocom$'Participación',decreasing=TRUE),]
number <- '%'
cat(sprintf("La provincia con mayor porcentaje de participación ha sido %s con un %.2f %s.\n",congresosort[1,1],congresosort[1,4],number))
#Apartado c
#Crea el vector con los partidos políticos y dataframe con los resultados electorales obtenidos
prov <- read_excel ("PROV_02_201606_1.xlsx",skip=4)
partidos2 <- names(prov)
prov <- read_excel ("PROV_02_201606_1.xlsx",skip=5)
partidos3 <- names(prov)
d <- 1
ind <- 0
for (i in partidos3)
{
  if (i=='Votos nulos')
    ind <- d+1
  d <- d+1
}
partidos <- vector ()
ind2 <-ind
d <- 1
while (ind<length(partidos2))
{
  partidos[d]=partidos2[ind]
  d <- d+1
  ind <-ind+2
}
remove(partidos2,partidos3)
#Muestra la cantidad de diputados obtenidos para cada partido con el sistema usual y el nuevo sistema
for (i in partidos)
{
  votantes=sum(prov$'Total votantes')
  votos=sum(prov[,ind2])
  ind2 <-ind2+1
  diputadosreales=sum(prov[,ind2])
  diputados=votos*350/votantes
  ind2 <- ind2+1
  cat(sprintf("%s:\n",i))
  cat(sprintf("Diputados obtenidos: %.0f\n",diputadosreales))
  cat(sprintf("Diputados estimados: %.0f\n",diputados))
  cat(sprintf("Diferencia de diputados: %.0f\n",diputados-diputadosreales))
}
#Apartado d
#Crea vector de partidos en el congreso y en el senado
congreso <- read_excel ("02_201606_1.xlsx",skip=5)
senado <- read_excel ("03_201606_1.xlsx",col_names=FALSE,skip=5)
pc <- names(congreso)
ps <- vector()
ps <- c(unlist(senado[2,]))
pcs <- vector ()
d <- 1
dd <- 0
ind <- 0
ind2 <- 1
for (i in pc)
{
  if (i=='Votos nulos')
    ind <- d+1
  if ((d>=ind)&(ind!=0))
  {
    for (a in ps)
    {
      if (a==i)
      {
        dd <- 1
      }
    }
    if (dd==1)
    {
      dd <- 0
      pcs[ind2]=i
      ind2 <- ind2+1
    }
  }
  d <- d+1
}
#Crea dataframes de trabajo
#El Congreso:
comunidad <- c(congreso$'Nombre de Comunidad')
votantes <- c(congreso$'Total votantes')
congresot <- data.frame (comunidad,votantes)
remove (comunidad,votantes)
for (i in pcs)
{
  congresot[[i]] <- congreso[[i]]
}
remove (congreso)
senado <- senado[-1,]
senado <- senado[-1,]
#El Senado (Suma las columnas de cada partido)
d <- 1
for (i in ps)
{
  if (i=='Nombre de Comunidad')
  {
    ind2 <- d
  }
  if (i=='Total votantes')
  {
    ind <- d
  }
  d <- d+1
}
senadot <- data.frame (senado[,ind2],senado[,ind])
d <- 3
for (i in pcs)
{
  senadot[,d]='0'
  d <- d+1
}
d <- 3
senadot[,2]='0'
for (i in pcs)
{
  aux <- c(unlist(senadot[,d]))
  ind <- 1
  for (a in ps)
  {
    if (a==i)
    {
      aux2 <- c(unlist(senado[,ind]))
      aux <- as.numeric(aux) + as.numeric(aux2)
    }
    ind <- ind+1
  }
  senadot[,d] <- aux
  d <- d+1
}
remove (aux,aux2,senado)
aux <- c(unlist(senadot[,2]))
d <- 3
for (i in pcs)
{
  aux2 <- c(unlist(senadot[,d]))
  aux <- as.numeric(aux) + as.numeric(aux2)
  colnames(senadot)[d]=i
  d <- d+1 
}
senadot[,2] <- aux
colnames(senadot)[1]='comunidad'
colnames(senadot)[2]='votos'
remove (aux,aux2)
#Para cada partivo va creando el dataframe que compara porcentaje de voto a cada cámara
for (i in pcs)
{
  cat(sprintf("Porcentaje votos %s por comunidades:\n",i))
  df <- data.frame (congresot['comunidad'],congresot['votantes'])
  df$'a'=congresot[[i]]
  Congreso <- df %>% group_by (comunidad) %>% summarize(votantes=sum(votantes),a=sum(a))
  Compara <- Congreso[order(Congreso['comunidad'],decreasing=FALSE),]
  Congreso <- Compara
  df <- data.frame (senadot['comunidad'],senadot['votos'])
  df$'a'=senadot[[i]]
  Senado <- df %>% group_by (comunidad) %>% summarize(votos=sum(as.integer(votos)),a=sum(a))
  Compara <- Senado[order(Senado['comunidad'],decreasing=FALSE),]
  Senado <- Compara
  remove (df)
  Compara <- data.frame (Congreso['comunidad'])
  Compara['% Congreso']=Congreso['a']/Congreso['votantes']*100
  Compara['% Senado']=Senado['a']/Senado['votos']*100
  print (Compara)
  u <- paste('Porcentaje votos ',i,' por comunidad')
  barplot(`colnames<-`(t(Compara[-1]), Compara[,1]), beside=TRUE,horiz=TRUE,legend.text = TRUE, col = c("red", "green"),main=u,xlim=c(0,80),las=1)
  print ("Presione INTRO para continuar")
  number <- scan(n=1)
}
