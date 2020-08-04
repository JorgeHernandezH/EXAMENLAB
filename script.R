#   Jorge Hernández Hernández
#   Métodos experimentales de la Física
#   4/08/2020
#----------------------------------------------------------------
#----------------         Examen   -----------------------
#----------------------------------------------------------------

#----IMPORTANTE: EL NOMBRE DE LAS VARIABLES USADAS AQUÍ 
#----COINCIDEN CON LOS NOMBRES USADOS EN LA TEORÍA.


#---Primero cargamos la librerias a usar (previamente descargadas)
require(ggplot2)
require(reshape2)
require(scales)
require(plot3D)
require(rgl)
#----------------------------------------------------------------



#--------------------------Esto sirve para aumentar los decimales
options(digits = 10)     
#----------------------------------------------------------------




#---------Cambiamos al directorio de trabajo end donde guardamos los datos
          #de las simulaciones para 30 kev y 50 kev 
          # y visualizamos las primeras lineas (comando head)

setwd("C:/Users/JorgeHH/Documents/Cinvestav/2doSemestre/Laboratorio/Examen/1")
dat <- read.table("RANGE.TXT", dec=".", sep="")
setwd("C:/Users/JorgeHH/Documents/Cinvestav/2doSemestre/Laboratorio/Examen/2")
dat2 <- read.table("RANGE.TXT", dec=".", sep="")
head(dat)
head(dat2)
#----------------------------------------------------------------




#"dat" contiene la informacion de la simulacion en 30 kev
#"dat2" contiene la informacion de la simulacion en 50 kev




#------IMPORTANTE: La columna 3 (en "dat" y "dat2")contiene datos que 
#------no son relevantes para el analizís, mas adelante esta columna 
#------sera reemplazada con datos utiles (los R_k).




#-------------Renombramos las columnas y visualizamos el cambio
colnames(dat)<-c("x_k","n_k","other")
colnames(dat2)<-c("x_k","n_k","other")
head(dat)
head(dat2)
#----------------------------------------------------------------




#-------Este data frame se usara para almacenar los datos: Media,
#------- Desviacion_estandar y numero total de mediciones(N).

RES<-data.frame(N=c(0,0),Media=c(0,0),Desviación_estandar=c(0,0),type=c("30 kev","50 kev"))


#----------------------------------------------------------------



#------Calculamos el valor de N ---------------------------------
RES[1,1]<-sum(dat$n_k)
RES[2,1]<-sum(dat2$n_k)
#----------------------------------------------------------------




#----------Cambio de n_k por F_k y calculo de R_k ---------------

dat$other<-dat$n_k/(RES[1,1]-1)
dat2$other<-dat2$n_k/(RES[2,1]-1)

dat$n_k<-dat$n_k/RES[1,1]
dat2$n_k<-dat2$n_k/RES[2,1]

colnames(dat)<-c("x_k","F_k","R_k")
colnames(dat2)<-c("x_k","F_k","R_k")

head(dat)
head(dat2)
#----------------------------------------------------------------




#-------------- Verificamos la normalizacion --------------------
#Verificamos la Normalizacion de los datos para 30 kev
sum(dat$F_k)

#Verificamos la Normalizacion de los datos para 50 kev
sum(dat2$F_k)

#----------------------------------------------------------------





#--------- Calculo de las medias (ION RANGE en este caso)--------

for(k in 1:nrow(dat)) {
RES[1,2]<-dat[k,1]*dat[k,2]+RES[1,2]
}

for(k in 1:nrow(dat2)) {
RES[2,2]<-dat2[k,1]*dat2[k,2]+RES[2,2]
}

#El ION RANGE para 30 kev y 50 kev, vale respectivamente:
RES[1,2]

RES[2,2]
#----------------------------------------------------------------





#----------- Calculo de las desviaciones estandar ---------------

for(k in 1:nrow(dat)) {
RES[1,3] <- dat[k,2]*(dat[k,1]-RES[1,2])^2 + RES[1,3]
}
RES[1,3] <- sqrt(RES[1,3])


for(k in 1:nrow(dat2)) {
RES[2,3] <- dat2[k,2]*(dat2[k,1]-RES[2,2])^2 + RES[2,3]
}
RES[2,3] <- sqrt(RES[2,3])


#Las desviaciones estandar para 30 kev y 50 kev, valen respectivamente:
RES[1,3]

RES[2,3]
#----------------------------------------------------------------



#---------------Veamos como queda el resumen(RES)----------------
RES
#----------------------------------------------------------------



#-----------------------------Distribuciones--------------------------------------

N1<-RES[1,1]
N2<-RES[2,1]
M1<-RES[1,2]
M2<-RES[2,2]
S1<-RES[1,3]
S2<-RES[2,3]


xx <- seq(2.01,200.01,2)

#Las distribuciones marcadas en los comentarios siguientes, crean las distribuciones gaussianas,
#automaticamente por R. En el programa que uso, creo las distribuciones metiendo directamente las
#expresiones para la distribucion. Las dos formas son equivalentes y se puede comprobar graficando,
#los data frame respectivos. 


#dfRG1 <- data.frame(x=xx,y=dnorm(xx, mean = M1, sd = S1),datos="Aproximacion R-Gaussiana para 30 kev")
#dfRG2 <- data.frame(x=xx,y=dnorm(xx, mean = M2, sd = S2),datos="Aproximacion R-Gaussiana para 50 kev")



df1 <- data.frame(x=dat$x_k,y=dat$F_k,datos="30 kev")
df2 <- data.frame(x=dat2$x_k,y=dat2$F_k,datos="50 kev")

dfP1 <- data.frame(x=xx,y=((M1^xx)*(exp(-M1)))/(factorial(xx)),datos="Aproximacion Poisson para 30 kev")
dfP2 <- data.frame(x=xx,y=((M2^xx)*(exp(-M2)))/(factorial(xx)),datos="Aproximacion Poisson para 50 kev")

dfG1 <- data.frame(x=xx,y=(1/(S1*sqrt(2*pi)))*exp(-((xx-M1)^2)/(2*S1^2)),datos="Aproximacion Gaussiana para 30 kev")
dfG2 <- data.frame(x=xx,y=(1/(S2*sqrt(2*pi)))*exp(-((xx-M2)^2)/(2*S2^2)),datos="Aproximacion Gaussiana para 50 kev")

#----------------------------------------------------------------



#-----------------------------------------------------------
CHI<-data.frame(datos=c("30 kev","50 kev"),G=c(0,0),P=c(0,0))
#----------------------------------------------------------------




for(l in 1:100){
CHI[1,2]<-(((df1[l,2]-dfG1[l,2])^2)/(dfG1[l,2]))+CHI[1,2]
}

for(l in 1:100){
CHI[2,2]<-(((df2[l,2]-dfG2[l,2])^2)/(dfG2[l,2]))+CHI[2,2]
}

for(l in 1:77){
CHI[1,3]<-(((df1[l,2]-dfP1[l,2])^2)/(dfP1[l,2]))+CHI[1,3]
}


for(l in 1:74){
CHI[2,3]<-(((df2[l,2]-dfP2[l,2])^2)/(dfP2[l,2]))+CHI[2,3]
}










#----graficos de puntos junto lineas.--------------------
p = ggplot() + 
  geom_point(data = df1, aes(x = x, y = y, colour = datos)) +
   geom_line(data = dfG1, aes(x = x, y = y, colour= datos))+
  geom_line(data = dfP1, aes(x = x, y = y, colour= datos))+
   theme_bw()+theme(legend.key.size = unit(1.15, "cm"),legend.key.width = unit(1,"cm"))+
   xlab('Rango de los iones[Angstrom]') + ylab('Probabilidad')+
    ggtitle("Ajuste de datos para prueba con 30 kev")


plot(p)
#---------------------------------


#----graficos de puntos junto lineas.--------------------
p = ggplot() + 
  geom_point(data = df2, aes(x = x, y = y, colour = datos)) +
   geom_line(data = dfG2, aes(x = x, y = y, colour= datos))+
  geom_line(data = dfP2, aes(x = x, y = y, colour= datos))+
   theme_bw()+theme(legend.key.size = unit(1.15, "cm"),legend.key.width = unit(1,"cm"))+
   xlab('Rango de los iones[Angstrom]') + ylab('Probabilidad')+
    ggtitle("Ajuste de datos para prueba con 50 kev")


plot(p)
#---------------------------------

#los valores para chi-cuadrada se muestran
CHI