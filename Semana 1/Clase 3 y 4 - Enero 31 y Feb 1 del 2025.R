#Clase 31 de enero del 2025
#Ejercicio 1
#Todas las condiciones del medio van con ELSE IF 
#Si o si inicio con IF y finalizo el codigo con ELSE.
LI=15
FB=9
if(LI>=15&FB>=15){
  sms=2 * (LI + FB)
}else if(LI<10&FB<10){
  sms=0.5 * (LI + FB)
}else{
  sms=LI+FB
}
print(sms)

#EJEMPLO 1----
ctr=1
while(ctr<=7){
  print(paste("ctr se ajusta a",ctr))
  ctr=ctr+1
}
#EJEMPLO 2----
resultados=c('cara','sello')
num.lanza=0
num.caras=0
historial=NULL
while(num.caras<7){
  res=sample(x=resultados,size=1) #en esta linea use sample que es para simular el lanzamiento
  num.lanza=num.lanza+1# en esta linea poner contador de num.lanza
  historial[num.lanza]=res #guarde los lanzamientos en la variable historial
  # use el condicional if , para que haga el conteo de caras
  if(res=='cara'){
    num.caras=num.caras+1
  }
}
historial
##EJEMPLO 3----
ctr=1
while(ctr<=7){
  if(ctr%%5==0){
    break
  }
  print(paste("ctr se ajusta a",ctr))
  ctr=ctr+1
}

##EJEMPLO 7----
speed=64
while(speed>30){
  print(paste('Despacio! su velocidad es de',speed))
  speed=speed-7
}
print(paste('su velocidad Actual es de',speed,', mantengala asi'))

#FOR- CICLO DE CONTROL ITERATICA
cities=c('New York','Paris','London','Tokyo', 'Rio de Janeiro','Cape Town')
for(city in cities){
  print(city)
}

##EJEMPLO 8----

for(city in cities){
  if(nchar(city)%%6==0){
    break
  }
  print(city)
}

##EJEMPLO 9----
for(city in cities){
  if(nchar(city)%%6==0){
    next
  }
  print(city)
}
##EJEMPLO 10----
nrep=20#numero de muestras
n=1000#Tamaño de mi muestra
conteo=numeric(nrep)
for(i in 1:nrep){
  x=runif(n,min = 1,max=3)
  conteo[i]=sum(x>=2.5)
}
print(conteo)
 

#FUNCIONES: Conjunto de Instrucciones
##EJEMPLO 11----
#parte a
suma=function(x,y){
  res=x+y
  return(res)
}
suma(1,3)
#parte b

fun=function(){
  suma=0
  veces=0
  while(suma<=3){
  suma=suma+runif(1)
  veces=veces+1
  }
  return(veces)
}
fun()

#FUNCIONES:LAPPLY
#ejemplo 12
nyc=list(pop = 8405837,boroughs = c('Manhattan','Bronx','Brooklyn','Queens','Staten Island'),capital = FALSE)
for ( info in nyc){
  print ( class ( info ))
}
#Esto es para una litsa
unlist(lapply(nyc,class))
#este es para un vector
sapply(nyc,class)
#ejemplo 12.1- corroborar
cities=c('New York','Paris','London','Tokyo', 'Rio de Janeiro','Cape Town')
num_chars=c()
for(i in 1: length(cities)){
  num_chars[i]=nchar(cities [i ])
}
lapply (cities ,nchar)
unlist (lapply (cities,nchar))

#ejemplo 15.Expresiones regulares
animales=c('gato','raton','hormiga','oso','tigre')
grepl(pattern = 'a', x = animales)
grep(pattern = 'a', x = animales)
grepl(pattern = '^o', x = animales)
sub(pattern = 'a',replacement = 'o',x = animales)
today=Sys.Date()











