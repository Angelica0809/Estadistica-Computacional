library(tidyverse)
4+4
#Ejemplo a----
a=3
4->b
d<-5
b<-6
A<-5
#Clase 1- 24 de enero del 2025
#ejemplo 1 ----
errores<-c(1.9,-2.6,4.0,-9.5,-3.4,7.3)
sum(abs(round(errores)))
vec1<-c(1.5,2.5,8.4,3.7,6.3)
vec2<-rev(vec1)
mean(x=c(abs(vec1),abs(vec2)),trim=0.2)
#ejemplo 2 ----
poker_vector<-c(140,-50,20,-120,240)
roulette_vector<-c(-24,-50,100,-350,10)
days_vector<-c('Monday','Tuesday','Wednesday','Thursday','Friday')
#ejemplo 3----
poker_wednesday<-poker_vecto[3]
poker_midweek<-poker_vecto[c(2,3,4)]
roulette_select_vector<-roulette_vector[2:5]

#ejemplo 4----
x<-c(2.1,4.2,3.3,5.4)#estoy creando una variable que es un vector 
x[c(3,1)]
x[c(3,1,1)]#concatenar, une o extrae los valores de esas posiciciones
order(x)#Ordeno de mayor a menor las posiciones 
rev(x[order(x)])
x[c(2.1,3.4)]


x[-1]
x[-c(1,3)]

#Clase 2 - 25 de Enero del 2025
#ejemplo 6----
x[c(T,F)]
#si coloco F no me va a mostrar los datos, verifica bien que me quiere decir esto
x[c(T,F,NA,F)]
x[c(T,NA)]

#Vector de caracteres: si el vector tiene nombre, tambien puede utilizar vectores de caracteres para devolver elementos con nombres coincidentes.
#ejemplo 7----
y<-setNames(x,letters[1:4])
y[c('a','c','a')]
x[y[c('a','a','a')]]

#Orden y nombre
#ejemplo 8----
sort(x=poker_vector,decreasing = TRUE)#si le coloco  True, ira en descendente
names(poker_vector)=c('Monday','Tuesday','Wednesday','Thursday','Friday')
poker_vector
#Asigna los dıas como nombres de roulette vector usando la variable days vector
names(roulette_vector)<-days_vector
#Asigna a total diario cuanto has ganado/perdido en cada dıa
total_diario<-poker_vector+roulette_vector
total_diario
#Calcula los totales de la ruleta y el poker, y despues calcula el total de la semana y asignalo a total week
total_poker<-sum(poker_vector)
total_roulette<-sum(roulette_vector)
total_week<-total_poker+total_roulette
total_week
#Calcula el promedio de los resultados del poker de los dıas Lunes,Martes y Miercoles
mean(poker_vector[2:4])

#EJERCICIO 1
#Asignamos el vector a una variable y aplicamos la ecuacion del ejercicio. 
vecA<-c(3,4,6,9,3,9,10,7,7,2)
vecA
n<-length(vecA)
(1/(n-1))*sum(vecA)

#ejemplo 9----
2 == 2
'a'<='b'
'Hello'>'Goodbye'
TRUE < FALSE
5+3==8 | 5==8/2
#¿Que dıas has ganado dinero con el poker? Seleccionelos
poker_vector>0
poker_vector[poker_vector>0]
#¿Que dıas has perdido dinero con la ruleta? Seleccionelos
roulette_vector[roulette_vector<0]
x>3
x[x>3]# es para seleccionar y extraer los valores de la condicion  respecto a un vector.
#ejemplo 10----
seq(1,10)#vamos a generar una secuencia.
1:10
seq(1,10,by=2)#La generamos pero de dos en dos. 
rep(1:4,4)
rep(c('x','y','z'),3)# La c es para crear un vecctor. 

#Vamos a hacer una distribucion normal, vamos a buscar unos datos con media igual 10 y varianza cuatro.

mu<-10
sigmma<-2
set.seed(12)
rnorm(n=10,mean=10,sd=2)

#MATRICES
#ejemplo 11----
A<-matrix(1:9,nrow=3,byrow=TRUE)
dim(A)
B<-matrix(c(1,2,3,-1,2,-5,8,3,2),nrow=3,ncol=3,byrow=FALSE)
dim(B)
A[-2,-2]#Estoy quitando la fila 2 y la columna 2, tambien se puede con concatenar.
A[c(1,2),c(1,2)]
A+B
A-B
2*A + B
A%*%B# Esta es la multiplicacion de matrices
det(A)#Determinante de A
det(B)#Determinante de B
solve(B)#Calculo de la inversa
D=matrix(c(1,1,3,-2),nrow=2,ncol=2,byrow=T)
R=matrix(c(1,3),nrow=2,byrow=T)#Matriz de solucion
det(D)
solve(D)%*%R
solve(D,R)


#Factores
#Ejemplo 12----
sex_vector<-c('male','female','female','male','male')
summary(sex_vector)#Vamos a convertirlo en un factor ya que este resumen no es el que quiero.
factor_sex_vector<-factor(sex_vector)
summary(factor_sex_vector)#aca si me hara el conteo que yo quiero. 
temperature_vector<- c('High','Low','High','Low','Medium')
factor_temperature_vector<-factor(temperature_vector,ordered=TRUE,levels = c('Low','Medium','High'))

#Ejemplo 13----
df<-mtcars# libreria de datos
view(df)
df$vs<-factor(df$vs)
df$am<-factor(df$am)
summary(df)

head(df)#primeras 5 filas
tail(df)#Ultimas 5 filas
str(df) #Resumen de mis filas
#debo extraer los cilindrajes que son menores o iguales a 5
df[df$cyl<=5,]#me indica que me va a dar un vector logico.
df[df$cyl==5|df$cyl==6,]#Traigo los iguales a 5 o los iguales a 6
df$mgp[1]<-25

#Ejemplo 14----
my_vector<-1:10
my_matrix<-matrix(1:9,ncol=3)
my_df<-mtcars[1:10,]
my_list<-list(my_vector,my_matrix,my_df)
#como entramos al vector
my_list[[1]]
my_list[[2]]
my_list[[3]]

#ejemplo 15----
#ejecuta siempre y cuando se cumpla la condicion.
x<- -3
if(x<0){
  print('x es un numero negativo')
}

#ejemplo 16----

x<- 3
if(x<0){
  print('x es un numero negativo')
}else{
  print('x es un numero Positivo o Cero')
}
#Aca no usamos print porque no es compatible
x<-3
ifelse(x<0,
       ('x es un numero negativo'),
       ('x es un numero Positivo o Cero'))

#ejemplo 17----
if(x<0){
  print('x es un numero negativo')
}else if(x==0){
  print('x es cero')
}else{
  print('x es un numero postivo')
}
#OBSERVACION

  
  








