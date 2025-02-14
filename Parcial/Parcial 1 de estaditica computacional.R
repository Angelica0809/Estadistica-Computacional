#EJERCICIO 1--------------------------------------------------------------------
#Genere un vector aleatorio de distribucion de Poisson, 
#con numero de observaciones 40, y promedio de 8. Use set.seed(2025)

set.seed(2025)
vector=rpois(40,8)
print(vector)

##(A) Ordenelos de menor a mayor------------------------------------------------
Vector_Ordenado=sort(vector)
print(Vector_Ordenado)

##(B) Extraiga el octavo y veinteavo elemento del vector-----------------------
Vector_Ordenado[c(8,20)]

##(C) Extraiga los valores de las posiciones impares--------------------------
Valores_Pos_Imp=Vector_Ordenado[seq(1, length(Vector_Ordenado), by = 2)]
print(Valores_Pos_Imp)

##(D) Extraiga los valores que son mayores que 3, pero menores que 8.----------
Vector_Ordenado[c(Vector_Ordenado>3,Vector_Ordenado<8)]

#EJERCICIO 2--------------------------------------------------------------------
#Un cliente busca informacion sobre conjuntos de datos relacionados con 
#experimentos quımicos. Recuerda que sus nombres son: esoph y trees
#El cliente necesita que usted realice lo siguiente:

##(A) Indique en que paquete se encuentran estos conjuntos----------------------
#se encuentran en el paquete datasets, el cual esta incluido por defecto en R

?esoph 
data(esoph)
str(esoph)
View(esoph)

?trees
data(trees)
str(trees)
View(trees)

##(B)Proporcione una descripcion breve de las variables de cada conjunto--------

#Ya

##(C) Realice una caracterizacion de la variable Volumen------------------------
#segun Height (convertida en categorıas por cuantiles)en trees. 
#Use el paquete dplyr de tidyverse y elimine los valores NA (si lo hay)
#Volume segun Height
library(tidyverse)
library(dplyr)
cuantil= quantile(trees$Height, q= c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)
trees %>% 
  mutate(Height_Categorizada=case_when(
  Height<=cuantil[2] ~ "Peso Bajo",
  Height<=cuantil[3] ~ "Peso Medio",
  Height<=cuantil[4] ~ "Peso Alto",
  TRUE~ "Peso Muy Alto"
)) %>% group_by(Height_Categorizada) %>% 
  summarise(
    vol_prom = mean(Volume, na.rm = TRUE),
    vol_min = min(Volume, na.rm = TRUE),
    vol_max = max(Volume, na.rm = TRUE)
  )
 
#EJERCICIO 3--------------------------------------------------------------------
rquote <- "r’s internals are irrefutably intriguing"
chars <- strsplit(rquote, split="")[[1]]
Num_r <- 0
for (i in 1:length(chars)) {
  if (chars[i] == "u") {
    break  
  }
  if (chars[i] == "r") {
    Num_r <- Num_r + 1  
  }
}

print(paste("El número de 'r' antes de encontrar la primera 'u' es:", Num_r))

#EJERCICIO 4--------------------------------------------------------------------


extraccion=0
numero_mayor_90=0
historial=c()
while(numero_mayor_90<3){
  numero=sample(1:100,1)
  extraccion=extraccion+1
  historial=c(historial,numero)
  if(numero>90){
    numero_mayor_90=numero_mayor_90+1
  }
}

print(historial)
print(paste("El total de extracciones fue:",extraccion))

#EJERCICIO 5--------------------------------------------------------------------
##Solo nos especifican el valor de Beta, asumiremos el valor de Alpha como 1.
RIC=function(){
  x=rbeta(40,1,5)
  sort(x)
  Q1=x[round(0.25*length(x))]
  Q3=x[round(0.75*length(x))]
  ric=Q3-Q1
  print(paste("El Rango Intercualitico es:",ric))
}
RIC()


#EJERCICIO 6--------------------------------------------------------------------

##Cargue los datos germination y muestre las 3 primeras filas-------------------
setwd("C:/Users/Angelica Elena/Documents/GitHub/Estadistica-Computacional/Parcial")
datos=read.delim("germinacion.txt",sep="",stringsAsFactors = FALSE)
str(datos)
summary(datos)
head(datos,3)
print(datos)
##Cambie el nombre codificado de los dos genotipos al nombre comun de la planta----
## Use dplyr
library(dplyr)
datos =datos%>% 
  mutate(orobanche = if_else(orobanche == "a73", "Trebol", "Hiedra"))
print(datos)
##Extraiga todas las observaciones en las que el numero de semillas-----
##que germinaron sea distinto de 10 y muestre las 5 muestras mas grandes. 
datos_obs=datos %>% 
  filter(conteo!=10) %>%
  arrange(desc(conteo))
head(datos_obs,5)

##Tabla de  control de los fungicidas relevantes----


orobanche=c("a70", "a71", "a72", "Hiedra", "a74", "Trebol", "a76")
fungicida=c("Ecoticid-K1", "Ecoticid-K2", "Ecoticid-K3", "Ecoticid-K0","Ecoticid-K4", "Ecoticid-K9", "Ecoticid-K8")
tratamiento=data.frame(orobanche, fungicida, stringsAsFactors = FALSE)
control=datos %>%
  left_join(tratamiento, by = "orobanche")
print(control)

##PRUEBA 2

orobanche=c("a70", "a71", "a72", "Hiedra", "a74", "Trebol", "a76")
fungicida=c("Ecoticid-K1", "Ecoticid-K2", "Ecoticid-K3", "Ecoticid-K0", "Ecoticid-K4", "Ecoticid-K9", "Ecoticid-K8")
tratamiento=data.frame(orobanche, fungicida, stringsAsFactors = FALSE)
if ("fungicida" %in% names(datos)) {
  datos=datos %>% select(-fungicida)##Esto lo hago porque me estaba colocando otra columna de fungicida adicional a la que la estaba
}
control=datos %>%
  left_join(tratamiento, by = "orobanche")
print(control)
##¿cuantas semillas germinaron?----------
Conteo_Semillas=control %>%
  group_by(fungicida) %>%
  summarise(semillas_germinadas= sum(conteo, na.rm = TRUE)) %>%
  arrange(desc(semillas_germinadas))
print(Conteo_Semillas)

