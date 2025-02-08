#TALLER 1-----------------------------------------------------------------------
#Integrantes:  Gabriel Fernandez y Angelica Sierra
##Ejercicio  1. Pelicula Star Wars----
#enuncia A. Construye una matriz llamada star wars 
#matrix de  2×3 que combine los 3 vectores
new_hope=c(460.998,314.4)
empire_strikes=c(290.475,247.900)
return_jedi=c(309.306,165.8)
star_wars_matrix=matrix(c(new_hope,empire_strikes,return_jedi),nrow=2,ncol=3,byrow=FALSE)
star_wars_matrix=t(star_wars_matrix)
star_wars_matrix
#Enunciado B. Renombra las filas (”US”,”non-US”) y 
#columas(”A New Hope”, ”The empire Strikes Back”,”Return of the Jedi”)
colnames(star_wars_matrix) <- c('US', 'non-US')
row.names(star_wars_matrix) <- c('A New Hope', 'The Empire Strikes Back', 'Return of the Jedi')
print(star_wars_matrix)
#Enunciado C. Calcule las cifras de la taquilla mundial de las tres pelıculas y guarde en  worldwide vector
worldwide_vector=c(rowSums(star_wars_matrix))
#worldwide_vector=c(775.398, 538.375, 475.106)
#Enunciado D.Añade worldwide vector como una nueva columna a la matriz
# usamos la funcion cbind para combinar columnas tambien podemos usar crow para combinar filas. 
star_wars_matrix=cbind(star_wars_matrix, worldwide_vector)
all_wars_matrix=star_wars_matrix
all_wars_matrix
#Enunciado E. Calcule los ingresos non-US Totales de las peliculas y el promedio. 
#Cuando queremos sumar una columna especifica no podemos sumar colsum porque esto es para vtoda la matriz. 
non_US_ALL=(star_wars_matrix[, 2])
non_US_ALL
Mean_non_us_all=mean(all_wars_matrix[, 2])
Mean_non_us_all
##Ejercicio  2. Sistema Solar --------------------------------------------------
name=c('Mercury', 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus','Neptune')
type=c('Terrestrial planet', 'Terrestrial planet', 'Terrestrial planet', 'Terrestrial planet', 'Gas giant', 'Gas giant', 'Gas giant”', 'Gas giant')
diameter=c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation=c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rings=c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
#Para crear el data Frame
planets_df=data.frame(name, type, diameter, rings)
str(planets_df)
#Enunciado  A.
#Para extraer un dato especifico.
mercury_diameter=subset(planets_df, name == "Mercury")$diameter
print(mercury_diameter)
#Para extraer todos los datos.
#Otra forma con corchetes seria mars_data=planets_df[planets_df$name == "Mars", ]
Mars_Data=subset(planets_df, name == "Mars")
#Selecciona e imprima los 5 primeros valores  del diametro
Five_Diameter=head(diameter,n=5)
print(Five_Diameter)
#Tres formas de seleccionar la variable rings y asignero a Rings_Vector
rings_vector= planets_df[, "rings"]
print(rings_vector)
rings_vector=subset(planets_df, select = rings)[,1]
print(rings_vector)
planets_df$rings
print(rings_vector)

#Seleccion Todos los planetas que tienen anillos
# Extraer los nombres de los planetas con anillos
planets_with_rings_names=planets_df$name[rings_vector == TRUE]
# Extraer el vector de anillos filtrado
rings_filtered=rings_vector[rings_vector == TRUE]
# Asociar nombres a los valores de anillos usando `names()`
names(rings_filtered)=planets_with_rings_names
# Mostrar el vector final
print(rings_filtered)
#Utilice subset() en planets df para seleccionar los planetas que tienen diametro <1
diameter_less_Earth=subset(planets_df, diameter <1)$name
#Ordenar los planetas por diametro de menor a mayor
Planets_Order=planets_df$name[order(planets_df$diameter)]
print(Planets_Order)

##Ejercicio  3. Sobre el Cine, Actrices, Actores---------------------------------
#Crea el codigo en el editor y asignala a sp man no way home list

mov="Spider-Man"
act=c("Tom Holland", "Ariana Grande", "Jackie Chan", "Chayanne", "DiCaprio")
reviews=data.frame(scores = c(3, 4.8, 5),sources = c("Netlifx", "A", "B"),comments = c("pelicula regular", "Buenas escenas", "Colorimetría exquisita"))
sp_man_no_way_home_list=list(moviename = mov,actors = act,reviews = reviews)
print(sp_man_no_way_home_list)
sp_man_no_way_home_list$actors
sp_man_no_way_home_list$actors[2]

#TALLER 2--------------------------------------------------------------------------
##Ejercicio 1. Swimming Pools----
# Importar Swimming_pools.csv: pools
setwd("C:/Users/Angelica Elena/Desktop/Estadistica Computacional/dataset")
df_Angie=read.csv("swimming_pools.csv",stringsAsFactors = TRUE)
# imprimir la estructura de pools
str(df_Angie)
summary(df_Angie)
print(df_Angie)


##Ejercicio 2. Hotdogs -------------------------------------------------------------
# Importar hotdogs.txt
df_hotdogs_txt=read.delim("hotdogs.txt",sep="",stringsAsFactors = FALSE)
str(df_hotdogs_txt)
summary(df_hotdogs_txt)
# Resumen hotdogs
summary(df_hotdogs_txt)
print(df_hotdogs_txt)

##Ejercicio 3. hotdogs con enunciados-------------------------------------------------
#Importe hotdogs.txt file con read.table 
df_Hotdogs_TABLE=read.table("hotdogs.txt",header=TRUE,sep="",stringsAsFactors = TRUE)
str(df_Hotdogs_TABLE)
#coloque los titulos a las columnas (type,calories,sodium)usaremosla funcion setNames:
df_Hotdogs_TABLE= setNames(df_Hotdogs_TABLE,c("type", "calories", "sodium"))
summary(df_Hotdogs_TABLE)
#Muestre 7 filas de hotdogs
df_Hotdogs_TABLE[1:7, c("type", "calories","sodium")]
#Selecciona el perro caliente con menos calorías (which.min): lily
max_calories= -Inf  
max_index=1  
for (i in 1:nrow(df_Hotdogs_TABLE)) {
  if (df_Hotdogs_TABLE[i, "calories"] > max_calories) {
    max_calories=df_Hotdogs_TABLE[i, "calories"]
    max_index =i  
  }
}
Lili=df_Hotdogs_TABLE[max_index, c("type", "calories","sodium")]# Extrae el tipo y calorías
print(Lili)
print(paste("Levels:", paste(levels(df_Hotdogs_TABLE$type), collapse = " ")))
# Selecciona el perro caliente con más calorías (which.max): tom
min_calories= -Inf  
min_index=1  
for (i in 1:nrow(df_Hotdogs_TABLE)) {
  if (df_Hotdogs_TABLE[i, "calories"] < min_calories) {
    min_calories=df_Hotdogs_TABLE[i, "calories"]
    min_index =i  
  }
}
Tom=df_Hotdogs_TABLE[min_index, c("type", "calories")]# Extrae el tipo y calorías
print(Tom)
print(paste("Levels:", paste(levels(df_Hotdogs_TABLE$type), collapse = " ")))

##Ejercicio 4. Urban Pop --------------------------------------------------------------
# imprime los nombres de todos los sheets de los datos urbanpop.xlsx
library(readxl)
sheets=excel_sheets("urbanpop.xlsx")
# carga todos los sheet, 1 por 1 y ponlos en una lista: pop_list
df_1960_1996= read_excel("urbanpop.xlsx",sheet = "1960-1966")
df_1967_1974= read_excel("urbanpop.xlsx",sheet = "1967-1974")
df_1975_2011= read_excel("urbanpop.xlsx",sheet = "1975-2011")
pop_list = list(
  "1960-1966" = df_1960_1996,
  "1967-1974" = df_1967_1974,
  "1975-2011" = df_1975_2011
)
# realiza los anterior con la funcion lapply
pop_list=lapply(sheets, read_excel, path = "urbanpop.xlsx")
# muestra la estructura de pop_list
str(pop_list)
# Importe la segunda hoja de "urbanpop.xlsx", pero omita las primeras 21 filas.
read_excel("urbanpop.xlsx", sheet = "1967-1974", skip = 21)

##Ejercicio 5. Densidad --------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)
counties=readRDS(file="C:/Users/Angelica Elena/Desktop/Estadistica Computacional/2. Manipulación/dataset/counties.rds")
counties#imprimir
counties$state=factor(counties$state)
counties %>%  
  # selecciona las columnas (state, county, population, land_area)
  select(state, county, population, land_area) %>% 
  # realiza grupos por estados
  group_by(state) %>%
  # calcula el total del area y el total de la poblacion
  summarize(totalpol = sum(population),
            totalar = sum(land_area))%>%
  # Adiciona la columna density: es el numero de personas por metro cuadrado
  mutate(density= totalpol / totalar)%>%
  # Ordena de manera descendiente density
  count(state, wt = density, sort = TRUE)

##Ejercicio 6. Trabajadores que Caminan--------------------------------------------
counties %>%
  # selecciona las columnas (region, state, county, metro, population, walk)
  select(region, state, county, metro, population, walk) %>% 
  # realiza grupos por region
  group_by(region) %>%
  # Encuentra el numero mas grande ciudadanos que caminan para trabajar
  top_n(1,walk)

##Ejercicio 7. Ingreso mas Alto---------------------------------------------------
counties %>%
  # selecciona las columnas (region, state, county, metro, population, walk)
  select(region, state, county, metro, population, walk, income) %>% 
  # realiza grupos por region y estado
  group_by(region,state) %>%
  # Calcula el promedio income
  summarize(prom_income = mean(income))%>%
  # Encuentre el ingreso mas alto por estado en cada region
  top_n(1,prom_income)

##Ejercicio 8. Personas en areas Metrop y No Metrop--------------------------------------
counties %>%
  # selecciona las columnas (state, county, population, land_area) 
  select(state, county, population, metro, land_area) %>%
  # Encuentre la poblacion total por la cada estado y metro
  group_by(state, metro) %>%
  mutate(total_population = sum(population)) %>%
  # Extrae la fila con mas poblaci?n por estado 
  top_n(1,total_population)%>%
  # # Cuenta los estados con mas personas en areas metropolitanas o no metropolitanas
  count(metro, wt = total_population, sort = TRUE)

##Ejercicio 9. Personaldel trabajo Publico--------------------------------------------
counties %>%
  # Selecciona el estado, el condado, la población y los que terminan en "trabajo
  select(state, county, population, ends_with("work")) %>%
  # Filtrar los condados que tienen al menos el 50% de la población dedicada al trabajo público
  filter(public_work>=50)


##Ejercicio 10. Renombre de Columna n-------------------------------------------------
counties %>%
  # Cuente el número de condados de cada estado
  select(state, county) %>% group_by(state) %>%   count(state) %>% 
  # renombrar la columna n por num_counties
  rename(num_counties = n)

##Ejercicio 11. Densidad Ordenada------------------------------------------------------
counties %>%
  # Mantenga las columnas de state, state, county, y population, 
  # y agregue la columna density (poblacion por metro cuadrado (land_area))
  transmute(state, county, population, density  = population/land_area) %>% 
  # Filtrar por county con una población superior a un millón 
  filter(population>1000000) %>% 
  # Ordena la densidad en orden ascendente 
  arrange(density)











