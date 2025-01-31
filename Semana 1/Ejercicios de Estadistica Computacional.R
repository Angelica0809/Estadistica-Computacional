#EJERCICIO 1 y 2 y 3 TAREA----Proximo fin de Semana. 
#Ejercicio  1. Pelicula Star Wars----
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
worldwide_vector=rowSums(star_wars_matrix)
worldwide_vector=c(775.398, 538.375, 475.106)
#Enunciado D.Añade worldwide vector como una nueva columna a la matriz
# usamos la funcion cbind para combinar columnas tambien podemos usar crow para combinar filas. 
star_wars_matrix=cbind(star_wars_matrix, worldwide_vector)
all_wars_matrix=star_wars_matrix
all_wars_matrix
#Enunciado E. Calcule los ingresos non-US Totales de las peliculas y el promedio. 
#Cuando queremos sumar una columna especifica no podemos sumar colsum porque esto es para vtoda la matriz. 
non_US_ALL=sum(all_wars_matrix[, 2])
non_US_ALL
Mean_non_us_all=mean(all_wars_matrix[, 2])
Mean_non_us_all
#Ejercicio  2. Sistema Solar ----
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
print(rings_vector)
#Seleccion Todos los planetas que tienen anillos
# Extraer los nombres de los planetas con anillos
planets_with_rings_names=planets_df$name[planets_df$rings == TRUE]
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

#Ejercicio  3. Sobre el Cine, Actrices, Actores ----
#Crea el codigo en el editor y asignala a sp man no way home list

mov="Spider-Man: No Way Home"
act=c("Tom Holland", "Zendaya", "Benedict Cumberbatch", "Jacob Batalon", "Willem Dafoe")
reviews=data.frame(scores = c(8.7, 9.0, 8.5),sources = c("IMDB", "Rotten Tomatoes", "Metacritic"),comments = c("Great movie!", "Amazing visuals!", "Fantastic performances!"))
sp_man_no_way_home_list=list(moviename = mov,actors = act,reviews = reviews)
print(sp_man_no_way_home_list)
sp_man_no_way_home_list$actors
sp_man_no_way_home_list$actors[2]







