rm(list = ls()) # Borramos los datos.

delete_naceldas <- function(datos) {
  # Borramos todas las filas que contengan al menos un na.
  datos[rowSums(is.na(datos)) <= 0,]
}

setwd("C:/Universidad/Cuarto Semestre/Estadistica/Proyecto_Estadistica")

# Cargamos los datos de la Base de datos del Icfes.
datos <- read_excel("Base.xlsx")

# Dejamos los colegios que tengan todos los datos completos.
datos2 <- delete_naceldas(datos)

################################################################################
## ##                                                                         ## 
## ##          Problemática 4  - Regresión Lineal                       ##
## ##                                                                         ##
################################################################################
# Seleccionamos matemáticas de 3 y 5.
math_3 = datos2$`Percentil Matemáticas 3`
math_5 = datos2$`Percentil Matemáticas 5`

mod = lm(math_5~math_3)
summary(mod)

plot(math_3, math_5, main = "Scatterplot")
abline(mod, col=4, lwd=2)

# Seleccionamos lenguaje de 3 y 5.
leng_3 = datos2$`Percentil Lenguaje 3`
leng_5 = datos2$`Percentil Lenguaje 5`

mod = lm(leng_5~leng_3)
summary(mod)

plot(leng_3, leng_5, main = "Scatterplot")
abline(mod, col=4, lwd=2)


# Seleccionamos matemáticas de 5 y 9.
math_5 = datos2$`Percentil Matemáticas 5`
math_9 = datos2$`Percentil Matemáticas 9`

mod = lm(math_9~math_5)
summary(mod)

plot(math_5, math_9, main = "Scatterplot")
abline(mod, col=4, lwd=2)

# Seleccionamos lenguaje de 5 y 9.
leng_5 = datos2$`Percentil Lenguaje 5`
leng_9 = datos2$`Percentil Lenguaje 9`

mod = lm(leng_9~leng_5)
summary(mod)

plot(leng_5, leng_9, main = "Scatterplot")
abline(mod, col=4, lwd=2)

# Seleccionamos matemáticas de 9 y 11.
math_9 = datos2$`Percentil Matemáticas 9`
math_11 = datos2$`Percentil Matemáticas 11`

mod = lm(math_11~math_9)
summary(mod)

plot(math_9, math_11, main = "Scatterplot")
abline(mod, col=4, lwd=2)

# Seleccionamos Lenguaje de 9 y 11.
leng_9 = datos2$`Percentil Lenguaje 9`
leng_11 = datos2$`Percentil Lectura Crítica 11`

mod = lm(leng_11~leng_9)
summary(mod)

plot(leng_9, leng_11, main = "Scatterplot")
abline(mod, col=4, lwd=2)

