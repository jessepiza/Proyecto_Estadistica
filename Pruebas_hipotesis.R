# Instalamos librerías en caso de que no se tengan. 
# Si se tienen, continuar.
install.packages(c("readxl", "dplyr"))
library(readxl)
library(dplyr)

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

######
# Todas las pruebas de hipótesis anterior se realizaran con un 95% de confianza.
#######
################################################################################
## ##                                                                         ## 
## ##                         Problemática 1                                  ##
## ##                                                                         ##
################################################################################

# Seleccionamos matemáticas de 5 y 11.
math_5 = datos2$`Percentil Matemáticas 5`
math_11 = datos2$`Percentil Matemáticas 11`

# Sacamos la media de los respectivos datos seleccionados.
meanm_5 = mean(math_5)
meanm_11 = mean(math_11)

# Sacamos la varianza de los datos seleccionados.
varm_5 = var(math_5)
varm_11 = var(math_11)

# Prueba de Hipótesis:
# H0: mu1 - mu2 = 0
# Ha: mu1 - mu2 < 0
# Tomamos mu1 como la media de la población de Percentil Matemáticas 5 y 
# mu2 como la del Percentil Matemáticas 11.

# Formulamos nuestro estadístico de prueba Z con los datos encontrados anteriormente.

n_m1 = length(math_5) # Población de matemáticas en quinto.
n_m2 = length(math_11) # Población de matemáticas en once.

Zm = (meanm_5 - meanm_11)/sqrt(varm_5/n_m1 + varm_11/n_m2)
valorpm = pnorm(Zm) # El valor p de la prueba Z en matemáticas.

# Rechazamos H0, es decir, dado que el valor p = 1.0499e-05. Esto nos indica que 
# el valor p < 0.001 (***) y por ende podemos concluir que en los colegios no oficiales
# que superaron el percentil 35 a nivel país tuvieron un progreso en la educación en 
# matemáticas en el paso del tiempo de primaria a secundaria.


################################################################################
## ##                                                                         ## 
## ##                             Problemática 2                              ##
## ##                                                                         ##
################################################################################

# Ahora compararemos lenguaje y lectura crítica entre quinto y once respectivamente, 
# haciendo el mismo procedimiento anterior.

# Seleccionamos lenguaje y lectura crítica de 5 y 11 respectivamente.
leng_5 = datos2$`Percentil Lenguaje 5`
leng_11 = datos2$`Percentil Lectura Crítica 11`

# Sacamos la media de los respectivos datos seleccionados.
meanl_5 = mean(leng_5)
meanl_11 = mean(leng_11)

# Sacamos la varianza de los datos seleccionados.
varl_5 = var(leng_5)
varl_11 = var(leng_11)

# Prueba de Hipótesis:
# H0: mu1 - mu2 = 0
# Ha: mu1 - mu2 < 0
# Tomamos mu1 como la media de la población de Percentil Lenguaje 5 y 
# mu2 como la del Percentil Lectura crítica 11.

# Formulamos nuestro estadístico de prueba Z con los datos encontrados anteriormente.

n_l1 = length(leng_5) # Población de lenguaje en quinto.
n_l2 = length(leng_11) # Población de lectura crítica en once.

Zl = (meanl_5 - meanl_11)/sqrt(varl_5/n_l1 + varl_11/n_l2)
valorpl = pnorm(Zl) # El valor p de la prueba Z en Lenguaje.

# Rechazamos H0, es decir, dado que el valor p = 0.0036. Esto nos indica que 
# el valor p < 0.05 (**) y por ende podemos concluir que en los colegios no oficiales
# que superaron el percentil 35 a nivel país tuvieron un progreso en la educación en 
# lenguaje en el paso del tiempo de primaria a secundaria.


# Entre las Pruebas de Hipótesis 1 y 2 podemos concluir que aunque en ambos hubo un 
# avance con respecto al tiempo, los colegios se enfocar más en subir el percentil
# de matemáticas 

