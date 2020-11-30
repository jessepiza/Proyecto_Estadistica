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
## ##                         Pruebas de Hipótesis 1                          ##
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

# Rechazamos H0, es decir, dado que el valor p = 1.0499 e-05. Esto nos indica que 
# el valor p < 0.001 (***) y por ende podemos concluir que en los colegios no oficiales
# que superaron el percentil 35 a nivel país tuvieron un progreso en la educación en 
# matemáticas en el paso del tiempo de primaria a secundaria.


################################################################################
## ##                                                                         ## 
## ##                         Pruebas de Hipótesis 2                          ##
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


################################################################################
## ##                                                                         ## 
## ##                         Pruebas de Hipótesis 3                          ##
## ##                                                                         ##
################################################################################

# Miraremos la mejora del colegio en matemáticas y lenguaje en tres secciones:
# 1. De tercero a Quinto.
# 2. De Quinto a Noveno.
# 3. De Noveno a Once.
# Luego Observaremos el valor p de cada una de las pruebas de hipótesis para 
# determinar en cuál intervalo hubo mejores resultados en las pruebas de ICFES.


###############################################################################
#                            Sección 1 - Matemáticas                         #
###############################################################################

# Realizamos la prueba de hipótesis entre tercero y quinto.
math_3 = datos2$`Percentil Matemáticas 3`
math_5 = datos2$`Percentil Matemáticas 5`

# Medias de tercero y quinto en el percentil de matemáticas respectivamente.
meanm_3 = mean(math_3) 
meanm_5 = mean(math_5)

# Varianzas de tercero y quinto en el percentil de matemáticas respectivamente.
varm_3 = var(math_3) 
varm_5 = var(math_5)

# Población de tercero y quinto
n_m3 = length(math_3)
n_m5 = length(math_5)

# Prueba de hipótesis: 
# mu3 y mu5 corresponden a las medias de tercero y quinto respectivamente.
# H0: mu3 - mu5 = 0
# H0: mu3 - mu5 < 0

Z1m = (meanm_3 - meanm_5)/sqrt(varm_3/n_m3 + varm_5/n_m5)
valorp_Z1m = pnorm(Z1m)
cat(valorp_Z1m)

# De esta manera, tenemos que el valorp_Z1m = 0.099 por lo que no rechazo H0.
# Esto indica que los colegios a nivel país no mejoraron entre tercero y quinto
# en el percentil de matemáticas.


###############################################################################
#                            Sección 2 - Matemáticas                         #
###############################################################################

# Realizamos la prueba de hipótesis entre quinto y noveno.
math_5 = datos2$`Percentil Matemáticas 5`
math_9 = datos2$`Percentil Matemáticas 9`

# Medias de quinto y noveno en el percentil de matemáticas respectivamente.
meanm_5 = mean(math_5)
meanm_9 = mean(math_9) 

# Varianzas de quinto y noveno en el percentil de matemáticas respectivamente.
varm_5 = var(math_5)
varm_9 = var(math_9) 

# Población de quinto y noveno.
n_m5 = length(math_5)
n_m9 = length(math_9)

# Prueba de hipótesis: 
# mu5 y mu9 corresponden a las medias de quinto y noveno respectivamente.
# H0: mu5 - mu9 = 0
# H0: mu5 - mu9 < 0

Z2m = (meanm_5 - meanm_9)/sqrt(varm_5/n_m5 + varm_9/n_m9)
valorp_Z2m = pnorm(Z2m)
cat(valorp_Z2m)

# Así, el valorp_Z2m = 3.081599e-22, es decir valorp_z2m < 0.001 y rechazo H0.
# Esto indica que los colegios a nivel país sí mejoraron entre quinto y noveno
# en el percentil de matemáticas.


###############################################################################
#                            Sección 3 - Matemáticas                         #
###############################################################################

# Realizamos la prueba de hipótesis entre noveno y once.
math_9 = datos2$`Percentil Matemáticas 9`
math_11 = datos2$`Percentil Matemáticas 11`

# Medias de noveno y once en el percentil de matemáticas respectivamente.
meanm_9 = mean(math_9) 
meanm_11 = mean(math_11)

# Varianzas de noveno y once en el percentil de matemáticas respectivamente.
varm_9 = var(math_9) 
varm_11 = var(math_11)

# Población de noveno y once.
n_m9 = length(math_9)
n_m11 = length(math_11)

# Prueba de hipótesis: 
# mu9 y mu11 corresponden a las medias de noveno y once respectivamente.
# H0: mu11 - mu9 = 0
# H0: mu11 - mu9 < 0

Z3m = (meanm_11 - meanm_9)/sqrt(varm_9/n_m9 + varm_11/n_m11)
valorp_Z3m = pnorm(Z3m)
cat(valorp_Z3m)

# Así, el valorp_Z2m = 2.578444e-07, es decir valorp_z2m < 0.001 y rechazo H0.
# Esto indica que los colegios a nivel país sí mejoraron entre quinto y noveno
# en el percentil de matemáticas.
