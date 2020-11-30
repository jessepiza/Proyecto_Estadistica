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
## ##                           Problemática 4                                ##
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
# Ha: mu3 - mu5 < 0

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
# Ha: mu5 - mu9 < 0

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
# Ha: mu11 - mu9 < 0

Z3m = (meanm_11-meanm_9)/sqrt(varm_9/n_m9 + varm_11/n_m11)
valorp_Z3m = pnorm(Z3m)
cat(valorp_Z3m)

# Así, el valorp_Z3m = 2.578444e-07, es decir valorp_z3m < 0.001 y rechazo H0.
# Esto indica que los colegios a nivel país no mejoraron de noveno a once
# en el percentil de matemáticas.

# Ahora bien, haciendo el comparativo entre los valores p de cada una de las
# secciones obtenemos:
valores_pm = c(valorp_Z1m, valorp_Z2m, valorp_Z3m)
cat(which.min(valores_pm))

# Así, el valor p menor entre las pruebas Z realizadas es el de la sección 2.
# Esto indica que entre quinto y noveno los avances entre los colegios fueron
# mejores.


###############################################################################
#                            Sección 1 - Lenguaje                             #
###############################################################################

# Realizamos la prueba de hipótesis entre tercero y quinto.
leng_3 = datos2$`Percentil Lenguaje 3`
leng_5 = datos2$`Percentil Lenguaje 5`

# Medias de tercero y quinto en el percentil de lenguaje respectivamente.
meanl_3 = mean(leng_3) 
meanl_5 = mean(leng_5)

# Varianzas de tercero y quinto en el percentil de lenguaje respectivamente.
varl_3 = var(leng_3) 
varl_5 = var(leng_5)

# Población de tercero y quinto
n_l3 = length(leng_3)
n_l5 = length(leng_5)

# Prueba de hipótesis: 
# mu3 y mu5 corresponden a las medias de tercero y quinto respectivamente.
# H0: mu3 - mu5 = 0
# Ha: mu3 - mu5 < 0

Z1l = (meanl_3 - meanl_5)/sqrt(varl_3/n_l3 + varl_5/n_l5)
valorp_Z1l = pnorm(Z1l)
cat(valorp_Z1l)

# De esta manera, tenemos que el valorp_Z1l = 0.0099 por lo que rechazo H0.
# Además, el valor p de esta prueba es < 0.05 (**)
# Esto indica que los colegios a nivel país sí mejoraron entre tercero y quinto
# en el percentil de lenguaje.


###############################################################################
#                            Sección 2 - Lenguaje                             #
###############################################################################

# Realizamos la prueba de hipótesis entre quinto y noveno.
leng_5 = datos2$`Percentil Lenguaje 5`
leng_9 = datos2$`Percentil Lenguaje 9`

# Medias de quinto y noveno en el percentil de lenguaje respectivamente.
meanl_5 = mean(leng_5)
meanl_9 = mean(leng_9) 

# Varianzas de quinto y noveno en el percentil de lenguaje respectivamente.
varl_5 = var(leng_5)
varl_9 = var(leng_9) 

# Población de quinto y noveno.
n_l5 = length(leng_5)
n_l9 = length(leng_9)

# Prueba de hipótesis: 
# mu5 y mu9 corresponden a las medias de quinto y noveno respectivamente.
# H0: mu5 - mu9 = 0
# Ha: mu5 - mu9 < 0

Z2l = (meanl_5 - meanl_9)/sqrt(varl_5/n_l5 + varl_9/n_l9)
valorp_Z2l = pnorm(Z2l)
cat(valorp_Z2l)

# Así, el valorp_Z2l = 6.754265e-13, es decir valorp_z2l < 0.001 y rechazo H0.
# Esto indica que los colegios a nivel país sí mejoraron entre quinto y noveno
# en el percentil de matemáticas.


###############################################################################
#                            Sección 3 - Lenguaje                             #
###############################################################################

# Realizamos la prueba de hipótesis entre noveno y once.
leng_9 = datos2$`Percentil Lenguaje 9`
leng_11 = datos2$`Percentil Lectura Crítica 11`

# Medias de noveno y once en el percentil de lenguaje y lectura crítica
# respectivamente.
meanl_9 = mean(leng_9) 
meanl_11 = mean(leng_11)

# Varianzas de noveno y once en el percentil de lenguaje y lectura crítica
# respectivamente.
varl_9 = var(leng_9) 
varl_11 = var(leng_11)

# Población de noveno y once.
n_l9 = length(leng_9)
n_l11 = length(leng_11)

# Prueba de hipótesis: 
# mu9 y mu11 corresponden a las medias de noveno y once respectivamente.
# H0: mu11 - mu9 = 0
# Ha: mu11 - mu9 < 0
Z3l = (meanl_11-meanl_9)/sqrt(varl_9/n_l9 + varl_11/n_l11)
valorp_Z3l = pnorm(Z3l)
cat(valorp_Z3l)

# Así, el valorp_Z3l = 1.64622e-05, es decir valorp_z3l < 0.001 y rechazo H0.
# Esto indica que los colegios a nivel país no mejoraron de noveno a once
# en el percentil de Lenguaje.

# Ahora bien, haciendo el comparativo entre los valores p de cada una de las
# secciones obtenemos:
valores_pm = c(valorp_Z1m, valorp_Z2m, valorp_Z3m)
cat(which.min(valores_pm))

# Así, el valor p menor entre las pruebas Z realizadas es el de la sección 2.
# Esto indica que entre quinto y noveno en Lenguaje en el ICFES, los avances 
# entre los colegios fueron mejores.


#####################################################################################
# En ambos casos podemos concluir que hay una mejoría  en los puntajes del icfes en #
# matemáticas y Lenguaje entre las pruebas de quinto y noveno a nivel nacional.     #
#####################################################################################