# Instalamos librer�as en caso de que no se tengan. 
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
# Todas las pruebas de hip�tesis anterior se realizaran con un 95% de confianza.
#######
################################################################################
## ##                                                                         ## 
## ##                         Problem�tica 1                                  ##
## ##                                                                         ##
################################################################################

# Seleccionamos matem�ticas de 5 y 11.
math_5 = datos2$`Percentil Matem�ticas 5`
math_11 = datos2$`Percentil Matem�ticas 11`

# Sacamos la media de los respectivos datos seleccionados.
meanm_5 = mean(math_5)
meanm_11 = mean(math_11)

# Sacamos la varianza de los datos seleccionados.
varm_5 = var(math_5)
varm_11 = var(math_11)

# Prueba de Hip�tesis:
# H0: mu1 - mu2 = 0
# Ha: mu1 - mu2 < 0
# Tomamos mu1 como la media de la poblaci�n de Percentil Matem�ticas 5 y 
# mu2 como la del Percentil Matem�ticas 11.

# Formulamos nuestro estad�stico de prueba Z con los datos encontrados anteriormente.

n_m1 = length(math_5) # Poblaci�n de matem�ticas en quinto.
n_m2 = length(math_11) # Poblaci�n de matem�ticas en once.

Zm = (meanm_5 - meanm_11)/sqrt(varm_5/n_m1 + varm_11/n_m2)
valorpm = pnorm(Zm) # El valor p de la prueba Z en matem�ticas.

# Rechazamos H0, es decir, dado que el valor p = 1.0499e-05. Esto nos indica que 
# el valor p < 0.001 (***) y por ende podemos concluir que en los colegios no oficiales
# que superaron el percentil 35 a nivel pa�s tuvieron un progreso en la educaci�n en 
# matem�ticas en el paso del tiempo de primaria a secundaria.


################################################################################
## ##                                                                         ## 
## ##                             Problem�tica 2                              ##
## ##                                                                         ##
################################################################################

# Ahora compararemos lenguaje y lectura cr�tica entre quinto y once respectivamente, 
# haciendo el mismo procedimiento anterior.

# Seleccionamos lenguaje y lectura cr�tica de 5 y 11 respectivamente.
leng_5 = datos2$`Percentil Lenguaje 5`
leng_11 = datos2$`Percentil Lectura Cr�tica 11`

# Sacamos la media de los respectivos datos seleccionados.
meanl_5 = mean(leng_5)
meanl_11 = mean(leng_11)

# Sacamos la varianza de los datos seleccionados.
varl_5 = var(leng_5)
varl_11 = var(leng_11)

# Prueba de Hip�tesis:
# H0: mu1 - mu2 = 0
# Ha: mu1 - mu2 < 0
# Tomamos mu1 como la media de la poblaci�n de Percentil Lenguaje 5 y 
# mu2 como la del Percentil Lectura cr�tica 11.

# Formulamos nuestro estad�stico de prueba Z con los datos encontrados anteriormente.

n_l1 = length(leng_5) # Poblaci�n de lenguaje en quinto.
n_l2 = length(leng_11) # Poblaci�n de lectura cr�tica en once.

Zl = (meanl_5 - meanl_11)/sqrt(varl_5/n_l1 + varl_11/n_l2)
valorpl = pnorm(Zl) # El valor p de la prueba Z en Lenguaje.

# Rechazamos H0, es decir, dado que el valor p = 0.0036. Esto nos indica que 
# el valor p < 0.05 (**) y por ende podemos concluir que en los colegios no oficiales
# que superaron el percentil 35 a nivel pa�s tuvieron un progreso en la educaci�n en 
# lenguaje en el paso del tiempo de primaria a secundaria.


# Entre las Pruebas de Hip�tesis 1 y 2 podemos concluir que aunque en ambos hubo un 
# avance con respecto al tiempo, los colegios se enfocar m�s en subir el percentil
# de matem�ticas 

