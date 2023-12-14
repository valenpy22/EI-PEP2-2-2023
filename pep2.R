# Cargar las librerías necesarias

library(car)
library(ggpubr)
library(lmtest)
library(tidyverse)
library(pROC)
library(leaps)
library(boot)


# Cargar los datos desde un archivo CSV
datos <- read.csv("Escritorio/EI-2023-2-PE2-Datos-Equipo04.csv")

# Ver si son independientes para ver si usar una prueba paramétrica o Wicolson .
# AL verificar que son realizaremos las pruebas pertinentes para cada caso .

# Se verifica la normalidad para ver si usar la paramétrica

#  Shapiro-Wilk para X2
shapiro_test_X2 <- shapiro.test(datos$X2)
print(shapiro_test_X2)

# Shapiro-Wilk para X6
shapiro_test_X6 <- shapiro.test(datos$X6)
print(shapiro_test_X6)

# Histogramas para ambos
p1 <- ggplot(datos, aes(x = X2)) + 
  geom_histogram(bins = 5, fill = 'blue', alpha = 0.7) +
  labs(title = "Histogram for X2", x = "X2", y = "Count")

p2 <- ggplot(datos, aes(x = X6)) + 
  geom_histogram(bins = 5, fill = 'red', alpha = 0.7) +
  labs(title = "Histogram for X6", x = "X6", y = "Count")

print(p1)
print(p2)

# Al ver los resultados de los p value de Shapiro se puede decir que hay que usar una prueba no paramétrica
# Se confirma que los promedios exhibidos por X3 y X6 están lejos de seguir una distribución normal. 
# Corresponde entonces usar una prueba no paramétrica para analizar estos datos. 
# En este caso, una prueba de Wilcoxon-Mann-Whitney, en reemplazo de una prueba T de Student para muestras independientes, con las siguientes hipótesis:

# Hipótesis a contrastar 

# H0 : no hay diferencia en los promedios nacionales para ambas variables X3 y X6.
# HA: sí hay diferencias en los promedios nacionales para ambos varianles X3 y X6.

# Verifiquemos que se cumplen las condiciones para aplicar esta prueba no paramétrica con validez:
  
# Procedamos entonces a realizar la prueba.

wilcox_test <- wilcox.test(datos$X3, datos$X6, paired = FALSE)

print(wilcox_test)

# Valor de p es  0,0005756
# Podemos concluir, entonces, que existe fuerte evidencia en contra de la hipótesis nula (W=1749,p<0,005), 
# por lo que la rechazamos en favor de la alternativa. 

# Se realizára una análisis inferencial con 95% de confianza ( alpha de 0,05 ) explicando el proceso .
# Se remuestrea con boostrapping con 300 personas.

set.seed(123) 

# Relizar bootsrapping
resultados <- boot(
  data = datos, 
  statistic = function(data, indices) {
    resampled_data <- data[indices, ]
    test_resultados <- wilcox.test(resampled_data$X3, resampled_data$X6, paired = FALSE)
    test_resultados$statistic
  }, 
  # Repeticiones de 300 
  R = 300 
)

print(resultados)


# La prueba de Shapiro-Wilk realizada en las variables X2 y X6 muestra que ambas se desvían
# significativamente de una distribución normal, lo que justifica el uso de una prueba no paramétrica.
# El test de Wilcoxon revela una diferencia significativa entre las medianas de X3 y X6, sugiriendo que
# no son iguales. El análisis de bootstrap confirma la fiabilidad de este resultado, mostrando un sesgo 
# pequeño y un error estándar razonable. En conclusión de que las medias nacionales de X3 y X6 son 
# significativamente diferentes.

