rm(list = ls())
getwd()
setwd("E:/Escritorio/RRL/")
#### REGRESION Y CORRELACION ABRAHAM ZAMUDIO ####
#### Ejemplo Pag 7 ####

y <- c(5,5,4,9,7,11)
x <- c(3,2,2,4,3,7)
x
y
plot(x,y)

#Regresion lineal simple

reg_lin <- lm(y ~ x) #lm estimaciones para los parametros
reg_lin  #almacena la regresion
# Call:
#   lm(formula = y ~ x)
# 
# Coefficients:
#   (Intercept)            x  
# 2.133        1.343  
# donde tendemos: yi=2.133 + 1.343xi

# Para obtener mas innformacion del modelo de regresion
summary(reg_lin)
# F-statistic: 23.92 on 1 and 4 DF,  p-value: 0.008095
# Donde Pvalor = 0.008095 < 0.05 supone que los parametros serian diferentes
# de cero

# Para dibujar la recta de regresion lineal sobre el 
# diagrama de dispesion
plot(x,y)
abline(reg_lin)

xmin <- 0.9*min(x)
xmax <- 1.1*max(x)
ymin <- 0.9*min(y)
ymax <- 1.1*max(y)

png(filename = "SueldovsAñosdeExperiencia.png")

plot(x, y, main="Sueldo ~ Años de Experiencia ",sub="Sueldo vs Años de Experiencia", xlab="Años de experiencia", ylab="Sueldo",
     xlim=c(xmin,xmax),ylim=c(ymin,ymax))
abline(reg_lin)
dev.off()  # para cerrar el dispositivo de grafico elegido
# de esa forma se creara el archivo de la linea 26


plot(reg_lin)

# Test de normalidad de Kolmogorov-Smirnov
# x= reg_line$residuals , es un vector numerico con los datos a los cuales vamos aplicar el test
# en nuestro caso los residuos
# distrib= pnorm , indica la distribucion de referencia que se usara en el contraste, en nuestro caso es la 
# distribucion normal
ks.test(reg_lin$residuals , "pnorm")

# Test de Durbin Watson

library(lmtest)
dwtest(y~x)

#### Pag 46 ####
#Datos por pares (x,y)
x <- c(1,1,2,3,4)
y <- c(2,3,5,5,9)

# el simbolo indica ~ que se estudia una relacio entre
# la variable dependiente a la izq y las ind a la derecha
# la regresion  se estudia por medio de los modelos lineales
# mod es el nombre del modelo a estudiar

mod <- lm(y~x)
# liste los coef de regresion
mod$coefficients
summary(mod)

#### Pag 48 ####

x <- c(1,2,3,4,5,6,7)
y <- c(2,5,8,9,8,5,2)
modeloLineal <- lm(y~x)
summary(modeloLineal)

#### Pag 49 ####

x <- c(1,2,3,4,5,6,7)
y <- c(2,5,8,9,8,5,2)
modeloparabolico <- lm(y~poly(x,2,raw=TRUE))
summary(modeloparabolico)

# Pag 51 A

x <- c(1,1,2,3,4)
y <- c(2,3,5,5,9)
mod <- lm(y~x)

# listamos coef de regresion
mod$coefficients
summary(mod)

# Coeficientes de correlacion
cor(x,y)

# decida la Ho: no hay correlacion lineal
# contra la alterna: hay correlacion lineal
# causada por un efecto sistematico
cor.test(x,y)
# haga el dibujo de dispersion
png(filename = "ejemplo51A.png")
plot(x,y)
# añadimos la linea de regresion de minimos cuadrados
abline(mod)
dev.off()


# Pag 51 B

x <- c(1,1,2,3,4)
y <- c(2,3,5,5,9)

mod <- lm(y~x)
summary(mod)
# listamos coef de regresion
mod$coefficients
summary(mod)

# Coef de Correlacion
cor(x,y)
# decida la Ho: no hay correlacion lineal
# contra la alterna: hay correlacion lineal
# causada por un efecto sistematico
cor.test(x,y)

png(filename = "ejemplo51B.png")
# dibujo de dispersion de (x,y)
plot(x,y)
abline(mod)
dev.off()

# Pag 54 A

x <- c(1,2,3,4,5,6,7)
y <- c(2,5,8,9,8,5,2)

mod <- lm(y~x)
mod$coefficients
summary(mod)
cor(x,y)
cor.test(x,y)
plot(x,y)
abline(mod)

# Pag 61

n1 <- c(8,2,4,3,7,5,7)
n2 <- c(2,4,6,7,7,5,3)
r <- c(4,5,3,6,4,3,5)

mod <- lm(r ~ n1+n2)
mod$coefficients
summary(mod)
