rm(list = ls())
getwd()
setwd("E:/Escritorio/RRL/")
#### Ejemplo 1 RLSimple####

edad <- c(56,42,72,36,63,47,55,47,38,42)
presion <- c(148,126,159,118,149,130,151,142,114,141)
edad
presion
plot(presion,edad)

#Regresion lineal simple

reg_lin <- lm(edad ~ presion) #lm estimaciones para los parametros
reg_lin  #almacena la regresion

# Para obtener mas innformacion del modelo de regresion
summary(reg_lin)

# Para dibujar la recta de regresion lineal sobre el 
# diagrama de dispesion
plot(presion,edad)
abline(reg_lin)

xmin <- 0.9*min(presion)
xmax <- 1.1*max(presion)
ymin <- 0.9*min(edad)
ymax <- 1.1*max(edad)

png(filename = "Poblacion10mujeresa.png")

plot(presion, edad, main="Edad ~ Presion Sanguinea",sub="Poblacion: 10 mujeres", xlab="Presion", ylab="Edad",
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
dwtest(edad~presion)

#### Ejemplo 2 RLMultiple####

gastos <- c(1000,580,520,500,600,550,400)
ingresos <- c(50000,2500,2000,1900,3000,4000,2000)
tamaño <- c(7,4,3,3,6,5,2)
hijosU <- c(3,1,1,0,1,2,0)

# Agrupamos los datos en un df
datos2 <- data.frame(gastos,ingresos,tamaño,hijosU)
# Ajustamos el modelo a una RL
reg_lin_mul <- lm(gastos~ingresos + tamaño+hijosU)
summary(reg_lin_mul)

# vemos en Coefficients los parametros estimados para la RLM

#### RLM en el df Prestige ####

library(car)
head(Prestige,5)
newdata=Prestige[,c(1:2)]
summary(newdata)
modelo=lm(income~education, data=newdata)
plot(newdata$education,newdata$income, main="Educacion ~ Income")
abline(modelo)
summary(modelo)

# income=898.8(education)-2853.6

#### Regresion Logistica #### 
# https://www.kaggle.com/ludobenistant/hr-analytics
# el url falla pero se descargo el archivo csv por el foro
datos <- read.csv("HumanResourcesAnalytics.csv",T)
muestra <- dim(datos)[1]
datos <- datos[sample(muestra,100,replace=TRUE),]
class(datos)
str(datos)
head(datos)
#View datos
colnames(datos)=c("nivel_satisfaccion","ultima_evaluacion","numero_proyectos","promedio_horas_mensuales","antiguedad",
                 "accidente","abandona","promocionado","departamento","salario")
datos.modelo <- subset(datos,select=c(abandona,nivel_satisfaccion,ultima_evaluacion))
datos.modelo$abandona <- factor(datos.modelo$abandona)
head(datos.modelo)
plot(datos.modelo$nivel_satisfaccion,datos.modelo$abandona)

table(datos.modelo$abandona)
summary(datos.modelo$nivel_satisfaccion)
summary(datos.modelo$ultima_evaluacion)

modelo.logit <- glm(abandona ~ultima_evaluacion + nivel_satisfaccion, data=datos.modelo, family="binomial")
summary(modelo.logit)

exp(coefficients(modelo.logit))

log.odds <- predict(modelo.logit, data.frame(nivel_satisfaccion=0.6,
                                             ultima_evaluacion=0.75))
log.odds
exp(log.odds)/(1-exp(log.odds))

q <- seq(from=0, to=20,by=0.1)
y <- 500+0.4*(q-10)*3
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y+noise
plot(q,noisy.y, col='deepskyblue4',xlab='q' , main='Observeddata')
lines(q,y,col='firebrick1',lwd=3)
model <- lm(noisy.y ~ poly(q,3))

confint(model, level=0.95)

#### Regresion de Poisson ####
NumResueltos <- c(0,1,2,1,5,3,2,5,7,8,12,13,12,11,10,12,10,15)
HorasClase <- c(1,3,4,1,3,5,1,3,5,2,3,5,0,3,5,4,3,5)

Nota <- c(0,2,3,0,4,3,1,3,4,3,4,5,4,5,4,5,5,5)
tabla <- data.frame(NumResueltos, HorasClase, Nota)

regPoisson <- glm(Nota~NumResueltos+HorasClase, data=tabla,family=poisson())
summary(regPoisson)
predict(regPoisson, type="response")








