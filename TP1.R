
library(ggplot2)


#Ej 1 a

muestra <- runif(1000,0,1)
hist(muestra)

#Ej 1 b


muestraTotal <- c(1:1000)
for (i in 1:1000) {
  x1 <- runif(1,0,1)
  x2 <- runif(1,0,1)
  xprom <- (x1 + x2) / 2
  muestraTotal[i] <- xprom
}

hist(muestraTotal) #no se si esta bien mostrar asi, pero como tengo q replicar 1000 y mostrar todo

#Ej 1 c

muestraTotal <- c(1:1000)
for (i in 1:1000) {
  muestra <- runif(5,0,1)
  xprom <- sum(muestra) * 1/5
  muestraTotal[i] <- xprom
}

hist(muestraTotal) #no se si esta bien mostrar asi, pero como tengo q replicar 1000 y mostrar todo

#Ej 1 d

muestraTotal <- c(1:1000)
#xVal <- c(1:30)
#xSum <- 0
for (i in 1:1000) {
  #for (j in 1:30) {
  #  xVal[j] <- runif(1,0,1)
  #  xSum <- xSum + xVal[j]
  #}
  muestra <- runif(30,0,1)
  xprom <- muestra * 1/30
  muestraTotal[i] <- xprom
}
hist(muestraTotal) #no se si esta bien mostrar asi, pero como tengo q replicar 1000 y mostrar todo

#Ej 1 e

muestraTotal <- c(1:1000)
#xVal <- c(1:500)
#xSum <- 0
for (i in 1:1000) {
 # for (j in 1:500) {
  #  xVal[j] <- runif(1,0,1)
  #  xSum <- xSum + xVal[j]
  #}
  muestra <- runif(500,0,1)
  xprom <- muestra * 1/500
  muestraTotal[i] <- xprom
}
hist(muestraTotal) #no se si esta bien mostrar asi, pero como tengo q replicar 1000 y mostrar todo

