
library(ggplot2)

set.seed(29)


#Ej 1 a
ej1a <- runif(1000,0,1)
hist(ej1a)

#Ej 1 b
ej1b <- c(1:1000)
var1b<-c(1:1000)
for (i in 1:1000) {
  x1 <- runif(1,0,1)
  x2 <- runif(1,0,1)
  xprom <- (x1 + x2) / 2
  var1b[i]<- ((x1-xprom)**2 + (x2-xprom)**2)/2
  ej1b[i] <- xprom
}

hist(ej1b) #no se si esta bien mostrar asi, pero como tengo q replicar 1000 y mostrar todo

#Ej 1 c
ej1c <- c(1:1000)
var1c<-c(1:1000)
for (i in 1:1000) {
  muestra <- runif(5,0,1)
  xprom <- sum(muestra) * 1/5
  var1c[i] <- (muestra-xprom)**2/5
  ej1c[i] <- xprom
}

hist(ej1c)
#Ej 1 d

ej1d <- c(1:1000)
var1d<- c(1:1000)
for (i in 1:1000) {
  muestra <- runif(30,0,1)
  xprom <- sum(muestra) * 1/30
  var1d[i] <- (muestra-xprom)**2/5
  ej1d[i] <- xprom
}
hist(ej1d) 

#Ej 1 e

ej1e <- c(1:1000)
var1e<- c(1:1000)
for (i in 1:1000) {
  muestra <- runif(500,0,1)
  xprom <- sum(muestra) * 1/500
  var1e[i] <- (muestra-xprom)**2/500
  ej1e[i] <- xprom
}
hist(ej1e)


#Ej 1 f
ej1f <- c(1:1000)
var1f<- c(1:1000)
for (i in 1:1000) {
  muestra <- runif(1200,0,1)
  xprom <- sum(muestra) * 1/1200
  var1f[i] <- (muestra-xprom)**2/500
  ej1f[i] <- xprom
}
todo <- data.frame( ej1=ej1a,ej2=ej1b,ej3=ej1c,ej4=ej1d,ej5=ej1e,ej6=ej1f)
todo
boxplot(todo,data=todo)


meanej1 <- mean(ej1a)
meanej2 <- mean(ej1b)
meanej3 <- mean(ej1c)
meanej4 <- mean(ej1d)
meanej5 <- mean(ej1e)
meanej6 <- mean(ej1f)

mvar1 <- var(ej1a)
mvar2 <- mean(var1b)
mvar3 <- mean(var1c)
mvar4 <- mean(var1d)
mvar5 <- mean(var1e)
mvar6 <- mean(var1f)

allVar <- data.frame(
  numberOfVariables=c(1000,2,5,30,500,1200),
  var=c(mvar1,mvar2,mvar3,mvar4,mvar5,mvar6),
  mean= c(meanej1,meanej2,meanej3,meanej4,meanej5,meanej6)
)

qqplot(x=allVar$numberOfVariables,y=allVar$var)
hist(allVar$var)

qqplot(allVar$numberOfVariables,allVar$mean)

#Ej 1 g

tcl <- function(xn,mu,v,n){
  return (xn-mu)/sqrt(v/n)
}
#preguntar q hacer en el A 
# var unifrome (b-a)**@/12
tcl1 <- tcl(meanej1,1/2,1/12,1000)
tcl2 <- tcl(meanej2,1/2,1/12,2)
tcl3 <-tcl(meanej3,1/2,1/12,5)
tcl4 <-tcl(meanej4,1/2,1/12,30)
tcl5 <-tcl(meanej5,1/2,1/12,500)
tcl6 <-tcl(meanej6,1/2,1/12,1200)

hist(c(tcl1,tcl2,tcl3,tcl4,tcl5,tcl6))
todoIncisoG <- data.frame( ej1=tcl1,ej2=tcl2,ej3=tcl3,ej4=tcl4,ej5=tcl5,ej6=tcl6)

boxplot(todoIncisoG,data = todoIncisoG)

################################################
##### ARRANCA TODO CON CAUCHY
################################################

#Cauchy<- function(x){
#  return(1/pi(1+x**2))
#}
#Ej 2 A
ej2a<- rcauchy(1000)
hist(ej2a)

#Ej 2 B
ej2b <- c(1:1000)
var2b<-c(1:1000)
for (i in 1:1000) {
  x1 <- rcauchy(1,0,1)
  x2 <- rcauchy(1,0,1)
  xprom <- (x1 + x2) / 2
  var2b[i]<- ((x1-xprom)**2 + (x2-xprom)**2)/2
  ej2b[i] <- xprom
}
hist(ej2b)

#Ej 2 C
ej2c <- c(1:1000)
var2c<-c(1:1000)
for (i in 1:1000) {
  muestra <- rcauchy(5,0,1)
  
  xprom <- sum(muestra) * 1/5
  var2c[i] <- (muestra-xprom)**2/5
  ej2c[i] <- xprom
}

hist(ej2c) #no se si esta bien mostrar asi, pero como tengo q replicar 1000 y mostrar todo

#Ej 2 D
ej2d <- c(1:1000)
var2d<- c(1:1000)
for (i in 1:1000) {
  muestra <- rcauchy(30,0,1)
  xprom <- sum(muestra) * 1/30
  var2d[i] <- (muestra-xprom)**2/5
  ej2d[i] <- xprom
}
hist(ej2d) 

#Ej 2 E
ej2e <- c(1:1000)
var2e<- c(1:1000)
for (i in 1:1000) {
  muestra <- rcauchy(500,0,1)
  xprom <- sum(muestra) * 1/500
  var2e[i] <- (muestra-xprom)**2/500
  ej2e[i] <- xprom
}
hist(ej2e)

#Ej 2 F
ej2f <- c(1:1000)
var2f<- c(1:1000)
for (i in 1:1000) {
  muestra <- rcauchy(1200,0,1)
  xprom <- sum(muestra) * 1/1200
  var2f[i] <- (muestra-xprom)**2/500
  ej2f[i] <- xprom
}
todo <- data.frame( ej1=ej2a,ej2=ej2b,ej2c=ej2d,ej4=ej2e,ej5=ej2f,ej6=ej6)
todo
boxplot(todo,data=todo)

#Ej 2 Gg
mvar1 <- var(ej1)
mvar2 <- mean(var2)
mvar3 <- mean(var3)
mvar4 <- mean(var4)
mvar5 <- mean(var5)
mvar6 <- mean(var6)

allVar <- data.frame(
  numberOfVariables=c(1000,2,5,30,500,1200),
  var=c(mvar1,mvar2,mvar3,mvar4,mvar5,mvar6),
  mean= c(meanej1,meanej2,meanej3,meanej4,meanej5,meanej6)
)

qqplot(x=allVar$numberOfVariables,y=allVar$var)
hist(allVar$var)

qqplot(allVar$numberOfVariables,allVar$mean)

#Ej 2 g
tcl <- function(xn,mu,v,n){
  return (xn-mu)/sqrt(v/n)
}
#preguntar q hacer en el A 
# var unifrome (b-a)**@/12
tcl1 <- tcl(meanej1,1/2,1/12,1000)
tcl2 <- tcl(meanej2,1/2,1/12,2)
tcl3 <-tcl(meanej3,1/2,1/12,5)
tcl4 <-tcl(meanej4,1/2,1/12,30)
tcl5 <-tcl(meanej5,1/2,1/12,500)
tcl6 <-tcl(meanej6,1/2,1/12,1200)

hist(c(tcl1,tcl2,tcl3,tcl4,tcl5,tcl6))
boxplot(x=allVar$numberOfVariables,data=c(tcl1,tcl2,tcl3,tcl4,tcl5,tcl6))


