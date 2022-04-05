#k <- c(10000,50000,150000,75000,400000)
#n <- c(1000000,2000000,3000000,1000000,4000000)
k <- c(7500,10000,25000,50000,75000)
n <- c(1000000)
replicas=5
den <- as.data.frame(t(rbind(k,n)))
names(densidades) <- c("k", "n")
filtracion=numeric()
rotura <- function(x) {
    return (1 / (1 + exp((c - x) / d)))
}
union <- function(x) {
    return (exp(-x / c))
}
romperse <- function(tam, cuantos) {
    res <- integer()
    for (cumulo in 1:cuantos) {
        if (runif(1) < rotura(tam)) {
            primera <- sample(1:(tam-1), 1)
            segunda <- tam - primera
            res <- c(res, primera, segunda)
        } else {
            res <- c(res, tam)
        }
    }
    return(res)
}

for(jj in 1:length(k)){
for(j in 1:replicas){
originales <- rnorm(den$k[jj])
cumulos <- originales - min(originales) + 1
cumulos <- round(den$n[jj] * cumulos / sum(cumulos))
diferencia <- den$n[jj] - sum(cumulos)
if (diferencia > 0) {
    for (i in 1:diferencia) {
        p <- sample(1:den$k[jj], 1)
        cumulos[p] <- cumulos[p] + 1
    }
} else if (diferencia < 0) {
    for (i in 1:-diferencia) {
        p <- sample(1:den$k[jj], 1)
        if (cumulos[p] > 1) {
            cumulos[p] <- cumulos[p] - 1
        }
    }
}
c <- median(cumulos) # tamanio critico de cumulos
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva

freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 10
digitos <- floor(log(duracion, 10)) + 1
xmax <- NULL
for (paso in 1:duracion) {
    pedazos <- integer()
    for (i in 1:dim(freq)[1]) {
        urna <- freq[i,]
        pedazos <- c(pedazos, romperse(urna$tam, urna$num))
    }
      freq <- as.data.frame(table(pedazos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    filtrados=numeric()
    for(i in 1:length(freq$tam)){
    if(freq$tam[i]>c){
    filtrados=c(filtrados,(freq$tam[i]*freq$num[i]))
    }
    }
    filtracion=c(filtracion,(sum(filtrados)/den$n[jj]))
   }
}
}
densidad=as.character(k/n)
treatment=rep(densidad,each=duracion*replicas)
paso=as.factor(c(rep(1:duracion)))
data=data.frame((filtracion*100), treatment,paso)
library(ggplot2)
ggplot(data, aes(x=paso, y=filtracion*100, fill=treatment)) +
geom_boxplot()+ggtitle("Filtracion según el valor de k/n en cada paso de la simulacion") +
  xlab("Paso") + ylab("% de particulas filtradas")+labs(fill = "Tasa k/n"
