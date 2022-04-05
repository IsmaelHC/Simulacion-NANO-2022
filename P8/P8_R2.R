k <- 10000
n <- 1000000
d_value=c(1,2,3,4)
replicas=5
filtracion=numeric()
duracion <- 10
digitos <- floor(log(duracion, 10)) + 1
xmax <- NULL
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
for(jj in d_value){
for(j in 1:replicas){
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
diferencia <- n - sum(cumulos)
if (diferencia > 0) {
    for (i in 1:diferencia) {
        p <- sample(1:k, 1)
        cumulos[p] <- cumulos[p] + 1
    }
} else if (diferencia < 0) {
    for (i in 1:-diferencia) {
        p <- sample(1:k, 1)
        if (cumulos[p] > 1) {
            cumulos[p] <- cumulos[p] - 1
        }
    }
}
cmediana <- median(cumulos) # tamanio critico de cumulos
c=cmediana*.95
d <- sd(cumulos) / jj # factor arbitrario para suavizar la curva

freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
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
    filtracion=c(filtracion,(sum(filtrados)/n))
   }
}}

suavizante=as.character(d_value)
treatment=rep(suavizante,each=duracion*replicas)
paso=as.factor(c(rep(1:duracion)))
data=data.frame((filtracion*100), treatment,paso)
library(ggplot2)
ggplot(data, aes(x=paso, y=filtracion*100, fill=treatment)) +
geom_boxplot()+ggtitle("Filtración según el factor suavizante") +
  xlab("Paso") + ylab("% de particulas filtradas")+labs(fill = "Factor suavizante")

