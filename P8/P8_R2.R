library(testit) # para pruebas, recuerda instalar antes de usar
k_i <-5000 
n_i <- 1000000
d_value=c(1,2,3,4)
replicas=10
den <- as.data.frame(t(rbind(k_i,n_i)))
names(den) <- c("k_i", "n_i")
filtracion=numeric()
for(jj in d_value){
for(j in 1:replicas){
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
assert(min(cumulos) > 0)
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
assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
assert(sum(cumulos) == n)
cmediana <- median(cumulos) # tamanio critico de cumulos
c=cmediana*1.05
d <- sd(cumulos) / jj # factor arbitrario para suavizar la curva
rotura <- function(x) {
    return (1 / (1 + exp((c - x) / d)))
}
union <- function(x) {
    return (exp(-x / c))
}
romperse <- function(tam, cuantos) {
    romper <- round(rotura(tam) * cuantos) # independientes
    resultado <- rep(tam, cuantos - romper) # los demas
    if (romper > 0) {
        for (cumulo in 1:romper) { # agregar las rotas
            t <- 1
            if (tam > 2) { # sample no jala con un solo valor
                t <- sample(1:(tam-1), 1)
            }
            resultado <- c(resultado, t, tam - t)
        }
    }
    assert(sum(resultado) == tam * cuantos) # no hubo perdidas
    return(resultado)
}
unirse <- function(tam, cuantos) {
    unir <- round(union(tam) * cuantos) # independientes
    if (unir > 0) {
        division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
        assert(sum(abs(division)) == tam * cuantos)
        return(division)
    } else {
        return(rep(tam, cuantos))
    }
}
freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 10
digitos <- floor(log(duracion, 10)) + 1
for (paso in 1:duracion) {
    assert(sum(cumulos) == n)
    cumulos <- integer()
    for (i in 1:dim(freq)[1]) { # fase de rotura
        urna <- freq[i,]
        if (urna$tam > 1) { # no tiene caso romper si no se puede
            cumulos <- c(cumulos, romperse(urna$tam, urna$num))
        } else {
            cumulos <- c(cumulos, rep(1, urna$num))
        }
    }
    assert(sum(cumulos) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    freq <- as.data.frame(table(cumulos)) # actualizar urnas
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)
    cumulos <- integer()
    for (i in 1:dim(freq)[1]) { # fase de union
        urna <- freq[i,]
        cumulos <- c(cumulos, unirse(urna$tam, urna$num))
    }
    assert(sum(abs(cumulos)) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    juntarse <- -cumulos[cumulos < 0]
    cumulos <- cumulos[cumulos > 0]
    assert(sum(cumulos) + sum(juntarse) == n)
    nt <- length(juntarse)
    if (nt > 0) {
        if (nt > 1) {
            juntarse <- sample(juntarse)
            for (i in 1:floor(nt / 2) ) {
                cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
            }
        }
        if (nt %% 2 == 1) {
            cumulos <- c(cumulos, juntarse[nt])
        }
    }
    assert(sum(cumulos) == n)
    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)
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
geom_boxplot()+ylim(45, 52)+
  xlab("Paso") + ylab("% de particulas filtradas")+labs(fill = "Factor suavizante")

