library(testit)
mejor_1=numeric()
mejor_2=numeric()
mejor_3=numeric()
optimo_1=numeric()
optimo_2=numeric()
optimo_3=numeric()
knapsack <- function(cap, peso, valor) {
    n <- length(peso)
    pt <- sum(peso)
    assert(n == length(valor))
    vt <- sum(valor)
    if (pt < cap) {
        return(vt)
    } else {
        filas <- cap + 1
        cols <- n + 1
        tabla <- matrix(rep(-Inf, filas * cols),
                       nrow = filas, ncol = cols)
        for (fila in 1:filas) {
            tabla[fila, 1] <- 0
        }
        rownames(tabla) <- 0:cap
        colnames(tabla) <- c(0, valor)
        for (objeto in 1:n) {
            for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
                anterior <- acum - peso[objeto]
                tabla[acum, objeto + 1] <- tabla[acum, objeto]
                if (anterior > 0) { # si conocemos una combinacion con ese peso
                    tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
                }
            }
        }
        return(max(tabla))
    }
}

factible <- function(seleccion, pesos, capacidad) {
    return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
    return(sum(seleccion * valores))
}

normalizar <- function(data) {
    menor <- min(data)
    mayor <- max(data)
    rango <- mayor - menor
    data <- data - menor # > 0
    return(data / rango) # entre 0 y 1
}


poblacion.inicial <- function(n, tam) {
    pobl <- matrix(round(runif(tam * n)), nrow = tam, ncol = n)
    return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
    pos <- sample(1:n, 1)
    mut <- sol
    mut[pos] <- (!sol[pos]) * 1
    return(mut)
}

reproduccion <- function(x, y, n) {
    pos <- sample(2:(n-1), 1)
    xy <- c(x[1:pos], y[(pos+1):n])
    yx <- c(y[1:pos], x[(pos+1):n])
    return(c(xy, yx))
}
generador.pesos_1<- function(cuantos, min, max) {
    return(sort(round(normalizar(runif(cuantos)) * (max - min) + min)))
}

generador.valores_1 <- function(cuantos, min, max) {
    return(sort(round(normalizar(runif(cuantos)) * (max - min) + min)))
   
}

generador.valores_2 <- function(cuantos, min, max) {
    return(sort(round(normalizar(rexp(cuantos)) * (max - min) + min)))
}

generador.pesos_2 <- function(pesos, min, max) {
    n <- length(valores)
    pesos <- double()
    for (i in 1:n) {
        media <- valores[n-i+1]
        desv <- runif(1)
        pesos<- c(pesos, rnorm(1, media, desv))
    }
    pesos <- normalizar(pesos) * (max - min) + min
    return(pesos)
}

generador.pesos_3<- function(cuantos, min, max) {
    return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores_3<- function(pesos, min, max) {
    n <- length(pesos)
    valores <- double()
    for (i in 1:n) {
        media <- (pesos[i])^2
        desv <- runif(1)
        valores <- c(valores, rnorm(1, media, desv))
    }
    valores <- normalizar(valores) * (max - min) + min
    return(valores)
}
n <- 50
poblacion <- c(20,30)
rep <- 10
tmax <- 10
probmut=0.05
replicas=3
for(init in poblacion){ 
 for(j in 1:replicas){
 pm <- pmi
########## 1
 p <- poblacion.inicial(n, init)
 tam <- dim(p)[1]
 assert(tam == init)
 pesos <- generador.pesos_1(n, 15, 80)
 valores <- generador.valores_1(n, 10, 500)
 capacidad <- round(sum(pesos) * 0.65) # capacidad 65% del peso total
 optimo <- knapsack(capacidad, pesos, valores)
 mejores <- double()

 for (iter in 1:tmax) {
    p$obj <- NULL
    p$fact <- NULL
    for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
        if (runif(1) < pm) {
            p <- rbind(p, mutacion(p[i,], n))
        }
    }
    for (i in 1:rep) { # una cantidad fija de reproducciones
        padres <- sample(1:tam, 2, replace=FALSE)
        hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
        p <- rbind(p, hijos[1:n]) # primer hijo
        p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
    }
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
     for (i in 1:tam) {
        obj <- c(obj, objetivo(p[i,], valores))
        fact <- c(fact, factible(p[i,], pesos, capacidad))
    }
    p <- cbind(p, obj)
    p <- cbind(p, fact)
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
  }
 #png("p10_1.png", width=600, height=300)
 #plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
 #points(1:tmax, mejores, pch=15)
 #abline(h=optimo, col="green", lwd=3)
 #graphics.off()
 #print(paste(mejor, (optimo - mejor) / optimo))
 optimo_1=c(optimo_1,optimo)
 mejor_1=c(mejor_1,mejor)

 ##########2
 p <- poblacion.inicial(n, init)
 tam <- dim(p)[1]
 assert(tam == init)
 valores <- generador.valores_2(n, 10, 500)
 pesos <- generador.pesos_2(valores, 15, 80)
 capacidad <- round(sum(pesos) * 0.65)
 optimo <- knapsack(capacidad, pesos, valores)
 mejores <- double()
 for (iter in 1:tmax) {
    p$obj <- NULL
    p$fact <- NULL
    for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
        if (runif(1) < pm) {
            p <- rbind(p, mutacion(p[i,], n))
        }
    }
    for (i in 1:rep) { # una cantidad fija de reproducciones
        padres <- sample(1:tam, 2, replace=FALSE)
        hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
        p <- rbind(p, hijos[1:n]) # primer hijo
        p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
    }
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
     for (i in 1:tam) {
        obj <- c(obj, objetivo(p[i,], valores))
        fact <- c(fact, factible(p[i,], pesos, capacidad))
    }
    p <- cbind(p, obj)
    p <- cbind(p, fact)
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
   }
 #png("p10.png", width=600, height=300)
 #plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
 #points(1:tmax, mejores, pch=15)
 #abline(h=optimo, col="green", lwd=3)
 #graphics.off()
 #print(paste(mejor, (optimo - mejor) / optimo))
 optimo_2=c(optimo_2,optimo)
 mejor_2=c(mejor_2,mejor)

############3
 p <- poblacion.inicial(n, init)
 tam <- dim(p)[1]
 assert(tam == init)
 pesos <- generador.pesos_3(n, 15, 80)
 valores <- generador.valores_3(pesos, 10, 500)
 capacidad <- round(sum(pesos) * 0.65)
 optimo <- knapsack(capacidad, pesos, valores)
 mejores <- double()
 for (iter in 1:tmax) {
    p$obj <- NULL
    p$fact <- NULL
    for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
        if (runif(1) < pm) {
            p <- rbind(p, mutacion(p[i,], n))
        }
    }
    for (i in 1:rep) { # una cantidad fija de reproducciones
        padres <- sample(1:tam, 2, replace=FALSE)
        hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
        p <- rbind(p, hijos[1:n]) # primer hijo
        p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
    }
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
     for (i in 1:tam) {
        obj <- c(obj, objetivo(p[i,], valores))
        fact <- c(fact, factible(p[i,], pesos, capacidad))
    }
    p <- cbind(p, obj)
    p <- cbind(p, fact)
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
    }
 #png("p10.png", width=600, height=300)
 #plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
 #points(1:tmax, mejores, pch=15)
 #abline(h=optimo, col="green", lwd=3)
 #graphics.off()
 #print(paste(mejor, (optimo - mejor) / optimo))
 optimo_3=c(optimo_3,optimo)
 mejor_3=c(mejor_3,mejor)
 #print(paste(mejor_1, (optimo_1 - mejor_1) / optimo_1))
 #print(paste(mejor_2, (optimo_2 - mejor_2) / optimo_2))
 #print(paste(mejor_3, (optimo_3 - mejor_3) / optimo_3))
}}
efi_1_POB=(optimo_1 - mejor_1) / optimo_1
efi_2_POB=(optimo_2 - mejor_2) / optimo_2
efi_3_POB=(optimo_3 - mejor_3) / optimo_3
POB=as.character(poblacion)
treatment=rep(POB,each=replicas)
generacion=as.character(rep(1:3,each=replicas*length(POB)))
eficiencias=c(efi_1_POB,efi_2_POB,efi_3_POB)
data_POB=data.frame(eficiencias, treatment,generacion)
library(ggplot2)
ggplot(data_POB, aes(x=generacion, y=eficiencias, fill=treatment)) +
geom_boxplot() +ylim(0.01, 0.3)+
  xlab("M�todo de generaci�n") + ylab("Relaci�n mejor/�ptimo")+labs(fill = "Impacto del n�mero de poblaci�n")
