l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
dia=numeric()
rep=10
pico=numeric()
aislamiento=numeric()
movilidad=c(1,2,3,4)
for( mov in movilidad){
for(repeticion in 1:rep){
agentes <- data.frame(x = double(), y = double(),
                      dx = double(), dy = double(),
                      estado  = character())
for (i in 1:n) {
    e <- "S"
    if (runif(1) < pi) {
        e <- "I"
    }
    agentes <- rbind(agentes, data.frame(x = runif(1, 0, l),
                                         y = runif(1, 0, l),
                                         dx = runif(1, -v, v),
                                         dy = runif(1, -v, v),
                                         estado = e))
 
}
levels(agentes$estado) <- c("S", "I", "R")
epidemia <- integer()
r <- 0.1
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
for(i in 1:n){
        if(agentes$estado[i] == "I"){
         agentes$dx[i]=agentes$dx[i]/mov
         agentes$dy[i]=agentes$dy[i]/mov 
 }}
for (tiempo in 1:tmax) {
    infectados <- dim(agentes[agentes$estado == "I",])[1]
    epidemia <- c(epidemia, infectados)
    if (infectados == 0) {
        break
    }
      
    contagios <- rep(FALSE, n)
    for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
            for (j in 1:n) {
                if (!contagios[j]) { # aun sin contagio
                    a2 <- agentes[j, ]
                    if (a2$estado == "S") { # hacia los susceptibles
                        dx <- a1$x - a2$x
                        dy <- a1$y - a2$y
                        d <- sqrt(dx^2 + dy^2)
                        if (d < r) { # umbral
                            p <- (r - d) / r
                            if (runif(1) < p) {
                                contagios[j] <- TRUE
                            }
                        }
                    }
                }
            }
        }
    }
    for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
            a$estado <- "I"
            a$dx=a$dx/mov 
            a$dy=a$dy/mov
        } else if (a$estado == "I") { # ya estaba infectado
            if (runif(1) < pr) {
                a$estado <- "R" # recupera
            }
        }
        a$x <- a$x + a$dx
        a$y <- a$y + a$dy
        if (a$x > l) {
            a$x <- a$x - l
        }
        if (a$y > l) {
            a$y <- a$y - l
        }
        if (a$x < 0) {
            a$x <- a$x + l
        }
        if (a$y < 0) {
            a$y <- a$y + l
        }
        agentes[i, ] <- a
    }
    
}
png("p6e.png", width=600, height=300)
plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo",ylim=c(10,70), ylab="Porcentaje de infectados")
graphics.off()

pico=c(pico,max(epidemia))
dia=c(dia,which.max(epidemia))
aislamiento=c(aislamiento,mov)
}}
data_compar=data.frame(pico,aislamiento)
ggplot(data_compar, aes(x=as.factor(aislamiento), y=pico)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
xlab("Nivel de aislamiento de infectados")+
ylab("Máximo de contagios")+
ggtitle("Aislamiento")

data_compar=data.frame(dia,aislamiento)
ggplot(data_compar, aes(x=as.factor(aislamiento), y=dia)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
xlab("Nivel de aislamiento de infectados")+
ylab("Día del pico de la pandemia")+
