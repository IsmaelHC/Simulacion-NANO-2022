matriz <- c(20)
rep=10
cruzamientos=numeric()
resultados=NULL
sumatoria=numeric()
for(n in matriz){
#n=20
#k=12
porcentaje <- c(0.1,0.2)
for (por in porcentaje){
zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
k=n*por
x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
y <- rep(0, k) # igual como las coordenadas y de las semillas

for (semilla in 1:k) {
    while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:n, 1)
        columna <- sample(1:n, 1)
        if (zona[fila, columna] == 0) {
            zona[fila, columna] = semilla
            x[semilla] <- columna
            y[semilla] <- fila
            break
        }
    }
}

for (t in 1:100){
     ft=numeric()
      ct=numeric()
            for(pos in 1:n^2){
                f <- floor((pos - 1) / n) + 1
                c <- ((pos - 1) %% n) + 1 
                if(zona[f,c]>0){
                   if(zona[f,c]<=t){
                      #print(paste(t,semilla,pos,f,c))
                      ft=c(ft,f)
                      ct=c(ct,c)
                       #print(ft)
             }}}
        for(i in 1:length(ft)){
     
          zona[max(ft[i] - 1,1) : min(ft[i] + 1, n),
                         max(ct[i] - 1, 1): min(ct[i] + 1, n)][zona[max(ft[i] - 1, 1) : min(ft[i] + 1, n),
                         max(ct[i] - 1, 1): min(ct[i] + 1, n)]==0]<-zona[ft[i],ct[i]]
         } 
    #png(paste("p4s",t,".png",sep=""))
    #par(mar = c(0,0,0,0))
    #image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
    #graphics.off()
    #if (all(zona > 0)) break
   }





voronoi <- zona
rotate <- function(x) t(apply(x, 2, rev))
png(paste("p4s.png"))
par(mar = c(0,0,0,0))
image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
graphics.off()

limite <- n # grietas de que largo minimo queremos graficar

inicio <- function() {
    direccion <- sample(1:4, 1)
    xg <- NULL
    yg <- NULL
    if (direccion == 1) { # horiz izq -> der
        xg <- 1
        yg <- sample(1:n, 1)
    } else if (direccion == 2) { # vertical abajo -> arriba
        xg <- sample(1:n, 1)
        yg <- 1
    } else if (direccion == 3) { # horiz der -> izq
        xg <- n
        yg <- sample(1:n, 1)
    } else { # vertical arriba -> abajo
        xg <- sample(1:n, 1)
        yg <- n
    }
    return(c(xg, yg))
}

vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
for (dx in -1:1) {
    for (dy in -1:1) {
        if (dx != 0 | dy != 0) { # descartar la posicion misma
            vp <- rbind(vp, c(dx, dy))
        }
    }
}
names(vp) <- c("dx", "dy")
vc <- dim(vp)[1]

propaga <- function(replica) {
    prob <- 1 # interno a la celda (inicial)
    dificil <- 0.99 # bajada al interior
    grieta <- voronoi # marcamos la grieta en una copia
    i <- inicio() # posicion inicial al azar
    xg <- i[1]
    yg <- i[2]
    largo <- 0
    while (TRUE) { # hasta que la propagacion termine
        grieta[yg, xg] <- 13 # usamos el cero para marcar la grieta
        largo <-  largo + 1
        frontera <- numeric()
        interior <- numeric()
        for (v in 1:vc) {
            vecino <- vp[v,]
            xs <- xg + vecino$dx # columna del vecino potencial
            ys <- yg + vecino$dy # fila del vecino potencial
            if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
                if (grieta[ys, xs] < 13) { # aun no hay grieta ahi
                    if (voronoi[yg, xg] == voronoi[ys, xs]) {
                        interior <- c(interior, v)
                    } else { # frontera
                        frontera <- c(frontera, v)
                    }
                }
            }
        }
        elegido <- 0
        if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
            if (length(frontera) > 1) {
                elegido <- sample(frontera, 1)
            } else {
                elegido <- frontera # sample sirve con un solo elemento
            }
            prob <- 1 # estamos nuevamente en la frontera
        } else if (length(interior) > 0) { # no hubo frontera para propagar
            if (runif(1) < prob) { # intentamos en el interior
                if (length(interior) > 1) {
                    elegido <- sample(interior, 1)
                } else {
                    elegido <- interior
                }
                prob <- dificil * prob # mas dificil a la siguiente
            }
        }
        if (elegido > 0) { # si se va a propagar
            vecino <- vp[elegido,]
            xg <- xg + vecino$dx
            yg <- yg + vecino$dy
        } else {
            break # ya no se propaga
        }
    }
    #if (largo >= limite) {
        #png(paste("p4g_", replica, ".png", sep=""))
        #par(mar = c(0,0,0,0))
        #image(rotate(grieta), col=rainbow(k+2), xaxt='n', yaxt='n')
        #graphics.off()
    #}
#grieta 2
cruza=numeric()
cruzan=numeric()
prob <- 1 # interno a la celda (inicial)
dificil <- 0.99 # bajada al interior
 i <- inicio() # posicion inicial al azar
    xg <- i[1]
    yg <- i[2]
    largo <- 0
    while (TRUE) { # hasta que la propagacion termine
        grieta[yg, xg] <- 14 # usamos el cero para marcar la grieta
        largo <-  largo + 1
        frontera <- numeric()
        interior <- numeric()
        for (v in 1:vc) {
            vecino <- vp[v,]
            xs <- xg + vecino$dx # columna del vecino potencial
            ys <- yg + vecino$dy # fila del vecino potencial
            if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
                if (grieta[ys, xs] < 14) { # aun no hay grieta ahi
                   cruza=c(cruza,(grieta[ys,xs]))
                    if (voronoi[yg, xg] == voronoi[ys, xs]) {
                        interior <- c(interior, v)
                    } else { # frontera
                        frontera <- c(frontera, v)
                    }
                }
            }
        }
         
        elegido <- 0
        if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
            if (length(frontera) > 1) {
                elegido <- sample(frontera, 1)
            } else {
                elegido <- frontera # sample sirve con un solo elemento
            }
            prob <- 1 # estamos nuevamente en la frontera
        } else if (length(interior) > 0) { # no hubo frontera para propagar
            if (runif(1) < prob) { # intentamos en el interior
                if (length(interior) > 1) {
                    elegido <- sample(interior, 1)
                } else {
                    elegido <- interior
                }
                prob <- dificil * prob # mas dificil a la siguiente
            }
        }
        if (elegido > 0) { # si se va a propagar
            vecino <- vp[elegido,]
            xg <- xg + vecino$dx
            yg <- yg + vecino$dy
        } else {
            break # ya no se propaga
        }
    }
    
        
   if (13%in%cruza) { 
 png(paste("p4g_cruza_", replica,"_",n,"_",k,".png", sep=""))
        par(mar = c(0,0,0,0))
        image(rotate(grieta), col=rainbow(k+3), xaxt='n', yaxt='n')
        graphics.off()
   cruzan=c(cruzan,1)
}else{ 
png(paste("p4g_nocruza_", replica,"_",n,"_",k,".png", sep=""))
        par(mar = c(0,0,0,0))
        image(rotate(grieta), col=rainbow(k+3), xaxt='n', yaxt='n')
        graphics.off()
        cruzan=c(cruzan,0)
} 

    return(cruzan)
    }
cc=numeric()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
fracturamiento <- foreach(r = 1:rep, .combine=c) %dopar% propaga(r)
stopImplicitCluster()
print(fracturamiento)
cruzamientos=c(cruzamientos,fracturamiento)
resultados=c(resultados,paste(n,"*",por))
sumatoria=c(sumatoria,sum(fracturamiento))
}}
cruzamientos=data.frame(matrix(cruzamientos,rep))
colnames(cruzamientos)=resultados
sumatoria=data.frame(matrix(sumatoria,1))
