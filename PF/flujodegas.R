rotate <- function(x) t(apply(x, 2, rev))
n=100
zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
porosidad=.01
k=porosidad*n*n
x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
y <- rep(0, k) # igual como las coordenadas y de las semillas
for (grano in 1:k) {
    while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:n, 1)
        columna <- sample(1:n, 1)
        if (zona[fila, columna] == 0) {
            zona[fila, columna] = 1
            x[grano] <- columna
            y[grano] <- fila
            break
        }
    }
}
zona1=zona
#GENERACION MEDIO POROSO
for(time in 1:2){
for(pos in 1:k){
   j=x[pos]
   i=y[pos]
      zona[max(i- 1,1) : min(i+ 1, n),
                         max(j-1, 1): min(j+ 1, n)]=1
   x=c(x,max(j- 1, 1): min(j+ 1, n))
   y=c(y,max(i- 1,1) : min(i+ 1, n))
    k=length(x)
}
}
image(rotate(zona), xaxt='n', yaxt='n',axes=T)
medioporoso=zona
zona[,1:5]=0

#INYECCION DE GAS
moleculas=5
xm=rep(0, moleculas)
ym=rep(0, moleculas)
for(i in 1:moleculas){
 while (TRUE) { # hasta que hallamos una posicion vacia para el gas
        fila<- sample(1:n, 1)
        if (zona[fila, 1] ==0 ) {
            zona[fila, 1] = 2
            xm[i] <- 1
            ym[i] <- fila
            break
        }
    }
}
#FLUJO DE GAS
for(tiempo in 1:100){
 for(i in 1:(tiempo*moleculas)){
   p1=zona[ym[i], min(n,xm[i]+1)]            #POSIBLES ESPACIO PARA EL GAS
   p2=zona[max(1,ym[i]-1), min(n,xm[i]+1)]
   p3=zona[ min(n,ym[i]+1),  min(n,xm[i]+1)]
   p4=zona[min(n,ym[i]+1), xm[i]]
   p5=zona[min(n,ym[i]-1), xm[i]]
   p6=zona[ min(n,ym[i]+1),  max(1,xm[i]-1)]
   p7=zona[ max(1,ym[i]-1),  max(1,xm[i]-1)]
   p8=zona[ym[i], max(1,xm[i]-1)] 
   dy=c(ym[i],max(1,ym[i]-1), min(n,ym[i]+1),min(n,ym[i]+1),min(n,ym[i]-1), +
           min(n,ym[i]+1),max(1,ym[i]-1),ym[i])                             #POSIBLES Y PARA E GAS
    dx=c(min(n,xm[i]+1),min(n,xm[i]+1),min(n,xm[i]+1), xm[i],xm[i], +
         max(1,xm[i]-1),max(1,xm[i]-1),max(1,xm[i]-1))                       #POSIBLES X PARA EL GASA

   zona[ym[i],xm[i]]=0
    j=1
     
      while (TRUE) {                   # hasta que hallamos una posicion vacia para el gas
       if (zona[dy[j],dx[j]] ==0){                  
           zona[dy[j],dx[j]]=2
          ym[i]=dy[j]
          xm[i]=dx[j]
           break
        }
       j=j+1
       }
   }
    png(paste("pf", tiempo, ".png", sep=""))
    image(rotate(zona), xaxt='n', yaxt='n',axes=T)
    graphics.off()
#INYECCION CONSTANTE DE GAS
    for(j in ((tiempo*moleculas)+1):((tiempo+1)*moleculas)){
        while (TRUE) { # hasta que hallamos una posicion vacia para el gas
          fila<- sample(1:n, 1)
          if (zona[fila, 1] ==0 ) {
            zona[fila, 1] = 2
            xm[j] <- 1
            ym[j] <- fila
            break
          }
       }
    }


}
