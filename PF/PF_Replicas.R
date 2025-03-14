rotate <- function(x) t(apply(x, 2, rev))
n=100
moleculas=10
catalizador=c(50,100)
a=numeric()
b=numeric()
sigma=numeric()
podcat=numeric()
prosoidad=0.1

for(tpor in 1:3){
  for(cat in catalizador){
   for (replicas in 1:5){
    k=porosidad*n*n 
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
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
 for(time in 1:tpor){
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
medioporoso=zona
zona[,1:5]=0
zona[1,1]=3
zona[,(n-9):n]=0
xm=rep(0, moleculas)
ym=rep(0, moleculas)
rm=rep(0, moleculas)
vm=rep(0, moleculas)
for(i in 1:moleculas){
 while (TRUE) { # hasta que hallamos una posicion vacia para el gas
        fila<- sample(1:n, 1)
        if (zona[fila, 1] ==0 ) {
            zona[fila, 1] = 2
            xm[i] <- 1
            ym[i] <- fila
            vm[i]=2
            rm[i]=0
            break
        }
    }
}


 for(tiempo in 1:250){
  for(i in 1:(tiempo*moleculas)){

     #POSIBLES X Y
   padonde=sample(1:2,1)
   if(padonde==1){  
      p1=zona[ym[i],min(n,xm[i]+1)]
      p2=zona[min(n,ym[i]+1),min(n,xm[i]+1)]
      p3=zona[max(1,ym[i]-1),min(n,xm[i]+1)]
      p4=zona[min(n,ym[i]+1),xm[i]]
      p5=zona[max(1,ym[i]-1),xm[i]]
      p6=zona[min(n,ym[i]+1),max(1,xm[i]-1)]
      p7=zona[max(1,ym[i]-1),max(1,xm[i]-1)]
      p8=zona[max(1,ym[i]-1),xm[i]]
     dy=c(ym[i], min(n,ym[i]+1),max(1,ym[i]-1),min(n,ym[i]+1),+
          max(1,ym[i]-1),min(n,ym[i]+1),max(1,ym[i]-1),max(1,ym[i]-1)) 
     dx=c(min(n,xm[i]+1),min(n,xm[i]+1),min(n,xm[i]+1),xm[i], xm[i],+
         max(1,xm[i]-1),max(1,xm[i]-1),xm[i])
     val=c(p1,p2,p3,p4,p5,p6,p7,p8)
        for(j in 1:8){   
          if(val[j]==0){ 
          zona[ym[i],xm[i]]=0
          xm[i]=dx[j]
          ym[i]=dy[j]
          zona[dy[j],dx[j]]=vm[i]
          break
        }}
     }   

     if(padonde==2){  
      p1=zona[ym[i],min(n,xm[i]+1)]
      p2=zona[max(1,ym[i]-1),min(n,xm[i]+1)]
      p3=zona[min(n,ym[i]+1),min(n,xm[i]+1)]
      p4=zona[max(1,ym[i]-1),xm[i]]
      p5=zona[min(n,ym[i]+1),xm[i]]
      p6=zona[max(1,ym[i]-1),max(1,xm[i]-1)]
      p7=zona[min(n,ym[i]+1),max(1,xm[i]-1)]
      p8=zona[max(1,ym[i]-1),xm[i]]
     dy=c(ym[i],max(1,ym[i]-1), min(n,ym[i]+1),max(1,ym[i]-1),+
          min(n,ym[i]+1),max(1,ym[i]-1),min(n,ym[i]+1),max(1,ym[i]-1)) 
     dx=c(min(n,xm[i]+1),min(n,xm[i]+1),min(n,xm[i]+1),xm[i], xm[i],+
         max(1,xm[i]-1),max(1,xm[i]-1),xm[i])
     val=c(p1,p2,p3,p4,p5,p6,p7,p8)
        for(j in 1:8){   
          if(val[j]==0){
          zona[ym[i],xm[i]]=0
          xm[i]=dx[j]
          ym[i]=dy[j]
          zona[dy[j],dx[j]]=vm[i]
          break
        }}
     
     }  


 vecindad=zona[max(1,ym[i]-1):min(n,ym[i]+1),max(1,xm[i]-1):min(n,xm[i]+1)]
    cata=length(which(vecindad==1))
    rm[i]<-rm[i]+cata
    if(rm[i]>cat){
    vm[i]=3
    }
  }

 for(j in ((tiempo*moleculas)+1):((tiempo+1)*moleculas)){
  while (TRUE) { # hasta que hallamos una posicion vacia para el gas
        fila<- sample(1:n, 1)
        if (zona[fila, 1] ==0 ) {
            zona[fila, 1] = 2
            xm[j] <- 1
            ym[j] <- fila
            vm[j]=2
            rm[j]=0
            break
        }
    }
 }

}
deposito=zona[,(n-9):n]
ai=length(which(deposito==2))
a=c(a,ai)
bi=length(which(deposito==3))
b=c(b,bi)
sigma=c(sigma,tpor)
podcat=c(podcat,cat)
print(c(ai,bi,tpor,cat))
}
} 
}
