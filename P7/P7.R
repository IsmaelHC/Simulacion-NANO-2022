library(reshape2) # recuerda instalar paquetes antes de intentar su uso
library(lattice) # lo mismo aplica con este paquete
library(rasterVis)
library(latticeExtra)
library(sp)
 g<- function(x, y) {
return(sin(sqrt(x^2  + y^2)))   
}
low <- 9.05
high <- 12.05
step <- 0.10
delta <- step
replicas <- 5
curr=numeric(replicas)
curr_x=numeric(replicas)
curr_y=numeric(replicas)
best_y=numeric(replicas)
best_x=numeric(replicas)
bests=numeric(replicas)
x <- seq(low, high, step)
y <-  x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
d <- melt(z)
names(d) <- c("x", "y", "z")
replica=5
time=c(500)
for(i in 1:replicas){
 curr_x[i] <- sample(seq(low, high, step),1)
 curr_y[i]<- sample(seq(low, high, step),1)
 curr[i]<-z[as.character(curr_y[i]),as.character(curr_x[i])]
 best_y[i]<-curr_y[i]
 best_x[i]=curr_x[i]
 bests[i]=curr[i]
}
best<-curr[1]
best_iny=as.character(curr_y[1])
best_inx = as.character(curr_x[1])
for (t in 1:time){
 for(i in 1:replica){
    min_fila<-max(curr_y[i]- delta, low)
    max_fila <-min(curr_y[i]+ delta,high)
    min_col<-max(curr_x[i] - delta, low)
    max_col <-min(curr_x[i] + delta,high)
     filas<-(seq(min_fila,max_fila,delta))  
     columnas<-(seq(min_col,max_col,delta)) 
     #vecindad<-z[as.character(filas),as.character(columnas)]          
     curr_y[i]<- sample(filas,1)
     curr_x[i] <- sample(columnas,1)
     curr[i] <-z[format(round(curr_y[i], 2), nsmall = 2),format(round(curr_x[i], 2), nsmall = 2)]
          if (curr[i] > bests[i]) {
          best_y[i]<-curr_y[i]
          best_x[i]=curr_x[i]
          bests[i] <- curr[i]
          }
          if (curr[i] > best) {
          best_iny<- curr_x[i]
          best_inx <- curr_y[i]
          best <- curr[i]
          }
}
salida <- paste("p7_t", t, ".png", sep="")
myPoints <- SpatialPoints(matrix(c(curr_x,curr_y),replicas))
p2 <- spplot(myPoints, pch=20,cex = 2.5,col="chartreuse3")
myPoints2 <- SpatialPoints(matrix(c(best_x,best_y),replicas))
p3 <- spplot(myPoints2, pch=10,cex = 2.5,col="chartreuse3")
p1=levelplot(z ~ x * y, data = d,
panel = function(...){
           panel.levelplot(...)
         panel.abline(h = best_inx)
            panel.abline(v = best_iny)
        })
p=p1+p2+p3
png(salida, width=500, height=500)
print(p)
graphics.off()
print(bests)
}
