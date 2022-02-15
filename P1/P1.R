start_time <- Sys.time()
p1=c(0,0)
cor=30
dur <- c(100,1000,10000)
num_ejes<-5
eje<-0
#DISTANCIA=rep(0,cor*num_ejes*length(dur))
DISTANCIA=numeric()
mayor<-array(rep(0,cor*num_ejes*length(dur)),dim=c(cor,num_ejes,length(dur)))
pos<- array(rep(0, cor*num_ejes*num_ejes), dim=c(cor,num_ejes, num_ejes))
 
 for(p in dur){
  for (pasos in 1:p) {
  for (caminata in 1:cor){
  for (e in 1:num_ejes){
    eje<-round(runif(1,1,e))
     
      if (runif(1) < 0.5){
      pos[caminata,eje,e] <- pos[caminata,eje,e] + 1
      }
      else{
       pos[caminata,eje,e]<- pos[caminata,eje,e] - 1
      }
   euclideana = sqrt(sum((pos[caminata,,e]**2))) # 
   
   
 if (p==100){
  j=1
}
  if (p==1000){
  j=2
}
  if (p==10000){
  j=3
}
 mayor[caminata,e,j] = max(mayor[caminata,e,j],euclideana)
}} 
}
}
for (i in 1:num_ejes){
 for (j in 1:length(dur)){
  D=c(mayor[,i,j])
DISTANCIA=c(DISTANCIA,D)
}}
end_time <- Sys.time()
run_time=end_time - start_time
DIMENSIONES=as.factor(c(rep(1:num_ejes, each=cor*length(dur))))
treatment=rep(c("100","1000","10000"),each=cor)
data=data.frame(DIMENSIONES, treatment,DISTANCIA)
library(ggplot2)
#png("boxplot_brownian.png") # mandar la figura a un archivo
 # grouped boxplot
 ggplot(data, aes(x=DIMENSIONES, y=DISTANCIA, fill=treatment)) +
geom_boxplot()
print(run_time)

