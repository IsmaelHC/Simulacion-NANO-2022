num=c(1000,10000,100000)#1000000,10000000,15000000,100000000
replicas=20
correcto=3.14159265359
absolutos=numeric()
absolutos_cuad=numeric()
exact=numeric()

for (n in num){
for(r in 1:replicas){
d=numeric()
x=runif(n,-1,1)
y=runif(n,-1,1)
for (i in 1:n){
d=c(d,sqrt(x[i]**2 + y[i]**2))
}
dentro=d<=1
estimado=sum(dentro)*4/n
abso_cuad=(correcto-estimado)**2
absolutos_cuad=c(absolutos_cuad,abso_cuad)

abso=abs(correcto-estimado)
absolutos=c(absolutos,abso)

decimal=1
de=substring(as.character(estimado),1,decimal)
dc=substring(as.character(correcto),1,decimal)
while(de==dc){
de=substring(as.character(estimado),1,decimal)
dc=substring(as.character(correcto),1,decimal)
decimal=decimal+1
}
exact=c(exact,decimal)
}}
absolutos=matrix(absolutos,1,length(num)*replicas)
absolutos_cuad=matrix(absolutos_cuad,1,length(num)*replicas)
exactitud=matrix(exact,1,length(num)*replicas)
library(ggplot2)
cantidad=rep(c("1000","10000","100000"))
data_compar=data.frame(t(absolutos),cantidad)
ggplot(data_compar, aes(x=(cantidad), y=absolutos)) +
geom_boxplot(fill="slateblue", alpha=0.2) +
xlab("Número de elementos")+
ylab("Diferencia Absoluta")+
ggtitle("Precision")

data_compar=data.frame(t(absolutos_cuad),cantidad)
ggplot(data_compar, aes(x=(cantidad), y=absolutos_cuad)) +
geom_boxplot(fill="slateblue", alpha=0.2) +
xlab("Número de elementos")+
ylab("Diferencia Cuadrada")+
ggtitle("Precision")

data_compar=data.frame(t(exactitud),cantidad)
ggplot(data_compar, aes(x=(cantidad), y=exactitud)) +
geom_boxplot(fill="slateblue", alpha=0.2) +
xlab("Número de elementos")+
ylab("Exactitud")+
ggtitle("Precision")
