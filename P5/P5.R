library('distr')
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
entre=function(n){gen=generador(n);return((pi/2)*sum(gen>=3&gen<=7)/n)}
correcto=0.04883411112604931084064237
num=c(1000,10000,100000,1000000,10000000,15000000,100000000) #cantidad de elementos
replicas=20
absolutos=numeric()
absolutos_cuad=numeric()
exact=numeric()
for (n in num){
for(r in 1:replicas){
estimado=entre(n)
abso=abs(correcto-estimado)
absolutos=c(absolutos,abso)

abso_cuad=(correcto-estimado)**2
absolutos_cuad=c(absolutos_cuad,abso_cuad)

decimal=3
de=substring(as.character(estimado),1,decimal)
dc=substring(as.character(correcto),1,decimal)
while(de==dc){
de=substring(as.character(estimado),1,decimal)
dc=substring(as.character(correcto),1,decimal)
decimal=decimal+1
}
exact=c(exact,decimal)
}}

cantidad=rep(c("1000","10000","100000","1000000","10000000","15000000","100000000"),each=replicas)
absolutos=matrix(absolutos,replicas,length(num))
absolutos_cuad=matrix(absolutos_cuad,replicas,length(num))
exactitud=matrix(exact,replicas,length(num))

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
