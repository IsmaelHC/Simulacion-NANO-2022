x=2
datos=c(1,2,3,4,5)
print(datos)
datos_2=datos*2
print('Datos *2')
print(datos_2)

e <- matrix(c(1, 2, 3, 4), nrow=2)
print(e)
F=e[1,1]
print(F)

sin_F=sin(F)
s <- seq(4, 100, 2)
print(s)

if (F == 1) { print("es uno") }

eucl <- function(x1, y1) { dx <- x1 +2; dy <- y1 +x1; return(sqrt(dx^2 + dy^2)) }
eucl(1,2)

datos <- read.csv("P0.csv", header=TRUE, sep=" ", stringsAsFactors=FALSE)
print(datos)

x=1:20
y=sin(x)
plot(x,y,type="l")