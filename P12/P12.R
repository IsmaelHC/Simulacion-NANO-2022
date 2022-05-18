binario <- function(d, l) {
    b <-  rep(FALSE, l)
    while (l > 0 | d > 0) {
        b[l] <- (d %% 2 == 1)
        l <- l - 1
        d <- bitwShiftR(d, 1)
    }
    return(b)
}
 
decimal <- function(bits, l) {
    valor <- 0
    for (pos in 1:l) {
        valor <- valor + 2^(l - pos) * bits[pos]
    }
    return(valor)
}
 

replicas=5 
r <- 5
c <- 3
dim <- r * c
 f1=numeric()
f2=numeric()
f3=numeric()
 fscoremean=numeric()
tasa <- 0.15
tranqui <- 0.99
 
tope <- 9
digitos <- 0:tope
k <- length(digitos)
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)
neg=c(0.990,0.999,0.980)
gris=c(0.920,0.930,0.910)
blan=c(0.002,0.001,0.003)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
for(ne in neg){
 for(gr in gris){
  for(bl in blan){
fscore=numeric()
modelos <- read.csv("digits.txt", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- ne
modelos[modelos=='g'] <- gr
modelos[modelos=='b'] <- bl
for(i in 1:replicas){
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
for (t in 1:5000) { # entrenamiento
    d <- sample(0:tope, 1)
    pixeles <- runif(dim) < modelos[d + 1,]
    correcto <- binario(d, n)
    for (i in 1:n) {
        w <- neuronas[i,]
        deseada <- correcto[i]
        resultado <- sum(w * pixeles) >= 0
        if (deseada != resultado) {
            ajuste <- tasa * (deseada - resultado)
            tasa <- tranqui * tasa
            neuronas[i,] <- w + ajuste * pixeles
        }
    }
}
 
for (t in 1:300) { # prueba
    d <- sample(0:tope, 1)
    pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
    correcto <- binario(d, n)
    salida <- rep(FALSE, n)
    for (i in 1:n) {
        w <- neuronas[i,]
        deseada <- correcto[i]
        resultado <- sum(w * pixeles) >= 0
        salida[i] <- resultado
    }
    r <- min(decimal(salida, n), k) # todos los no-existentes van al final
    contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
}
#print(contadores)
tp=numeric()
for (i in 1:10){
tp=c(contadores[i,i],tp)
}
tp=sum(tp)
fn=sum(contadores[,11])
tn=0
fp=abs(tp+fn-sum(contadores))
acuraccy=(tp+tn)/(tp+tn+fp+fn)
precision=tp/(tp+fp)
recall=tp/(tp+tn)
fscore=c(fscore,2*(precision*recall)/(precision+recall))
}
fscoremean=c(fscoremean,mean(fscore))
f1=c(f1,ne)
f2=c(f2,gr)
f3=c(f3,bl)
}}}
f1_1=fscoremean[1:9]
f1_2=fscoremean[10:18]
f1_3=fscoremean[19:27]
dim(f1_1)=c(3,3)
dim(f1_2)=c(3,3)
dim(f1_3)=c(3,3)
f1_1=data.frame(f1_1)
f1_2=data.frame(f1_2)
f1_3=data.frame(f1_3)
colnames(f1_1) <- c('f2_1','f2_2','f2_3')
rownames(f1_1) <- c('f3_1','f3_2','f3_3')
colnames(f1_2) <- c('f2_1','f2_2','f2_3')
rownames(f1_2) <- c('f3_1','f3_2','f3_3')
colnames(f1_3) <- c('f2_1','f2_2','f2_3')
rownames(f1_3) <- c('f3_1','f3_2','f3_3')
df=data.frame(fscoremean,f1,f2,f3)
model <- aov(fscoremean ~ f1 * f2 * f3, data=df)
