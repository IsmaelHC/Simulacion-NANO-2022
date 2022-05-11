pick.one <- function(x) {
    if (length(x) == 1) {
        return(x)
    } else {
        return(sample(x, 1))
    }
}
 
poli <- function(maxdeg, varcount, termcount) {
    f <- data.frame(variable=integer(), coef=integer(), degree=integer())
    for (t in 1:termcount) {
        var <- pick.one(1:varcount)
        deg <- pick.one(1:maxdeg)
        f <-  rbind(f, c(var, runif(1), deg))
    }
    names(f) <- c("variable", "coef", "degree")
    return(f)
}
 
eval <- function(pol, vars, terms) {
    value <- 0.0
    for (t in 1:terms) {
        term <- pol[t,]
        value <-  value + term$coef * vars[term$variable]^term$degree
    }
    return(value)
}
 
domin.by <- function(target, challenger) {
    if (sum(challenger < target) > 0) {
        return(FALSE) # hay empeora
    } #si no hay empeora, vemos si hay mejora
    return(sum(challenger > target) > 0)
}
 
vc <- 4
md <- 3
tc <- 5
mydata=data.frame(valor_de_k=integer(), solpar=integer())
replicas=15
kvec <- c(2,3,4,5)#cuantas funciones objetivo
obj <- list()
for(k in kvec){
for (rep in 1:replicas){
for (i in 1:k) {
    obj[[i]] <- poli(vc, md, tc)
}
minim <- (runif(k) < 0.5)
sign <- (1 + -2 * minim)
n <- 200 #cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
for (i in 1:n) { #evaluamos las soluciones
    for (j in 1:k) { #para todos los objetivos
        val[i, j] <- eval(obj[[j]], sol[i,], tc)
    }
}

for(i in 1:k){
assign(paste("mejor",k,sep=""), which.max(sign[k] * val[,k]))    
}
cual <- c("max", "min")
no.dom <- logical()
dominadores <- integer()
for (i in 1:n) {
    d <- logical()
    for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,]))
    }
    cuantos <- sum(d)
    dominadores <- c(dominadores, cuantos)
    no.dom <- c(no.dom, cuantos == 0) #nadie le domina
}
frente <- subset(val, no.dom) #solamente las no dominadas
solpar=length(frente)/k
mydata <-  rbind(mydata, c(k, solpar))
colnames(mydata)=c("K","SP")
}}

library(ggplot2) #recordar instalar si hace falta
data <- data.frame(pos=as.character(mydata$K), dom=((mydata$SP*100)/n))
setEPS()
postscript("p11_violin.eps")
gr <- ggplot(data, aes(x=pos,y=dom,fill=dom)) + geom_violin(fill="orange", color="red",width=1)
gg=gr + geom_boxplot(width=0.02, fill="blue", color="green", lwd=0.5) +
    xlab("K")+ 
    ylab("% de soluciones de Pareto ") +
    ggtitle("Soluciones de Pareto")
graphics.off
SP=data$dom
estat <-data.frame(data$dom[1:15],data$dom[16:30],data$dom[31:45],data$dom[46:60]) 
colnames(estat)=c("2","3","4","5")
stargazer(estat)
wilcox.test(data$dom[1:15],data$dom[16:30])
