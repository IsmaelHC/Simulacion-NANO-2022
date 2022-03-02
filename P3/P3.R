library(ggplot2)
primo_1 <- function(n) {
    if (n < 3) {
        return(TRUE)
    }
    for (i in 2:(n-1)) {
        if (n %% i == 0) { # residuo es cero
            return(FALSE)
        }
    }
    return(TRUE)
}
primo_2 <- function(n) {
    if (n < 4) {
        return(TRUE)
    }
    if (n %% 2 == 0) { # par
        return(FALSE)
    }
    for (i in seq(3, max(3, n - 1), 2)) {
        if (n %% i == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
primo_3 <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
residuo <- function(n) {
    for (i in seq(1, max(1,n-1), 1)) {
        if (n %% i == 0) { # residuo es cero
            return(TRUE)
        }
    }
    return(FALSE)
}

potencia<- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        } else{
           return(pot=log(i,base=hasta))
           
       }
    }
  
}

pot=0
desde <- 1
hasta <- 4000
original <- desde:hasta
invertido <- hasta:desde
aleatorio <- sample(original) # fijo
replicas <- 15
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
ot_1 <-  numeric()
it_1<-  numeric()
at_1 <-  numeric()
ot_2 <-  numeric()
it_2<-  numeric()
at_2 <-  numeric()
ot_3 <-  numeric()
it_3<-  numeric()
at_3 <-  numeric()
matriz<-numeric()
for (r in 1:replicas) {
    #RUTINA 1 
    ot_1<- c(ot_1, system.time(foreach(n = original, .combine=c) %dopar% primo_1(n))[3]) # de menor a mayor
    it_1 <- c(it_1, system.time(foreach(n = invertido, .combine=c) %dopar% primo_1(n))[3]) # de mayor a menor
    at_1<- c(at_1, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo_1(n))[3]) # orden aleatorio
    #RUTINA 2
    ot_2<- c(ot_2, system.time(foreach(n = original, .combine=c) %dopar% primo_2(n))[3]) # de menor a mayor
    it_2 <- c(it_2, system.time(foreach(n = invertido, .combine=c) %dopar% primo_2(n))[3]) # de mayor a menor
    at_2<- c(at_2, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo_2(n))[3]) # orden aleatorio
    #RUTINA 3
    ot_3<- c(ot_3, system.time(foreach(n = original, .combine=c) %dopar% primo_3(n))[3]) # de menor a mayor
    it_3 <- c(it_3, system.time(foreach(n = invertido, .combine=c) %dopar% primo_3(n))[3]) # de mayor a menor
    at_3<- c(at_3, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo_3(n))[3]) # orden aleatorio
}
stopImplicitCluster()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 3))
ot_1_1 <-  numeric()
it_1_1<-  numeric()
at_1_1 <-  numeric()
ot_2_1 <-  numeric()
it_2_1<-  numeric()
at_2_1 <-  numeric()
ot_3_1 <-  numeric()
it_3_1<-  numeric()
at_3_1 <-  numeric()
for (r in 1:replicas) {
    #RUTINA 1 
    ot_1_1<- c(ot_1_1, system.time(foreach(n = original, .combine=c) %dopar% primo_1(n))[3]) # de menor a mayor
    it_1_1 <- c(it_1_1, system.time(foreach(n = invertido, .combine=c) %dopar% primo_1(n))[3]) # de mayor a menor
    at_1_1<- c(at_1_1, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo_1(n))[3]) # orden aleatorio
    #RUTINA 2
    ot_2_1<- c(ot_2_1, system.time(foreach(n = original, .combine=c) %dopar% primo_2(n))[3]) # de menor a mayor
    it_2_1 <- c(it_2_1, system.time(foreach(n = invertido, .combine=c) %dopar% primo_2(n))[3]) # de mayor a menor
    at_2_1<- c(at_2_1, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo_2(n))[3]) # orden aleatorio
    #RUTINA 3
    ot_3_1<- c(ot_3_1, system.time(foreach(n = original, .combine=c) %dopar% primo_3(n))[3]) # de menor a mayor
    it_3_1<- c(it_3_1, system.time(foreach(n = invertido, .combine=c) %dopar% primo_3(n))[3]) # de mayor a menor
    at_3_1<- c(at_3_1, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo_3(n))[3]) # orden aleatorio
}
stopImplicitCluster()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
ot_residuo <-  numeric()
it_residuo<-  numeric()
at_residuo <-  numeric()
for (r in 1:replicas) {
    #RUTINA 1 
    ot_residuo<- c(ot_residuo, system.time(foreach(n = original, .combine=c) %dopar% residuo(n))[3]) # de menor a mayor
    it_residuo<- c(it_residuo, system.time(foreach(n = invertido, .combine=c) %dopar% residuo(n))[3]) # de mayor a menor
    at_residuo<- c(at_residuo, system.time(foreach(n = aleatorio, .combine=c) %dopar% residuo(n))[3]) # orden aleatorio
 
}
stopImplicitCluster()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
ot_potencia <-  numeric()
it_potencia<-  numeric()
at_potencia<-  numeric()
for (r in 1:replicas) {
    #RUTINA 1 
    ot_potencia<- c(ot_potencia, system.time(foreach(n = original, .combine=c) %dopar% potencia(n))[3]) # de menor a mayor
    it_potencia<- c(it_potencia, system.time(foreach(n = invertido, .combine=c) %dopar% potencia(n))[3]) # de mayor a menor
    at_potencia<- c(at_potencia, system.time(foreach(n = aleatorio, .combine=c) %dopar% potencia(n))[3]) # orden aleatorio
 
}
stopImplicitCluster()






tiempos=c(ot_1,it_1,at_1,ot_2,it_2,at_2,ot_3,it_3,at_3,ot_1_1,it_1_1,at_1_1,ot_2_1,it_2_1,at_2_1,ot_3_1,it_3_1,at_3_1)
rutina=as.factor(c(rep(1:3, each=replicas*3)))
treatment=rep(c("ot","it","at"),each=replicas)
nucleos=rep(c("3","2"),each=replicas*3*3)
data=data.frame(nucleos,rutina,treatment,tiempos)

tiempos=c(ot_1,it_1,at_1,ot_2,it_2,at_2,ot_3,it_3,at_3)
rutina=as.factor(c(rep(1:3, each=replicas*3)))
treatment=rep(c("ot","it","at"),each=replicas)
data3=data.frame(rutina,treatment,tiempos)

tiempos=c(ot_1_1,it_1_1,at_1_1,ot_2_1,it_2_1,at_2_1,ot_3_1,it_3_1,at_3_1)
rutina=as.factor(c(rep(1:3, each=replicas*3)))
treatment=rep(c("ot","it","at"),each=replicas)
data2=data.frame(rutina,treatment,tiempos)

tiempos=c(ot_3,it_3,at_3,ot_3_1,it_3_1,at_3_1)
rutina=as.factor(c(rep("3","1"), each=replicas*3)))
treatment=rep(c("ot","it","at"),each=replicas)
data_compar=data.frame(rutina,treatment,tiempos)

summary(ot_1)
summary(it_1)
summary(at_1)
summary(ot_2)
summary(it_2)
summary(at_2)
summary(ot_3)
summary(it_3)
summary(at_3)
summary(ot_1_1)
summary(it_1_1)
summary(at_1_1)
summary(ot_2_1)
summary(it_2_1)
summary(at_2_1)
summary(ot_3_1)
summary(it_3_1)
summary(at_3_1)
summary(ot_residuo)
summary(it_residuo)
summary(at_residuo)
summary(ot_potencia)
summary(it_potencia)
summary(at_potencia)

tiempos=c(list(ot_1,it_1,at_1,ot_2,it_2,at_2,ot_3,it_3,at_3,ot_1_1,it_1_1,at_1_1,ot_2_1,it_2_1,at_2_1,ot_3_1,it_3_1,at_3_1,ot_residuo,it_residuo,at_residuo,ot_potencia,it_potencia,at_potencia))
experimento=c("ot_1","it_1","at_1","ot_2","it_2","at_2","ot_3","it_3","at_3","ot_1_1","it_1_1","at_1_1","ot_2_1","it_2_1","at_2_1","ot_3_1","it_3_1","at_3_1","ot_residuo","it_residuo","at_residuo","ot_potencia","it_potencia","at_potencia")
varianzas=numeric()
for (i in 1:length(tiempos)){
arr=array(unlist(tiempos[i]),dim=c(replicas,1))
varianzas=c(varianzas,var(arr))
}
tabla=data.frame(experimento,varianzas)
colnames(tabla)<- c("EXPERIMENTO","VARIANZA")

#ggplot(data3, aes(x=treatment, y=tiempos, fill=rutina)) + geom_boxplot()
ggplot(data2, aes(x =treatment, fill =rutina, y = tiempos)) +geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",binwidth=1/50)

wilcox.test(ot_residuo,at_potencia)
