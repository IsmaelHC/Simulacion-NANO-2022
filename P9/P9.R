n <- 25
gra=9.81
vel_total_sc=numeric()
vel_total_cm=numeric()
replicas=15
suav=0.005 #suavizante del componente de la masa
fuerza_cm <- function(i) {
    xi <- p_cm[i,]$x
    yi <- p_cm[i,]$y
    ci <- p_cm[i,]$c
    mi <- p_cm[i,]$m
    fx <- 0
    fy <- 0
    for (j in 1:n) {
        cj <- p_cm[j,]$c
        mj <- p_cm[j,]$m
        dir <- (-1)^(1 + 1 * (ci * cj < 0))
        dx <- xi - p_cm[j,]$x
        dy <- yi - p_cm[j,]$y
        masa=(gra*mi*mj)*(suav)/(((sqrt(dx^2 + dy^2))/2)^2 + eps)
        carga=abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
        factor <- dir *( carga+masa)
        fx <- fx - dx * factor
        fy <- fy - dy * factor
    }
    return(c(fx, fy))
}
fuerza_sc <- function(i) {
    xi <- p_sc[i,]$x
    yi <- p_sc[i,]$y
    ci <- p_sc[i,]$c
    mi <- p_sc[i,]$m
    fx <- 0
    fy <- 0
    for (j in 1:n) {
        cj <- p_sc[j,]$c
        mj <- p_sc[j,]$m
        dir <- (-1)^(1 + 1 * (ci * cj < 0))
        dx <- xi - p_sc[j,]$x
        dy <- yi - p_sc[j,]$y
        factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
        fx <- fx - dx * factor
        fy <- fy - dy * factor
    }
    return(c(fx, fy))
}

for(r in 1:replicas){
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n),m=sample(1:5,n,replace=T) )
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
p_0=p
p_sc=p
p_cm=p
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
eps <- 0.001
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
tmax <-50
for (iter in 1:tmax) {
    f_sc <- foreach(i = 1:n, .combine=c) %dopar% fuerza_sc(i)
    delta_sc<- 0.02 / max(abs(f_sc)) # que nadie desplace una paso muy largo
    p_sc$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p_sc[i,]$x + delta_sc * f_sc[c(TRUE, FALSE)][i], 1), 0)
    p_sc$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p_sc[i,]$y + delta_sc * f_sc[c(FALSE, TRUE)][i], 1), 0)
    
    f_cm <- foreach(i = 1:n, .combine=c) %dopar% fuerza_cm(i)
    delta_cm<- 0.02 / max(abs(f_cm)) # que nadie desplace una paso muy largo
    p_cm$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p_cm[i,]$x + delta_cm * f_cm[c(TRUE, FALSE)][i], 1), 0)
    p_cm$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p_cm[i,]$y + delta_cm * f_cm[c(FALSE, TRUE)][i], 1), 0)


    }
stopImplicitCluster()
distancia_sc=sqrt(((p_0$x-p_sc$x)^2)+((p_0$y-p_sc$y)^2))
velocidad_sc=distancia_sc/tmax
vel_total_sc=c(vel_total_sc,sum(velocidad_sc))

distancia_cm=sqrt(((p_0$x-p_cm$x)^2)+((p_0$y-p_cm$y)^2))
velocidad_cm=distancia_cm/tmax
vel_total_cm=c(vel_total_cm,sum(velocidad_cm))
}
wilcox.test(vel_total_cm,vel_total_sc)
mean(vel_total_sc)
mean(vel_total_cm)
