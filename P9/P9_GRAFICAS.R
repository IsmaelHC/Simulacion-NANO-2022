n <- 50
suav=0.0005 #suavizante del componente de la masa
gra=9.81
fuerza <- function(i) {
    xi <- p[i,]$x
    yi <- p[i,]$y
    ci <- p[i,]$c
    mi <- p[i,]$m
    fx <- 0
    fy <- 0
    for (j in 1:n) {
        cj <- p[j,]$c
        mj <- p[j,]$m
        dir <- (-1)^(1 + 1 * (ci * cj < 0))
        dx <- xi - p[j,]$x
        dy <- yi - p[j,]$y
        masa=(gra*mi*mj)*(suav)/(((sqrt(dx^2 + dy^2))/2)^2 + eps)
        carga=abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
        factor <- dir * (carga+masa)
        #factor <- dir * (carga)
        fx <- fx - dx * factor
        fy <- fy - dy * factor
    }
    return(c(fx, fy))
}
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n),m=sample(1:5,n,replace=T) )
p_0=p
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
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
eps <- 0.001
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
plot(p$x, p$y, col=colores[p$g+6], pch=16, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()
for (iter in 1:tmax) {
    f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
    delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
    p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
    p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
    tl <- paste(iter, "", sep="")
    while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
    }
    png(paste("p9_t", tl,"_MC", ".png", sep=""))
    plot(p$x, p$y, col=colores[p$g+6], pch=16, cex=p$m, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),auto.key=list(space="right"),main=paste("Paso", iter), xlab="X", ylab="Y")
    
    graphics.off()
}
stopImplicitCluster()

