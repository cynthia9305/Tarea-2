n <- 50

p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=abs(rnorm(n)))

dens<-0.5

p$r<-sqrt(p$m/(dens*pi))

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

p$g <- as.factor(p$g)

paso <- floor(256 / 10)

niveles <- seq(0, 255, paso)

#print(length(niveles))

colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)

#print(colores)

###########################graficar con ggplot

library(ggplot2)

ggplot() +
  
  geom_point(data=p, aes(x = p$x, y= p$y, size=p$r, color=p$g))+
  
  scale_colour_manual(values=colores)+ 
  
  ggtitle("Partículas")+
  
  theme(plot.title = element_text(hjust = 0.5))+
  
  guides(size=guide_legend(title = "radio"),color=guide_legend(title="carga"))+
  
  scale_x_continuous(name="x",limits = c(0, 1))+
  
  scale_y_continuous(name="y", limits = c(0, 1))

ggsave("P9_parts.png")

############################

eps <- 0.001

fuerza <- function(i) {
  
  xi <- p[i,]$x
  
  yi <- p[i,]$y
  
  ci <- p[i,]$c
  
  mi <- p[i,]$m
  
  fx <- 0
  
  fy <- 0
  
  for (j in 1:n) {
    
    cj <- p[j,]$c
    
    dir <- (-1)^(1 + 1 * (ci * cj < 0))#si se repele o une
    
    dx <- (xi - p[j,]$x)#distancia entres una particula y otra
    
    dy <- (yi - p[j,]$y)
    
    
    
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)#relaciona carga y distancia
    
    
    
    fx <- fx - dx/mi* factor
    
    fy <- fy - dy/mi* factor
    
    
    
  }
  
  return(c(fx, fy))
  
  
  
}

suppressMessages(library(doParallel))

registerDoParallel(makeCluster(detectCores() - 1))

tmax <- 100



#######################graficar estado inicial con ggplot

ggplot()+
  
  geom_point(data = p, aes(x= p$x, y= p$y, size=p$r, color= p$g))+
  
  scale_color_manual(values = colores)+
  
  ggtitle("Estado inicial")+
  
  theme(plot.title = element_text(hjust = 0.5))+
  
  guides(size=guide_legend(title = "tamaño"), color=guide_legend(title = "carga"))+
  
  scale_x_continuous(name = "x", limits = c(0,1))+
  
  scale_y_continuous(name = "y", limits = c(0,1))

ggsave("P9_pR1_0.png")



##############################



for (iter in 1:tmax) {
  
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  
  delta <- 0.009 / max(abs(f)) # que nadie desplace una paso muy largo
  
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * (f[c(TRUE, FALSE)][i]), 1), 0)
  
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * (f[c(FALSE, TRUE)][i]), 1), 0)
  
  
  
  ############################graficamos cada paso
  
  ggplot()+
    
    geom_point(data = p, aes(x= p$x, y= p$y, size=p$r, color= p$g))+
    
    scale_color_manual(values = colores)+
    
    ggtitle(paste("paso",iter))+
    
    theme(plot.title = element_text(hjust = 0.5))+
    
    guides(size=guide_legend(title = "tamaño"), color=guide_legend(title = "carga"))+
    
    scale_x_continuous(name = "x", limits = c(0,1))+
    
    scale_y_continuous(name = "y", limits = c(0,1))
  
  ggsave(paste("P9_pR1_",iter,".png"))
  
  ############################  
  
  
  
}



stopImplicitCluster()



ggplot(data = p, aes(x= p$m, y= p$r))+
  
  geom_point(size=2)+
  
  geom_smooth(method = "loess", se=FALSE, formula =y ~ x )+
  
  stat_summary(fun.y = mean, geom = "point",
               
               size = 0.5, color = "black")+
  
  ggtitle("Correlación radio-masa")+
  
  scale_x_continuous(name = "masa")+
  
  scale_y_continuous(name = "radio")+
  
  theme(plot.title = element_text(hjust = 0.5))+
  
  ggsave("P9T_c_radiomasa.png")



fit <- lm(p$r ~ p$m, data = p)

summary(fit)



library(magick)

frames=lapply(1:tmax,function(x) image_read(paste("P9_pR1_",x,".png")))

animation <- image_animate(image_join(frames), fps=100)

image_write(animation, paste("P9_Reto1", ".gif"))