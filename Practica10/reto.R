Tinicial=Sys.time()

library(testit)

library(lattice)

library(latticeExtra)

knapsack <- function(cap, peso, valor) {
  
  n <- length(peso)
  
  pt <- sum(peso) 
  
  assert(n == length(valor))
  
  vt <- sum(valor) 
  
  if (pt < cap) { 
    
    return(vt)
    
  } else {
    
    filas <- cap + 1 
    
    cols <- n + 1 
    
    tabla <- matrix(rep(-Inf, filas * cols),
                    
                    nrow = filas, ncol = cols) 
    
    for (fila in 1:filas) {
      
      tabla[fila, 1] <- 0 
      
    }
    
    rownames(tabla) <- 0:cap 
    
    colnames(tabla) <- c(0, valor) 
    
    for (objeto in 1:n) { 
      
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        
        anterior <- acum - peso[objeto]
        
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
          
        }
        
      }
      
    }
    
    return(max(tabla))
    
  }
  
}



factible <- function(seleccion, pesos, capacidad) {
  
  return(sum(seleccion * pesos) <= capacidad)
  
}

objetivo <- function(seleccion, valores) {
  
  return(sum(seleccion * valores))
  
}

normalizar <- function(data) {
  
  menor <- min(data)
  
  mayor <- max(data)
  
  rango <- mayor - menor
  
  data <- data - menor # > 0
  
  return(data / rango) # entre 0 y 1
  
}



generador.pesos <- function(cuantos, min, max) {
  
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
  
}



generador.valores <- function(pesos, min, max) {
  
  n <- length(pesos)
  
  valores <- double()
  
  for (i in 1:n) {
    
    media <- pesos[n]
    
    desv <- runif(1)
    
    valores <- c(valores, rnorm(1, media, desv))
    
  }
  
  valores <- normalizar(valores) * (max - min) + min
  
  return(valores)
  
}



poblacion.inicial <- function(n, tam) {
  
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  
  for (i in 1:tam) {
    
    pobl[i,] <- round(runif(n))
    
  }
  
  return(as.data.frame(pobl))
  
}



mutacion <- function(sol, n) {
  
  pos <- sample(1:n, 1)
  
  mut <- sol
  
  mut[pos] <- (!sol[pos]) * 1
  
  return(mut)
  
}



reproduccion <- function(x, y, n) {
  
  pos <- sample(2:(n-1), 1)
  
  xy <- c(x[1:pos], y[(pos+1):n])
  
  yx <- c(y[1:pos], x[(pos+1):n])
  
  return(c(xy, yx))
  
}

n <- 50

pesos <- generador.pesos(n, 15, 80)

valores <- generador.valores(pesos, 10, 500)

capacidad <- round(sum(pesos) * 0.65)

optimo <- knapsack(capacidad, pesos, valores)

init <- 200



suppressMessages(library(doParallel))

registerDoParallel(makeCluster(detectCores() - 1))



#Paralelizar mutaciones

mutar<-function(i){
  
  if (runif(1) < pm) {
    
    return( mutacion (unlist(p[i,]), n))
    
  }
  
}

#Paralelizar cruces

cruces<-function(i){
  
  padres <- sample(1:tam, 2, replace=FALSE)
  
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  
  h1 <-hijos[1:n] # primer hijo
  
  h2 <-hijos[(n+1):(2*n)] # segundo hijo
  
  hijo<- rbind(h1,h2)
  
  return(hijo)
  
}

#Paralelizar cruces ruleta

crucesruleta<-function(i){
  
  padres <- sample(1:tam, 2, prob=bestpadres, replace=FALSE)
  
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  
  h1 <-hijos[1:n] # primer hijo
  
  h2 <-hijos[(n+1):(2*n)] # segundo hijo
  
  hijo<- rbind(h1,h2)
  
  return(hijo)
  
}

#Paralelizar factbilidad

factibilidad <- function(i){
  
  obj <- c(objetivo(p[i,], valores))
  
  fact <- c(factible(p[i,], pesos, capacidad))
  
  resul<-(cbind(obj,fact))
  
  return(resul)
  
}

for (replica in 1:2){
  
  
  
  #Tarea base paralelizado con ruleta
  
  p <- poblacion.inicial(n, init)
  
  tam <- dim(p)[1]
  
  assert(tam == init)
  
  pm <- 0.05
  
  rep <- 50
  
  tmax <- 50
  
  Rmejores <- double()
  
  
  
  for (iter in 1:tmax) {
    
    
    
    p$obj <- NULL
    
    p$fact <- NULL
    
    p<-rbind(p, foreach(i=1:tam, .combine = rbind) %dopar% mutar(i)) #MUTAR
    
    bestpadres<-foreach(i=1:tam, .combine=c) %dopar% objetivo(p[i,],valores)#Calcular lo valores de cada individuo
    
    bestpadres<-bestpadres/sum(bestpadres)
    
    p<-rbind(p,foreach(i=1:rep, .combine=rbind) %dopar% crucesruleta(i)) #CRUCES
    
    tam <- dim(p)[1]
    
    obj <- double()
    
    fact <- integer()
    
    rownames(p)<-c(1:dim(p)[1])
    
    p<-data.frame(sapply(p,function(x)as.numeric(as.character(x))))#Convertir de caracter a numerico
    
    p<-cbind(p,foreach(i=1:tam, .combine=rbind) %dopar% factibilidad(i)) #FACTBILIDAD
    
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    
    p <- p[mantener,]
    
    tam <- dim(p)[1]
    
    assert(tam == init)
    
    factibles <- p[p$fact == TRUE,]
    
    Rmejor <- max(factibles$obj)
    
    Rmejores <- c(Rmejores, Rmejor)
    
  }
  
  
  
  #Tarea base paralelizado sin ruleta
  
  p <- poblacion.inicial(n, init)
  
  tam <- dim(p)[1]
  
  assert(tam == init)
  
  pm <- 0.05
  
  rep <- 50
  
  tmax <- 50
  
  mejores <- double()
  
  
  
  for (iter in 1:tmax) {
    
    p$obj <- NULL
    
    p$fact <- NULL
    
    
    
    p<-rbind(p, foreach(i=1:tam, .combine = rbind) %dopar% mutar(i)) #MUTAR
    
    
    
    p<-rbind(p,foreach(i=1:rep, .combine=rbind) %dopar% cruces(i)) #CRUCES
    
    tam <- dim(p)[1]
    
    obj <- double()
    
    fact <- integer()
    
    rownames(p)<-c(1:dim(p)[1])
    
    p<-data.frame(sapply(p,function(x)as.numeric(as.character(x))))
    
    p<-cbind(p,foreach(i=1:tam, .combine=rbind) %dopar% factibilidad(i)) #FACTBILIDAD
    
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    
    p <- p[mantener,]
    
    tam <- dim(p)[1]
    
    assert(tam == init)
    
    factibles <- p[p$fact == TRUE,]
    
    mejor <- max(factibles$obj)
    
    mejores <- c(mejores, mejor)
    
  }
  
  
  
  stopImplicitCluster() 
  
  
  
  sinruleta=xyplot(mejores~1:tmax,ylim=c(0.95*min(mejores), 1.05*optimo),xlab = "Paso",ylab = "Valor Mayor", 
                   
                   key=list(space="right",
                            
                            lines=list(col=c("green","blue"), lty=c(1,1), lwd=2),
                            
                            text=list(c("Con selección de ruleta","Sin selección de ruleta"))
                            
                   ),
                   
                   panel=function(x,y,subscripts){
                     
                     panel.grid(h=-1,v=-1)
                     
                     panel.xyplot(x,y)
                     
                     panel.stripplot(x,y,
                                     
                                     subscripts = subscripts,pch=19,type="o",col=c("blue"))
                     
                     panel.abline(h=optimo,col="pink",lwd=2)
                     
                     
                     
                   })
  
  
  
  
  
  conruleta=xyplot(Rmejores~1:tmax,ylim=c(0.95*min(Rmejores), 1.05*optimo),xlab = "Paso",ylab = "Mayor valor",
                   
                   key=list(space="right",
                            
                            lines=list(col=c("green","blue"), lty=c(1,1), lwd=1),
                            
                            text=list(c("Con selección de ruleta","Sin selección de ruleta"))
                            
                   ),
                   
                   panel=function(x,y,subscripts){
                     
                     panel.grid(h=-1,v=-1)
                     
                     panel.xyplot(x,y)
                     
                     panel.stripplot(x,y,
                                     
                                     subscripts = subscripts,pch=19,type="o",col=c("green"))
                     
                     panel.abline(h=optimo,col="pink",lwd=2)
                     
                   })
  
}

png(paste("P130_Reto1",replica,".png"), width=600, height=300)

sinruleta + as.layer(conruleta)

graphics.off()



print(paste(mejor, (optimo - mejor) / optimo))

Tfinal=Sys.time()

Tiempo=Tfinal-Tinicial

print(Tiempo)