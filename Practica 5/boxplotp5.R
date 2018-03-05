radio <- 1 

real <- 3.141592

muestras <- c(100, 500, 1000, 5000, 7500, 10000, 50000)

iteracion <- 20

i = 1

cuantos <- 500

error <- numeric(length(6*iteracion))



valor <- function() {
  
  base <- runif(muestra, -radio, radio)
  
  altura <- runif(muestra, -radio, radio)
  
  return(sum(base**2 + altura**2 <= 1))
  
}



suppressMessages(library(doParallel))

registerDoParallel(makeCluster(detectCores() - 1))

for(muestra in muestras){
  
  for (replica in 1:iteracion) {
    
    montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% valor()
    
    aproximado <- sum(montecarlo)/(cuantos * muestra)
    
    aprox <- (4*aproximado)
    
    porcentaje <- abs(((real - aprox)/real)*100)
    
    error[i] <- porcentaje
    
    i = i + 1
    
  }
  
}

stopImplicitCluster()

mat <- matrix(error, ncol=7, nrow=iteracion)

png("p54789.png")

lbls = c("100", "1000", "5000", "7500", "10000","25000","50000")

boxplot(mat, col=c(17,18,19, 20, 21, 22), names=lbls, ylab="Error", xlab="Muestra")

graphics.off()