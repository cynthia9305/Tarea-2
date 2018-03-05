radio <- 1 

real <- 3.141592

muestras <- c(100, 500, 1000, 5000, 10000, 50000)

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

#runif samples from a uniform distribution

xs <- runif(runs,min=-0.5,max=0.5)

ys <- runif(runs,min=-0.5,max=0.5)

in.circle <- xs^2 + ys^2 <= 0.5^2

mc.pi <- (sum(in.circle)/runs)*4

png("pi.png")

plot(xs,ys,pch='.',col=ifelse(in.circle,"red","grey")
     
     ,xlab="",ylab="",asp=1,
     
     main=paste("Aproximación de Pi =",mc.pi))


graphics.off()