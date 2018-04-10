g <- function(x, y) {
  
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
  
}


low <- -4

high <- 4

step <- 0.25

replicas <- 100


replica <- function(t) {
  
  curr <- c(runif(1, low, high),runif(1, low, high))
  
  best <- curr
  
  for (tiempo in 1:t) {
    
    delta <- runif(1, 0, step)
    
    pnts<- c(curr[1]+delta,curr[2],curr[1]-delta,curr[2],curr[1],curr[2]+delta,curr[1],curr[2]-delta,curr[1],curr[2])
    
    valores <- c(g(pnts[1],pnts[2]),g(pnts[3],pnts[4]),g(pnts[5],pnts[6]),g(pnts[7],pnts[8]),g(curr[1],curr[2]))
    
    v <- which.max(valores)
    
    curr<- c(pnts[(v*2)-1],pnts[v*2])
    
    if (g(curr[1],curr[2]) > g(best[1],best[2])) {
      
      best <- curr
      
    }
    
  }
  
  return(best)
  
}



suppressMessages(library(doParallel))

registerDoParallel(makeCluster(detectCores() - 1))



for (pot in 2:4) {
  
  tmax <- 10^pot
  
  resultados <- foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax)
  
  resux <- c()
  
  resuy <- c()
  
  valores <- c()
  
  for(i in seq(1,(replicas*2),2)){
    
    resux <- c(resux,resultados[i])
    
    resuy <- c(resuy,resultados[i+1])
    
  }
  
  for(i in 1:replicas){
    
    valores <- c(valores,g(resux[i],resuy[i]))
    
  }
  
  mejor <- which.max(valores)
  
  
  
  x <- seq(-4, 4, 0.25)
  
  y <- x
  
  z <- outer(x, y, g)
  
  dimnames(z) <- list(x, y)
  
  library(reshape2)
  
  d <- melt(z)
  
  names(d) <- c("x", "y", "z")
  
  library(lattice)
  
  png(paste("p7_", tmax, ".png", sep=""), width=700, height=700)
  
  plot(levelplot(z ~ x * y, data = d))
  
  trellis.focus("panel", 1, 1, highlight=FALSE)
  
  for(i in 1:replicas){
    
    lpoints(resux[i], resuy[i], pch=1, col="purple", cex=1)
    
  }
  
  trellis.unfocus()
  
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  
  lpoints(resux[mejor],resuy[mejor], pch=19, col="red", cex=1)
  
  if((resux[mejor]>5)|(resux[mejor]<(-6))|(resuy[mejor]<(-6))|(resuy[mejor]>5)){
    
    adentrox <- c()
    
    adentroy <- c()
    
    val <- c()
    
    for(v in 1:replicas){
      
      if((resux[v]>-6)&(resux[v]<5)&(resuy[v]>-6)&(resuy[v]<5)){
        
        adentrox <- c(adentrox,resux[v])
        
        adentroy <- c(adentroy,resuy[v])
        
        val <- c(val,g(adentrox[v],adentroy[v]))
        
      }
      
    }
    
    a<-which.max(val)
    
    lpoints(adentrox[a],adentroy[a], pch=19, col="red", cex=1)
    
  }
  
  trellis.unfocus()
  
  graphics.off()
  
}

stopImplicitCluster()