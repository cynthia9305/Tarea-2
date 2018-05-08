suppressMessages(library(doParallel))
suppressMessages(library(foreach))
registerDoParallel(makeCluster(2))
tm <- data.frame()
resultados <- data.frame()

library(ggplot2)
n <- 200
for (i in 1:50){ 
  for (k in seq(2, 14, 2)){
    source('P11_Tiempo.R', encoding = 'UTF-8')
    tm <- cbind(dim(frente)[1], k, n)
    resultados <- rbind(resultados, tm)
  }
}
stopImplicitCluster()

names(resultados)=c("Dominadores", "Objetivos", "Soluciones")
resultados$Objetivos <- as.factor(resultados$Objetivos)
#resultados$Soluciones <- as.factor(resultados$Soluciones)

ggplot(data = resultados, aes(resultados$Objetivos, resultados$Dominadores/n)) +
  geom_violin(scale="width",fill="gold", color="firebrick")+
  geom_boxplot(width=0.2, fill="slateblue1", color="springgreen3")+ 
  xlab("Número de funciones objetivo") +
  ylab("Porcentaje")+ 
  # ggtitle("Cantidad de soluciones dominantes")
  ggsave(file=paste("P11_Violin.png", sep=""))