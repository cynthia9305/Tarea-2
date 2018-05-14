library('ggplot2')
pa <- as.data.frame(read.csv("TiempPar.csv"))
sec <-as.data.frame(read.csv("TiempSec.csv"))
#por<-as.data.frame(read.csv("porciento200.csv"))
ambos<- data.frame()
ambos<-rbind(pa,sec)
ambos$Cantidad <- as.factor(ambos$Cantidad)
png(paste("AmbosTiempos.png", sep=""), width=700, height=700)
ggplot(data=ambos,aes(x=Cantidad,y=Tiempo,fill=Tipo))+geom_boxplot()+ylab("Tiempos (s)")#stat_summary(fun.y=mean,geom="smooth",aes(group=Tipo,col=Tipo))
graphics.off()



png(paste("porcentaje.png", sep=""), width=700, height=700)
por$Funciones <- as.factor(por$Funciones)
ggplot(data=por,aes(x=Funciones,y=Dominadores,fill=Funciones)) + geom_violin(fill="deeppink1", color="yellow", scale = "width") + geom_boxplot(width=0.2, fill="green", color="white", lwd=2) +
  xlab("Funciones") +
  ylab("Porcentaje") +theme(axis.text=element_text(size=17),axis.title=element_text(size=17,face="bold"))
graphics.off()

png(paste("pord.png", sep=""), width=700, height=700)
por$Funciones <- as.factor(por$Funciones)
ggplot(data=por,aes(x=Funciones,y=Porcentaje,fill=Funciones)) + geom_boxplot() +
  xlab("Funciones") +
  ylab("Porcentaje") +theme(axis.text=element_text(size=17),axis.title=element_text(size=17,face="bold"))
graphics.off()