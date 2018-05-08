datos <- data.frame()

for(i in 1:5){
  for (k in c(10, 20, 30, 40, 50)) {
    source('P11_NP.R', encoding = 'UTF-8')
    to <- cbind("NP", k, t)
    source('P11_P.R', encoding = 'UTF-8')
    tm <- cbind("P", k, t)
    datos <- rbind(datos, to, tm)
  }
}

save.image(file = "Datos.RData")

names(datos) <- c("Tipo","k", "Tiempo")
datos$k <- as.numeric(levels(datos$k))[datos$k]
datos$Tiempo <- as.numeric(levels(datos$Tiempo))[datos$Tiempo]
datos$Tipo <- as.factor(datos$Tipo)
png("tiempos.png", width = 800, height = 800, pointsize = 15)
boxplot(Tiempo ~ Tipo * k, data = datos, col = ("lightgrey"), border = c("red", "blue"), xlab = "Número de funciones objetivo", ylab = "Tiempo")
legend("topleft", inset = 0.02, c("No Paralelo", "Paralelo"), fill = c("red", "blue"), horiz=TRUE, cex=0.8, box.lty = 0)
graphics.off()