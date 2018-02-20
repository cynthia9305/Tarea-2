primo <- function(n) {

    if (n == 1 || n == 2) {

        return(TRUE)

    }

    if (n %% 2 == 0) {

        return(FALSE)

    }

    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {

        if ((n %% i) == 0) {

            return(FALSE)

        }

    }

    return(TRUE)

}

 

basedatos = read.csv("primes1.txt", sep=" ", header=FALSE) 

basedatos$V1 = NULL 

basedatos$V10 = NULL

m = as.matrix(basedatos) 

v = as.vector(t(m)) 

vi <- v[seq(1, 10000000, 20000)]

vp <- v[9999900:10000000]

vr <- v[1:100]

vnp <- (1:100)

vf <- sort(rbind (vr, vi, vp, vnp))



menmay <- vf[1:100]

maymen <- vf[100:1]

aleatorio1 <- sample(maymen)

aleatorio2 <- sample(maymen)

aleatorio3 <- sample(maymen)




replicas <- 10

suppressMessages(library(doParallel))

core <- (detectCores()-1)

for (nucleo in 1:core) {

registerDoParallel(makeCluster(detectCores() - nucleo))

ot <-  numeric()

it <-  numeric()

at1 <-  numeric()

at2 <-  numeric()

at3 <-  numeric()


for (r in 1:replicas) {

    ot <- c(ot, system.time(foreach(n = menmay, .combine=c) %dopar% primo(n))[3]) # de menor a mayor

    it <- c(it, system.time(foreach(n = maymen, .combine=c) %dopar% primo(n))[3]) # de mayor a menor

    	at1 <- c(at1, system.time(foreach(n = aleatorio1, .combine=c) %dopar% primo(n))[3]) # orden aleatorio

	at2 <- c(at2, system.time(foreach(n = aleatorio2, .combine=c) %dopar% primo(n))[3]) # orden aleatorio

	at3 <- c(at3, system.time(foreach(n = aleatorio3, .combine=c) %dopar% primo(n))[3]) # orden aleatorio

	


}

stopImplicitCluster()

summary(ot)

summary(it)

summary(at1)

summary(at2)

summary(at3)


tiempo <- t(rbind(ot, it, at1, at2, at3))

boxplot (tiempo)
}