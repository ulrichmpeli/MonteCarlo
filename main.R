# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = =
# ENSAE - 2A - Simulation et Monte Carlo
#    Sujet : Files d'attente
#       Chargé de TD : Alexander Buchholz
#       Etudiants : Romain Luong, Peter Martigny, Mehdi Miah, Ulrich Mpeli
#
#       Fichier : main.R
#       Description : fonction principal
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = =

# ========== 1.PREAMBULE =======================================================


rm(list=ls())
cat("\014") 

source("./generate_arrivals.R")
source("./generate_services.R")
source("./generate_queue.R")

# ========== 2.CHOIX DES PARAMETRES ============================================

start.prog = Sys.time()
Tmax = 100
lambda = 0.3 #lambda grand <=> les clients arrivent rapidement
mu = .7     #mu grand <=> les services sont rendus rapidement

k_X = 1   #k_X = 1 pour exponentielle
k_Y = 1   #k_Y = 1 pour exponentielle

# ========== 3.GENERATION DES VECTEURS ET MATRICES =============================

start.time = Sys.time()
X = generate_arrivals(Tmax, lambda, k_X)
end.time = Sys.time()
cat(sprintf("Génération de X : %.2fs\n", end.time - start.time))

start.time = Sys.time()
Y = generate_services(X, mu, k_Y, Tmax)
end.time = Sys.time()
cat(sprintf("Génération de Y : %.2fs\n", end.time - start.time))

start.time = Sys.time()
Q = generate_queue(X,Y)
end.time = Sys.time()
cat(sprintf("Génération de Q : %.2fs\n \n", difftime(end.time, start.time, units = "secs")))

# ========== 4.AFFICHAGE DES GRAPHIQUES ========================================

abs = seq(0,Tmax, by = 0.01)

par(mfrow=c(2,2))

plot(Q$date,Q$number, type="s", 
     main = "Nombre de clients dans la queue", 
     xlab = "Temps", 
     ylab = "Nombre")
abline(v = Tmax, col="red")

hist(X[-1]-X[-length(X)], main = "Distribution des arrivées", 
     xlab = "", ylab="", breaks = Tmax/10, freq = FALSE)
lines(abs, dweibull(abs, scale = 1/lambda, shape = k_X), col = "red")

hist(Y[-1]-Y[-length(Y)], main = "Distribution des sorties", 
     xlab = "", ylab="", breaks = Tmax/10, freq = FALSE)
lines(abs, dweibull(abs, scale = 1/mu, shape = k_Y), col = "red")

hist(Q$number, main = "Distribution du nombre de clients", 
     xlab = "", ylab="", freq = FALSE, breaks = max(Q$number)+1)

# ========== 5.AFFICHAGE DE RESULTATS TEXTUELS =================================

cat(sprintf("Nombre de clients total : %.0f \n", sum(Q$arrival == 1)))
cat(sprintf("Nombre de clients à la fin : %.0f \n", Q$number[dim(Q)[1]]))
cat(sprintf("Nombre maximal de clients : %.0f \n", max(Q$number)))
cat(sprintf("Nombre moyen de clients : %.2f \n\n", (Q$number[-1]%*%(Q$date[-1]-Q$date[-dim(Q)[1]]))/Q$date[dim(Q)[1]]))

end.prog = Sys.time()
cat(sprintf("Programme : %.2fs\n \n", difftime(end.prog, start.prog, units = "secs")))

# ============== 6.CALCUL DES PROBABILITES =========================================

l=20

# Pilot Stage
l0 = 8
N1 = 100

list.X = list()
list.Y = list()
list.Q = list()

# génération des N1 queues
for(n1 in 1:N1){
  # inilisation
  if(n1 == 1){
    list.X = list(generate_arrivals(Tmax, lambda, k_X))
    list.Y = list(generate_services(list.X[[n1]], mu, k_Y, Tmax))
    list.Q = list(generate_queue(list.X[[n1]],list.Y[[n1]])[,3])
  }else{
    list.X = c(list.X, list(generate_arrivals(Tmax, lambda, k_X)))
    list.Y = c(list.Y, list(generate_services(list.X[[n1]], mu, k_Y, Tmax)))
    list.Q = c(list.Q, list(generate_queue(list.X[[n1]],list.Y[[n1]])[,3]))
  }
}

# génération des intervalles d'arrivées et de sorties
list.dX = list()
for(n in 1:N1){
  list.dX[[n]] = list.X[[n]][2:length(list.X[[n]])] - list.X[[n]][1:length(list.X[[n]])-1]
}

list.dY = list()
for(n in 1:N1){
  list.dY[[n]] = list.Y[[n]][2:length(list.Y[[n]])] - list.Y[[n]][1:length(list.Y[[n]])-1]
}

source("./indicator_event.R")
source("./estimate_f.R")
source("./estimate_W.R")
source("./update_v.R")

lambda0 = lambda
mu0 = mu

new_v = update_v(lambda0, mu0, list.dX, list.dY, list.Q, lambda, mu, l0)
print(new_v)


