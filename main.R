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

source("./generate_arrivals.R")
source("./generate_services.R")
source("./generate_queue.R")

# ========== 2.CHOIX DES PARAMETRES ============================================

start.prog = Sys.time()
Tmax = 500
lambda =.3 #lambda grand <=> les clients arrivent rapidement
mu = .7 #mu grand <=> les services sont rendus rapidement

k_X = 1 #k_X = 1 pour exponentielle
k_Y = 1 #k_Y = 1 pour exponentielle

# ========== 3.GENERATION DES VECTEURS ET MATRICES =============================

start.time = Sys.time()
X = generate_arrivals(Tmax, lambda, k_X)
end.time = Sys.time()
cat(sprintf("Génération de X : %.2fs\n", end.time - start.time))

start.time = Sys.time()
Y = generate_services(X, mu, k_Y)
end.time = Sys.time()
cat(sprintf("Génération de Y : %.2fs\n", end.time - start.time))

start.time = Sys.time()
N = generate_queue(X,Y)
end.time = Sys.time()
cat(sprintf("Génération de N : %.2fs\n \n", difftime(end.time, start.time, units = "secs")))

# ========== 4.AFFICHAGE DES GRAPHIQUES ========================================

abs = seq(0,Tmax, by = 0.01)

par(mfrow=c(2,2))

plot(N[,1],N[,3], type="s", 
     main = "Nombre de clients dans la queue", 
     xlab = "Temps", 
     ylab = "Nombre")
abline(v = Tmax, col="red")

hist(X[-1]-X[-length(X)], main = "Distribution des arrivées", 
     xlab = "", ylab="", breaks = Tmax/10, freq = FALSE)
lines(abs, dweibull(abs, scale = 1/lambda, shape = k_X), col = "red")

hist(Y-X, main = "Distribution des sorties", 
     xlab = "", ylab="", breaks = Tmax/10, freq = FALSE)
lines(abs, dweibull(abs, scale = 1/mu, shape = k_Y), col = "red")

hist(N[,3], main = "Distribution du nombre de clients", 
     xlab = "", ylab="", freq = FALSE, breaks = max(N[,3])+1)

# ========== 5.AFFICHAGE DE RESULTATS TEXTUELS =================================

cat(sprintf("Nombre de clients total : %.0f \n", sum(N[,2] == 1)))
cat(sprintf("Nombre de clients à la fin : %.0f \n", N[dim(N)[1],3]))
cat(sprintf("Nombre maximal de clients : %.0f \n", max(N[,3])))
cat(sprintf("Nombre moyen de clients : %.2f \n\n", (N[-1,3]%*%(N[-1,1]-N[-dim(N)[1],1]))/N[dim(N)[1],1]))

end.prog = Sys.time()
cat(sprintf("Programme : %.2fs\n \n", difftime(end.prog, start.prog, units = "secs")))

# ============== 6.CALCUL DES PROBABILITES =========================================

# Pilot Stage
l=20
l0 = 2
N1 = 100

list.X = list()
list.Y = list()
list.N = list()

for(n1 in 1:N1){
  # inilisation
  if(n1 == 1){
    list.X = list(generate_arrivals(Tmax, lambda, k_X))
    list.Y = list(generate_services(list.X[[n1]], mu, k_Y))
    list.N = list(generate_queue(list.X[[n1]],list.Y[[n1]])[,3])
  }else{
    list.X = c(list.X, list(generate_arrivals(Tmax, lambda, k_X)))
    list.Y = c(list.Y, list(generate_services(list.X[[n1]], mu, k_Y)))
    list.N = c(list.N, list(generate_queue(list.X[[n1]],list.Y[[n1]])[,3]))
  }
}

N = length(list.N)

source("./indicator_event.R")
source("./estimate_f.R")
source("./estimate_W.R")
source("./update_v.R")

new_v = update_v(lambda0, mu0, list.N, list.X, list.Y, lambda, mu, l0, N)
print(new_v)


