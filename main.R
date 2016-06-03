# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = =
# ENSAE - 2A - Simulation et Monte Carlo
#    Sujet : Files d'attente
#       ChargÃ© de TD : Alexander Buchholz
#       Etudiants : Romain Luong, Peter Martigny, Mehdi Miah, Ulrich Mpeli
#
#       Fichier : main.R
#       Description : fonction principal
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = =

# ========== 1.PREAMBULE =======================================================

rm(list=ls())
cat("\014") 

source("./generate_suite_Weibull.R")
source("./generate_arrivals.R")
source("./generate_departures.R")
source("./generate_queue.R")

# ========== 2.CHOIX DES PARAMETRES ============================================

Tmax = 100
lambda0 = 0.4 #lambda grand <=> les clients arrivent rapidement
mu0 = 0.6    #mu grand <=> les services sont rendus rapidement

k_X = 1   #k_X = 1 pour exponentielle
k_Y = 1   #k_Y = 1 pour exponentielle

# ========== 3.GENERATION DES VECTEURS ET MATRICES =============================

dX = generate_suite_Weibull(Tmax, lambda0, k_X)
dY = generate_suite_Weibull(Tmax, mu0, k_Y)

X = generate_arrivals(dX)
Y = generate_departures(X, dY,Tmax)

Q = generate_queue(X,Y)

# ========== 4.AFFICHAGE DES GRAPHIQUES ========================================

abs = seq(0,Tmax, by = 0.01)

par(mfrow=c(2,2))

plot(Q$date,Q$number, type="s", 
     main = "Nombre de clients dans la queue", 
     xlab = "Temps", 
     ylab = "Nombre")
abline(v = Tmax, col="red")

hist(dX, main = "Distribution des arrivÃ©es", 
     xlab = "", ylab="", breaks = Tmax/10, freq = FALSE)
lines(abs, dweibull(abs, scale = 1/lambda0, shape = k_X), col = "red")

hist(dY, main = "Distribution des sorties", 
     xlab = "", ylab="", breaks = Tmax/10, freq = FALSE)
lines(abs, dweibull(abs, scale = 1/mu0, shape = k_Y), col = "red")

hist(Q$number, main = "Distribution du nombre de clients", 
     xlab = "", ylab="", freq = FALSE, breaks = max(Q$number)+1)

# ========== 5.AFFICHAGE DE RESULTATS TEXTUELS =================================

cat(sprintf("Nombre de clients total : %.0f \n", sum(Q$arrival == 1)))
cat(sprintf("Nombre de clients à la fin : %.0f \n", Q$number[dim(Q)[1]]))
cat(sprintf("Nombre maximal de clients : %.0f \n", max(Q$number)))
cat(sprintf("Nombre moyen de clients : %.2f \n\n", (Q$number[-1]%*%(Q$date[-1]-Q$date[-dim(Q)[1]]))/Q$date[dim(Q)[1]]))

# ============== 6.CALCUL DES PROBABILITES =========================================

l=15

# Pilot Stage
l0 = 5
N1 = 10000

list.dX = list()
list.dY = list()
list.X = list()
list.Y = list()
list.Q = list()

# gÃ©nÃ©ration des N1 queues
for(n1 in 1:N1){
  # initialisation
  if(n1 == 1){
    list.dX = list(generate_suite_Weibull(Tmax, lambda0, k_X))
    list.dY = list(generate_suite_Weibull(Tmax, mu0, k_Y))
    list.X = list(generate_arrivals(list.dX[[n1]]))
    list.Y = list(generate_departures(list.X[[n1]], list.dY[[n1]],Tmax))
    list.Q = list(generate_queue(list.X[[n1]],list.Y[[n1]])[,3])
  }else{
    list.dX = c(list.dX, list(generate_suite_Weibull(Tmax, lambda0, k_X)))
    list.dY = c(list.dY, list(generate_suite_Weibull(Tmax, mu0, k_Y)))
    list.X = c(list.X, list(generate_arrivals(list.dX[[n1]])))
    list.Y = c(list.Y, list(generate_departures(list.X[[n1]], list.dY[[n1]],Tmax)))
    list.Q = c(list.Q, list(generate_queue(list.X[[n1]],list.Y[[n1]])[,3]))
  }
}

source("./indicator_event.R")
source("./estimate_f.R")
source("./estimate_W.R")
source("./update_v.R")

lambda = lambda0
mu = mu0

new_v = update_v(lambda0, mu0, list.dX, list.dY, list.Q, lambda, mu, l0)
print(new_v)

# second stage
N2 = 500

epsilon = 0.05
diflambda = abs(new_v[1] - lambda)
difmu = abs(new_v[2] - mu)

while (diflambda > epsilon | difmu > epsilon) {
  lambda1 = lambda
  mu1 = mu
  lambda = new_v[1]
  mu = new_v[2]

  list.dX = list()
  list.dY = list()
  list.X = list()
  list.Y = list()
  list.Q = list()
  
  # gÃ©nÃ©ration des N1 queues
  for(n2 in 1:N2){
    # initialisation
    if(n2 == 1){
      list.dX = list(generate_suite_Weibull(Tmax, lambda, k_X))
      list.dY = list(generate_suite_Weibull(Tmax, mu, k_Y))
      list.X = list(generate_arrivals(list.dX[[n2]]))
      list.Y = list(generate_departures(list.X[[n2]], list.dY[[n2]],Tmax))
      list.Q = list(generate_queue(list.X[[n2]],list.Y[[n2]])[,3])
    }else{
      list.dX = c(list.dX, list(generate_suite_Weibull(Tmax, lambda, k_X)))
      list.dY = c(list.dY, list(generate_suite_Weibull(Tmax, mu, k_Y)))
      list.X = c(list.X, list(generate_arrivals(list.dX[[n2]])))
      list.Y = c(list.Y, list(generate_departures(list.X[[n2]], list.dY[[n2]],Tmax)))
      list.Q = c(list.Q, list(generate_queue(list.X[[n2]],list.Y[[n2]])[,3]))
    }
  }

  new_v = update_v(lambda1, mu1, list.dX, list.dY, list.Q, lambda, mu, l)
  
  diflambda = abs(new_v[1] - lambda)
  difmu = abs(new_v[2] - mu)
  
  print(new_v)
}


# Third stage

#génération de la queue avec les paramètres optimaux

lambda_opt = new_v[1]
mu_opt = new_v[2]

#lambda_opt = 0.716
#mu_opt = 0.296

N3 = 2500

for(n3 in 1:N3){
  # initialisation
  if(n3 == 1){
    list.dX_opt = list(generate_suite_Weibull(Tmax, lambda_opt, k_X))
    list.dY_opt = list(generate_suite_Weibull(Tmax, mu_opt, k_Y))
    list.X_opt = list(generate_arrivals(list.dX_opt[[n3]]))
    list.Y_opt = list(generate_departures(list.X_opt[[n3]], list.dY_opt[[n3]],Tmax))
    list.Q_opt = list(generate_queue(list.X_opt[[n3]],list.Y_opt[[n3]])[,3])
  }else{
    list.dX_opt = c(list.dX_opt, list(generate_suite_Weibull(Tmax, lambda_opt, k_X)))
    list.dY_opt = c(list.dY_opt, list(generate_suite_Weibull(Tmax, mu_opt, k_Y)))
    list.X_opt = c(list.X_opt, list(generate_arrivals(list.dX_opt[[n3]])))
    list.Y_opt = c(list.Y_opt, list(generate_departures(list.X_opt[[n3]], list.dY_opt[[n3]],Tmax)))
    list.Q_opt = c(list.Q_opt, list(generate_queue(list.X_opt[[n3]],list.Y_opt[[n3]])[,3]))
  }
}

vect.H_opt = indicator_event(list.Q_opt,l)

f0 = estimate_f(lambda0, mu0, list.dX_opt, list.dY_opt, k_X, k_Y)
f_opt = estimate_f(lambda_opt, mu_opt, list.dX_opt, list.dY_opt, k_X, k_Y)
vect.W_opt = estimate_W(f0, f_opt)

p = (1/N3) * vect.H_opt %*% vect.W_opt
p
