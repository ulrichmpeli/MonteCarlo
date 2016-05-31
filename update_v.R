update_v = function(lambda0, mu0, list.dX, list.dY, list.Q, lambda, mu, l0){
  # INPUT   lambda0 : lambda initial
  #         mu0     : mu initial
  #         lambda  : lambda actuel
  #         mu      : mu actuel
  #         list.dX : liste contenant les temps inter-arrivée
  #         list.dY : liste contenant les temps inter-services
  #         list.Q  : liste contenant le nombre de clients dans la queue
  #         l0      : nombre de clients à ne pas dépasser   
  # OUTPUT
  
  # nombre de queues
  nb.Queues = length(list.Q)
  
  #calcul des indicatrices de dépassement de seuil
  vect.H = indicator_event(list.Q,l0)

  #calcul des pondérations W
  f1 = estimate_f(lambda0, mu0, list.dX, list.dY, k_X, k_Y)
  f2 = estimate_f(lambda, mu, list.dX, list.dY, k_X, k_Y)
  vect.W = estimate_W(f1, f2)
  
  # calcul du somme des Ykj
  vect.sum_Y_lambda = vector(length = nb.Queues)
  vect.sum_Y_mu = vector(length = nb.Queues)
  
  for(n in 1:nb.Queues){
    vect.sum_Y_lambda[n] = sum(list.dX[[n]])   
    vect.sum_Y_mu[n] = sum(list.dY[[n]])      
  }
    
  # calcul du somme des tauk
  vect.tau_lambda = vector(length = nb.Queues)
  vect.tau_mu = vector(length = nb.Queues)
  
  for(n in 1:nb.Queues){
    vect.tau_lambda[n] = length(list.dX[[n]])
    vect.tau_mu[n] = length(list.dY[[n]])   
  }
  
  # calcul du num?rateur
  num_lambda = (vect.H * vect.W) %*% vect.sum_Y_lambda
    
  num_mu = (vect.H * vect.W) %*% vect.sum_Y_mu
  
  denum_lambda = (vect.H * vect.W) %*% vect.tau_lambda
  
  denum_mu = (vect.H * vect.W) %*% vect.tau_mu
    
  new_v = c(num_lambda/denum_lambda, num_mu/denum_mu)  
  return(new_v)
}