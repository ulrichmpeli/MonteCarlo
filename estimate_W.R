estimate_W = function(lambda0, mu0, lambda, mu, list.dX, list.dY, k_X, k_Y){
  
  # nombre de listes
  nb.Queues = length(list.dX)
  
  # initialisation des vecteurs contenant l'estimation des f pour chacun des queues
  vect.prod_f0_lambda = vector(length = nb.Queues)
  vect.prod_f0_mu = vector(length = nb.Queues)
  vect.prod_f_lambda = vector(length = nb.Queues)
  vect.prod_f_mu = vector(length = nb.Queues)
  vect.prod = vector(length=nb.Queues)

  for(n in 1:nb.Queues){
    vect.prod_f0_lambda[[n]] = prod(dweibull(list.dX[[n]], scale = 1/lambda0, shape = k_X))
    vect.prod_f0_mu[[n]] = prod(dweibull(list.dY[[n]], scale = 1/mu0, shape = k_Y))
    vect.prod_f_lambda[[n]] = prod(dweibull(list.dX[[n]], scale = 1/lambda, shape = k_X))
    vect.prod_f_mu[[n]] = prod(dweibull(list.dY[[n]], scale = 1/mu, shape = k_Y))
  }
  
  for (n in 1:nb.Queues){
  vect.prod[[n]] = (vect.prod_f0_lambda[[n]]/vect.prod_f_lambda[[n]]) * (vect.prod_f0_mu[[n]]/vect.prod_f_mu[[n]])
  }
  
  return(vect.prod)
}