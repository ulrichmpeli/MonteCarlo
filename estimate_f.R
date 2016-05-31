estimate_f = function(lambda, mu, list.dX, list.dY, k_X, k_Y){
  # INPUT lambda  : 
  #       k_X     :
  #       mu      :
  #       k_Y     : 
  #       list.dX : liste contenant les temps inter-arrivée
  #       list.dY : liste contenant les temps inter-services
  # OUTPUT        : estimation de f(X,v) (cf article formule 11)
  
  # nombre de listes
  nb.Queues = length(list.dX)
  
  # initialisation des vecteurs contenant l'estimation de f pour chacun des queues
  vect.prod_f_lambda = vector(length = nb.Queues)
  vect.prod_f_mu = vector(length = nb.Queues)
  vect.prod = vector(length = nb.Queues)
  
  for(n in 1:nb.Queues){
    vect.prod_f_lambda[[n]] = prod(dweibull(list.dX[[n]], scale = 1/lambda, shape = k_X))
    vect.prod_f_mu[[n]] = prod(dweibull(list.dY[[n]], scale = 1/mu, shape = k_Y))
    vect.prod[[n]] = vect.prod_f_lambda[[n]] * vect.prod_f_mu[[n]]
  }
  
  return(vect.prod)
}