estimate_f = function(lambda, mu, list.X, list.Y, k_X, k_Y){
  # INPUT lambda  :
  #       k_X     :
  #       mu      :
  #       k_Y     : 
  #       queue   : la queue
  # OUTPUT        : estimation de f(X,v) (cf article formule 11)
  
  list.interarrivals = list()
  for(n in 1:N){
    list.interarrivals[[n]] = list.X[[n]][2:length(list.X[[n]])] - list.X[[n]][1:length(list.X[[n]])-1]
  }
  
  list.interservices = list()
  for(n in 1:N){
    list.interservices[[n]] = list.Y[[n]][1:length(list.Y[[n]])] - list.X[[n]][1:length(list.X[[n]])]
  }
  
  nb = length(list.N)
  
  vect.prod_f_lambda = vector(length = nb)
  vect.prod_f_mu = vector(length = nb)
  vect.prod = vector(length = nb)
  for(n in 1:nb){
    vect.prod_f_lambda[[n]] = prod(dweibull(list.interarrivals[[n]], scale = 1/lambda, shape = k_X))
    vect.prod_f_mu[[n]] = prod(dweibull(list.interservices[[n]], scale = 1/mu, shape = k_Y))
    vect.prod[[n]] = vect.prod_f_lambda[[n]] * vect.prod_f_mu[[n]]
  }
  
  return(vect.prod)
}