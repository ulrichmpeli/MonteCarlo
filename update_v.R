update_v = function(lambda0, mu0, list.N, list.X, list.Y, lambda, mu, l, N){
  # INPUT 
  # OUTPUT
  
  # préambule
  
  list.interarrivals = list()
  for(n in 1:N){
    list.interarrivals[[n]] = list.X[[n]][2:length(list.X[[n]])] - list.X[[n]][1:length(list.X[[n]])-1]
  }
  
  list.interservices = list()
  for(n in 1:N){
    list.interservices[[n]] = list.Y[[n]][1:length(list.Y[[n]])] - list.X[[n]][1:length(list.X[[n]])]
  }
  
  #calculs pour le numérateur et le dénominateur
  vect.H = indicator_event(list.N,l)
  lambda0 = lambda
  mu0 = mu
  f1 = estimate_f(lambda0, mu0, list.X, list.Y, k_X, k_Y)
  f2 = estimate_f(lambda, mu, list.X, list.Y, k_X, k_Y)
  vect.W = estimate_W(f1, f2)
  
  # calcul du somme des Ykj
  vect.sum_Y_lambda = vector(length = N)
  vect.sum_Y_mu = vector(length = N)
  
  for(n in 1:N){
    vect.sum_Y_lambda[n] = sum(list.interarrivals[[n]])   
    vect.sum_Y_mu[n] = sum(list.interservices[[n]])      
  }
    
  # calcul du somme des tauk
  vect.tau_lambda = vector(length = N)
  vect.tau_mu = vector(length = N)
  
  for(n in 1:N){
    vect.tau_lambda[n] = length(list.interarrivals[[n]])
    vect.tau_mu[n] = length(list.interservices[[n]])   
  }
  
  # calcul du numérateur
  num_lambda = (vect.H * vect.W) %*% vect.sum_Y_lambda
    
  num_mu = (vect.H * vect.W) %*% vect.sum_Y_mu
  
  denum_lambda = (vect.H * vect.W) %*% vect.tau_lambda
  
  denum_mu = (vect.H * vect.W) %*% vect.tau_mu
    
  new_v = c(num_lambda/denum_lambda, num_mu/denum_mu)  
  return(new_v)
}