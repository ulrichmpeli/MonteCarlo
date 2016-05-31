generate_arrivals = function(Tmax, lambda, k_lambda=1){
  # INPUT		Tmax	    : date de fermeture de la file d'attente (plus d'entrée à partir de Tmax)
  #			    lambda	  : paramètre d'échelle de la loi de Weibull
  #			    k_lambda  : paramètre de forme de la loi de Weibull (1 par défaut)
  # OUTPUT            : vecteur (de taille aléatoire) contenant les dates d'arrivées des clients.
  
  ## Rq : Les clients arrivent entre la date t=0 et t=Tmax
  
  # Création du vecteur (sous la forme d'une liste tout d'abord)
  vect.Arrivals = list()
  
  # Arrivée du premier client
  t = rweibull(1, scale = 1/lambda, shape = k_lambda) # Attention : scale = 1/lambda et non lambda
  
  # Rajout du client dans le vecteur et arrivées des autres clients
  while(t < Tmax){
    vect.Arrivals = append(vect.Arrivals,t)                                           #rajout de t dans la liste vect.Arrivals
    deltaT = rweibull(1, scale = 1/lambda, shape = k_lambda)  #intervalle de temps avant l'arrivée du prochain client
    t = t + deltaT                                            #date d'arrivée du nouveau client
  }
  
  # Transformation en matrice de la liste
  vect.Arrivals = as.matrix(unlist(vect.Arrivals), nrow = 1, byrow = TRUE) 
  
  return(vect.Arrivals)
}