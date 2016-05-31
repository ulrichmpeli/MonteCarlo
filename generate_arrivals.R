generate_arrivals = function(Tmax, lambda, k=1){
  # INPUT		Tmax	  : date de fermeture de la file d'attente (plus d'entrée à partir de Tmax)
  #			    lambda	: paramètre d'échelle de la loi de Weibull
  #			    k		    : paramètre de forme de la loi de Weibull (1 par défaut)

  ## Cette fonction génère un vecteur (de taille aléatoire) contenant les dates d'arrivées des clients.
  ## Les clients arrivent entre la date t=0 et t=Tmax
  
  # Création du vecteur sous la forme d'une liste
  X = list()
  
  # Arrivée du premier client
  t = rweibull(1, scale = 1/lambda, shape = k) # Attention : scale = 1/lambda et non lambda
  
  # Rajout du client dans le vecteur et arrivées des autres clients
  while(t < Tmax){
    X = append(X,t)                                   #rajout de t dans la liste X
    deltaT = rweibull(1, scale = 1/lambda, shape = k) #intervalle de temps avant l'arrivée du prochain client
    t = t + deltaT                                    #date d'arrivée du nouveau client
  }
  
  # Transformation en matrice de la liste
  X = as.matrix(unlist(X), nrow = 1, byrow = TRUE) 
  
  return(X)
}