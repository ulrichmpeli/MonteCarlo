generate_departures = function(X, dY, Tmax){
  # INPUT   X   : vecteur des temps d'arrivée des clients
  #         dY  : vecteur des temps inter-services
  
  # Nombre d'arrivée de clients
  nb.Arrivals = length(X)
  
  Y = 0
  Y[1] = X[1] + dY[1] #contient les dates de rendus de services
  
  for(i in 2:nb.Arrivals){
    Y[i] = max(X[i], Y[i-1])+ dY[i] #soit le service est rendu après un autre service (si queue non vide), 
                                    #soit après l'arrivée d'un nouveau client (si queue vide)
  }
  
  Y = Y[Y < Tmax]
  
  return(Y)
}