generate_arrivals = function(dX){
  # INPUT   dX  : vecteur contenant les durées inter-arrivées
  # OUTPUT      : vecteur contenant les dates d'arrivées des clients
  
  # Nombre de clients 
  nb.Arrivals = length(dX)
  
  X = 0 #temps d'arrivée des clients
  somme = 0 #indique la date du dernier arrivée
  
  for(i in 1:nb.Arrivals){
    X[i] = somme + dX[i]
    somme = X[i]
    i = i+1
  } 
  
  return(X)
}