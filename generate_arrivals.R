generate_arrivals = function(dX){
  # INPUT   dX  : vecteur contenant les dur�es inter-arriv�es
  # OUTPUT      : vecteur contenant les dates d'arriv�es des clients
  
  # Nombre de clients 
  nb.Arrivals = length(dX)
  
  X = 0 #temps d'arriv�e des clients
  somme = 0 #indique la date du dernier arriv�e
  
  for(i in 1:nb.Arrivals){
    X[i] = somme + dX[i]
    somme = X[i]
    i = i+1
  } 
  
  return(X)
}