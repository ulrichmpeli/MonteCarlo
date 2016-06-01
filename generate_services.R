generate_services = function(vect.X, mu, k_Y=1, Tmax){
  # INPUT	vect.X	: le vecteur des dates d'arrivée
  #		            mu	  : paramètre d'échelle de la loi de Weibull
  #		            k_Y  : paramètre de forme de la loi de Weibull (1 par défaut)
  # OUTPUT              : un vecteur (de la même taille que vect.Arrivals) contenant les dates de rendu de service
  
  nb.X = length(vect.X)
  vect.Y = matrix(NA, nrow = nb.X)
  # Génération du vecteur des dates de services
  t = 0
  for(i in 1:nb.X){ #`
    vect.Y[i] = max(t,vect.X[i])+ rweibull(1, scale = 1/mu, shape = k_Y)
    t = vect.Y[i]
    i = i+1
  }

  return(vect.Y)

}