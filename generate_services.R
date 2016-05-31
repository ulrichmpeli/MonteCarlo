generate_services = function(X, mu, k=1){
  # INPUT	X	  : le vecteur des dates d'arrivée
  #		    mu	: paramètre d'échelle de la loi de Weibull
  #		    k	  : paramètre de forme de la loi de Weibull (1 par défaut)
  
  ## Cette fonction génére un vecteur (de la même taille que X) contenant les dates de rendu de service
  ## Cette fa�on de proc�der de donne pas une queue FIFO, mais on conjecture que cela n'a pas d'importance ici
  
  # Génération du vecteur des dates de services
  nb.Services = length(X)
  Y = X + rweibull(nb.Services, scale = 1/mu, shape = k) # Attention : scale = 1/mu et non mu
  
  return(Y)

}