generate_services = function(vect.Arrivals, mu, k_mu=1){
  # INPUT	vect.Arrivals	: le vecteur des dates d'arrivée
  #		            mu	  : paramètre d'échelle de la loi de Weibull
  #		            k_mu  : paramètre de forme de la loi de Weibull (1 par défaut)
  # OUTPUT              : un vecteur (de la même taille que vect.Arrivals) contenant les dates de rendu de service
  
  ## Cette façon de procéder de donne pas une queue FIFO, mais on conjecture que cela n'a pas d'importance ici
  ## S'agit-il d'une file M/M/infini ?
  
  # Génération du vecteur des dates de services
  nb.Services = length(vect.Arrivals)
  vect.Services = vect.Arrivals + rweibull(nb.Services, scale = 1/mu, shape = k_mu) # Attention : scale = 1/mu et non mu
  
  return(vect.Services)

}