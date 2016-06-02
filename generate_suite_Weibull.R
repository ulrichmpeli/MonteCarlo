generate_suite_Weibull = function(Tmax, beta, k=1){
  # INPUT		Tmax  : date de fermeture de la file d'attente
  #			    beta	: paramètre d'échelle de la loi de Weibull
  #			    k     : paramètre de forme de la loi de Weibull (1 par défaut)
  # OUTPUT        : vecteur (de taille aléatoire) contenant les temps inter-arrivée/inter-service
  
  ## Rq : Les clients arrivent entre la date t=0 et t=Tmax
  
  # Création du vecteur (sous la forme d'une liste tout d'abord)
  vect.Weibull = 0
  
  rw = rweibull(1, scale = 1/beta, shape = k)
  time = rw
  i = 1
  while(time < Tmax){ #while nécessaire car le vecteur est de taile aléatoire
    vect.Weibull[i] = rw
    rw = rweibull(1, scale = 1/beta, shape = k)
    time = time + rw
    i = i+1
  }

  return(vect.Weibull)  
}