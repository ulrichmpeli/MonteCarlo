generate_suite_Weibull = function(Tmax, beta, k=1){
  # INPUT		Tmax  : date de fermeture de la file d'attente
  #			    beta	: param�tre d'�chelle de la loi de Weibull
  #			    k     : param�tre de forme de la loi de Weibull (1 par d�faut)
  # OUTPUT        : vecteur (de taille al�atoire) contenant les temps inter-arriv�e/inter-service
  
  ## Rq : Les clients arrivent entre la date t=0 et t=Tmax
  
  # Cr�ation du vecteur (sous la forme d'une liste tout d'abord)
  vect.Weibull = 0
  
  # rw : suit une loi de Weibull => la variable correspond � la date du premier arriv�/service
  rw = rweibull(1, scale = 1/beta, shape = k)
  
  # time servira � faire le cumul des temps 
  time = rw
  
  i = 1
  while(time < Tmax){ #while n�cessaire car le vecteur est de taile al�atoire
    vect.Weibull[i] = rw
    rw = rweibull(1, scale = 1/beta, shape = k)
    time = time + rw
    i = i+1
  }

  return(vect.Weibull)  
}