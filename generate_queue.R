generate_queue = function(X,Y){
  # INPUT   X : vecteur des dates d'arrivée
  #         Y : vecteur des dates de départ
  # OUTPUT  N : matrice contenant les dates cumulés et le nombre de clients dans la queue
  
  ## Cette fonction calcul le nombre de clients dans la file d'attente
  ## Attention : X est trié (les clients arrivent les uns après les autres), mais Y NE l'est PAS
  ## En effet, le client2 arrivé après le client1 peut être servi avant lui !
  
  # Création du vecteur contenant les dates (ordonnées) d'arrivée et de sorties des clients
  AllDates = sort(c(0,X,Y)) #on rajoute également la date initiale 0
  nb.Dates = length(AllDates)
  
  # Création de la matrice contenant les dates, +/- 1 (arrivée ou sortie), et le nombre de clients à chaque date
  N =  matrix(0, nrow = nb.Dates, ncol = 3)
  
  N[,1] = AllDates                                            #les dates
  N[1,2] = 0  
  N[-1,2] = 2*(AllDates[-1] %in% X)-1                             #1 si arrivée, -1 si sortie
  N[1,3] = 0
  for(d in 2:nb.Dates){
    N[d,3] = N[d-1,3] + N[d,2]          #nombre de clients à chaque date
  }
  
  return(N)
  
}
