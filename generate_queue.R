generate_queue = function(X,Y){
  # INPUT   X : vecteur des dates d'arrivée
  #         Y : vecteur des dates de départ
  # OUTPUT  mat.Queue     : matrice contenant les dates cumulés et le nombre de clients dans la queue
  
  ## Cette fonction calcule le nombre de clients dans la file d'attente
  ## Attention : X est trié (les clients arrivent les uns après les autres), mais Y NE l'est PAS !! (Non FIFO)
  ## En effet, le client2 arrivé après le client1 peut être servi avant lui !
  
  # Création du vecteur contenant les dates (ordonnées) d'arrivée et de sorties des clients
  AllDates = sort(c(0,X,Y))     #on rajoute également la date initiale 0
  nb.Dates = length(AllDates)
  
  # Création de la matrice contenant les dates, +/- 1 (arrivée ou sortie), et le nombre de clients à chaque date
  mat.Queue =  as.data.frame(matrix(0, nrow = nb.Dates, ncol = 3))
  colnames(mat.Queue) = c("date", "arrival", "number")
  
  #dates dans l'ordre chronologique
  mat.Queue$date = AllDates
  
  #ensemble des arrivées et des sorties
  mat.Queue$arrival[1] = 0  
  mat.Queue$arrival[-1] = 2*(AllDates[-1] %in% X)-1 #1 si arrivée, -1 si sortie
  
  #nombre de personnes dans la queue
  mat.Queue$number[1] = 0
  for(d in 2:nb.Dates){
    mat.Queue$number[d] = mat.Queue$number[d-1] + mat.Queue$arrival[d]    #[nombre de clients à chaque date t] = [nombre de clients à la date t-1] + [+/-1]
  }
  
  return(mat.Queue)
  
}
