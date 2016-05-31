indicator_event = function(list.Q, l0){
  # INPUT  	list.Q  : liste des vecteurs du nombre de clients
  #         l0      : le nombre de client maximal
  # OUTPUT          : un vecteur indiquant si les queues ont dépassé le seuil l0
  
  # nombre de queues
  nb.Queues = length(list.Q)
  
  # vecteur contenant le nombre maximal de clients dans les queues
  vect.maxQ = vector(length = nb.Queues)  
  for(n in 1:nb.Queues){
    vect.maxQ[[n]] = max(list.Q[[n]])
  }
  
  return(vect.maxQ >= l0)
}