indicator_event = function(list.N, l){
  # INPUT  	list.N  : liste des vecteurs du nombre de clients
  #         l       : le nombre de client maximale
  
  nb = length(list.N)
  vect.maxN = vector(length = nb)  
  for(n in 1:nb){
    vect.maxN[[n]] = max(list.N[[n]])
  }
  
  return(vect.maxN >= l)
}