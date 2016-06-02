generate_departures = function(X, dY, Tmax){
  
  nb.Arrivals = length(X)
  Y = 0
  Y[1] = X[1] + dY[1] #contient les dates de rendus de services
  
  for(i in 2:nb.Arrivals){
    Y[i] = max(X[i], Y[i-1])+ dY[i]
  }
  
  Y = Y[Y < Tmax]
  
  return(Y)
}