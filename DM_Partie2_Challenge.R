simu_X_Y = function(k){
  # k paramètre {X^{1/k} + Y^{1/k}}
  
  ######### Simulation de X par inversion de la fonctoin de reparttion ########
  u = runif(1)
  x = (1 - sqrt(1 - u))^k
  #############################################################################
  
  ######## Simulation de Y = (uniforme(1, 0, 1 - x^{1/k})^k  ###########
  y = runif(1, 0, 1 - x^{1/k})^k
  ######################################################################
  return(c(x, y))
}

simu_Gibbs = function(k, n){
  # k paramètre {X^{1/k} + Y^{1/k}}
  # n taille de la simulation
  
  array(as.numeric(unlist(replicate(n,simu_X_Y(k),F))), dim=c(n, 2))
}

############ Main ##############
v = simu_Gibbs(3, 100)
plot(v[,1],v[,2])
################################


