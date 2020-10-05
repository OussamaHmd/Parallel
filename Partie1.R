simu_u_v_D1 = function(){
  u = runif(1)# u suit une loi uniforme
  x = 1 - sqrt(1 - u) # pour simuler la variable x
  #(si on calcule la fct de rep marginalemarginale, on inverse la fct de répartion pour simuler le u)
  y = runif(1,0,1-x) # c est notre v 
  return(c(x,y))
}

simu_u_v_Dk = function(k){
 vect = simu_u_v_D1()# vecteur (u,v) avec la 
 while (vect[1]^(1/k) + vect[2]^(1/k) > 1) {
   vect = simu_u_v_D1()
 }
 return(vect)
}

simuRejecte = function(k, n){
  array(as.numeric(unlist(replicate(n,simu_u_v_Dk(k),F))), dim=c(n, 2))
}
# transformer la liste en tableau(une liste de vecteur)

# replicate prend la fonction simu_u_v_Dk

v = simuRejecte(2, 1e4)
plot(v[,1],v[,2])

N = 10
t = numeric(N)
for (k in 1:N) {
  T1<-Sys.time()
  v = simuRejecte(k, 1e4)
  T2<-Sys.time()
  
  Tdiff= difftime(T2, T1) 
  t[k] = Tdiff
}

