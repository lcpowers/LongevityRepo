lifespan <- function(nx,Ninit,percentile){
  nclasses=dim(nx)[1]
  vec=c(Ninit,rep(0,(nclasses-1)))
  nx[1,]=0
  jj=1
  while (sum(vec)>Ninit*percentile){
    vec=nx%*%vec
    jj=jj+1
    #print(sum(vec))
  }
  rm(nclasses,vec,nx)
  return(jj)
}
