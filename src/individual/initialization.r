getRandomRad<-function(di){
  ang<-runif(1,max=2*pi)
  return(di*c(cos(ang),sin(ang)))
}

initialize<-function(individual,D,type='radial',max=1){
  n<-individual$getNrow()
  m<-individual$getNcol()
  if(type=='random'){
    return(matrix(runif(n*m,max=max),ncol=m))
  }
  if(type=='radial'){
    i=sample(n,size=1)
    d<-as.matrix(D)[i,][-i]
    dd<-unlist(lapply(d,function(x) getRandomRad(x)))
    M=matrix(dd,ncol=m,byrow='true')
    if(i==n){
      M<-rbind(M[1:i-1,]+1,rep(1,m))
    } else if (i==1) {
      M<-rbind(rep(1,m),M[(i):(n-1),]+1)
    } else{
      M<-rbind(M[1:i-1,]+1,rep(1,m),M[(i):(n-1),]+1)
    }
    return(M)
  }
}
