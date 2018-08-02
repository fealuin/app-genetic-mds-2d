fitness <- function(D,points,type='rawStress'){
    DD=dist(points)
    if(type=='rawStress'){
      return(c(sum((D[[1]]-DD)^2),sum((D[[2]]-DD)^2)))
    }
    else if (type=='stress') {
      return (c(sum((D[[1]]-DD)^2)/sum(D^2),sum((D[[2]]-DD)^2)/sum(D^2)))
    }
    else if (type=='mse'){
      return(c(sum((D[[1]]-DD)^2)/(ncol(as.matrix(D[[1]]))^2),sum((D[[2]]-DD)^2)/(ncol(as.matrix(D[[2]]))^2)))
    }
    else if(type=='normalizedStress'){
      return(c(sqrt(sum((D[[1]]-DD)^2)/sum(D[[1]]^2)),sqrt(sum((D[[2]]-DD)^2)/sum(D[[2]]^2))))
    }
    else if(type=='sStress'){
      return(c(sqrt(sum((D[[1]]^2-DD^2)^2)/sum((D[[1]]^2)^2)),sqrt(sum((D[[2]]^2-DD^2)^2)/sum((D[[2]]^2)^2))))
    }
    else{
      return(-1)
    }
}
