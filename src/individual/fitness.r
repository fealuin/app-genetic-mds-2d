fitness <- function(D,points,type='rawStress'){
    D1=dist(points)
    if(type=='rawStress'){
      return(sum((D-D1)^2))
    }
    else if (type=='stress') {
      return (sum((D-D1)^2)/sum(D^2))
    }
    else if (type=='mse'){
      return(sum((D-D1)^2)/(ncol(as.matrix(D))^2))
    }
    else if(type=='normalizedStress'){
      return(sqrt(sum((D-D1)^2)/sum(D^2)))
    }
    else if(type=='sStress'){
      return(sqrt(sum((D^2-D1^2)^2)/sum((D^2)^2)))
    }
    else{
      return(-1)
    }
}
