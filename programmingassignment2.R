library(MASS)                      ##it is used to calculate inverse for non-squared and squared matrices
makecachematrix=function(x=matrix()){
  inv=NULL
  set=function(y){
    x<<-y
    inv<<-NULL
  }
  get=function(){x}                ##function to get a matrix
  setinverse=function(inverse){inv=inverse}
  getinverse=function(){
    inver=ginv(x)
    inver%*%x
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
cachesolve=function(x,...){
  inv=x$getinverse()
  if(!is.null(inv)){               ##checking weather inverse is null
    message("getting cached data")
    return(inv)
  }
  
  value=x$get()
  inv=solve(value,...)             ##calculates inverse value
  x$setinverse(inv)
  inv                              ##returns a x inverse matrix 
} 
p=makecachematrix(matrix(1:8,4,2))
p
p$get()
p$getinverse()
cachesolve(p)

