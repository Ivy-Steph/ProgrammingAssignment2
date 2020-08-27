## The R function used are set to cache potentially time-consuming computations.

library(MASS)
makeCacheMatrix <- function(x = matrix()) {        ## makeCacheMatrix comprises of get, set, getinv and setinv
   inv <- NULL
   set <- function(y){
                      x<<-y
                      inv<<-NULL
                     }
  get<-function()x   
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x) ## This function will allow us to obtain the inverse of the matrix
                    inver%*%x
                    }
  list(set = set, get = get, 
      setinv = setinv,
      getinv = getinv)
}
## The R function is used to obtained the cached data.

cacheSolve <- function(x, ...) 
{
inv<-x$getinv()
if(!is.null(inv)){
                  message("getting cached data")       
                  return(inv)              ## Return a matrix that is the inverse of 'x'
}
data<-x$get()        
inv<-solve(data,...)                     ## Will compute the inverse
x$setinv(inv)
inv                                     ## Return a matrix that is the inverse of 'x'
}
