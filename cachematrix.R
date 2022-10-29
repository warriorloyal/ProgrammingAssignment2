## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Here there are 2 functions namely, makeCacheMatrix and cachesolve functions
##library(MASS) is used to calculate inverse for non squared as well as square matrices
##makeCacheMatrix consists of set,get,setinv,getinv
library(MASS)
makeCacheMatrix <- function(x = matrix())
  {
      inv<-NULL     #initializing inverse as NULL
      set<-function(y)
      {
        x<<-y
        inv<<-NULL
      }
      get<-function()x    #function to get matrix x
      setinv<-function(inverse)inv<<-inverse
      getinv<-function(){
        inver<-ginv(x)
        inver%*%x   #function to obtain inverse of the matrix
      }
      list(set= set,get=get,setinv=setinv,getinv=getinv)
      }      



## Write a short comment describing this function
##This function is for cache data
cacheSolve <- function(x, ...) ##gets cache data
  {
  inv<-x$getinv()
  if(!is.null(inv)){                    ##checking whether inverse is null
    message("getting cached data!!")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)                      ##calculates inverse value
  x$setinv(inv)
  inv                             ##Return a matrix that is an inverse of'x'
}
