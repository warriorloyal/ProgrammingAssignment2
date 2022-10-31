
## Here there are 2 functions namely, makeCacheMatrix and cachesolve functions
##makeCacheMatrix consists of set,get,setinv,getinv

makeCacheMatrix <- function(x = matrix())
  {
      i<-NULL     #initializing inverse as NULL
      set<-function(y)
      {
        x<<-y
        i<<-NULL
      }
      get<-function()x    #function to get matrix x
      setinv<-function(inverse)i<<-inverse
      getinv<-function(){
        inver<-gi(x)
        inver%*%x   #function to obtain inverse of the matrix
      }
      list(set= set,get=get,setinv=setinv,getinv=getinv)
      }      



## Write a short comment describing this function
##This function is for cache data
cacheSolve <- function(x, ...) ##gets cache data
  {
  inv<-x$getinv()
  if(!is.null(i)){                    ##checking whether inverse is null
    message("getting cached data!!")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)                      ##calculates inverse value
  x$setinv(i)
  i                            ##Return a matrix that is an inverse of'x'
}
