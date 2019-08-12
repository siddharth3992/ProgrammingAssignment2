## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that will cache its invers

makeCacheMatrix <- function(x = matrix()) {

    inv<- NULL
    set<-function(y)
    {
      x<<-y
      inv <<- NULL
    }
    get<- function() x
    setinverse<- function(inverse) inv<<- inverse
    getinverse<-function() inv
    list ( set = set, get= get, setinverse= setinverse, getinverse = getinverse)

}


## Checks if inverse is NULL, if not then prints value stored in cache, else computes the inverse and stores it in cache

cacheSolve <- function(x, ...) {
  inv<- x$getinverse()
  if(!is.null(inv)) {
    message("getting answer from cache")
    return(inv)
  }
  mat<- x$get()
  inv<- solve(mat,...)
  x$setinverse(inv)
  inv
}
