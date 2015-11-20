# This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix<- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cachesolve should retrieve the inverse from the cache.
#The function assumes,that the matrix is square and invertible

cacheSolve <- function(x, ...) {
  #check if inverse has been already computed
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  #set Inverse function of R as inv for makeCacheMatrix
  x$setinverse(inv)
  #return inverse.
  inv
}