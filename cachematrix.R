##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  ##no values initially so set to null
  set<-function(matrix) {
          x <<- matrix
          ##remove the value of matrix since assigned a new value  
          inv <<- NULL
  }
    get = function() x  ##gets stored matrix
    setInverse = function(inverse) inv <<- inverse
    getInverse = function() inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}  ##returns a list

##This function computes the inverse of the special "matrix" returned 
##by `makeCacheMatrix` above. If the inverse has already been calculated
##(and the matrix has not changed), then `cacheSolve` should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv = x$getInverse()  ##get cached value if it exists
      if ( !is.null(inv) ) {
                message( "retrieving cached data")
                return(inv)
      }   ##if cache is empty, display the message, calculate the inverse and store in cache 
      matx <- x$get()
      inv <- solve(matx, ...)
      x$setInverse(inv)
      inv 
} 
