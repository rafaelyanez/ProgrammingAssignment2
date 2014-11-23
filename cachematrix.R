## These functions create an special matrix that caches its inverse 

## Creates the special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  
    set <- function(B) {
        x <<- B
        inv <<- NULL
    }
  
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of the matrix or returns the cached inverted matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
