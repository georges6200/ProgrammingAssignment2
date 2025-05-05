##These are a pair of functions that cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){
x <<- y
inv <<- NULL  ## reset cache when matrix is updated
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## This function computes the inverse of the special matrix and caches it

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data")
return(inv)
}
mat <- x$get()
inv <- solve(mat, ...) ## computes the inverse
x$setinverse(inv)      ## caches the inverse
inv
}
