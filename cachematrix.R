## The function here makeCacheMatrix gets a matrix as an input, sets the value of the matrix,
## get the value of the matrix, set the inverse matrix and get the inverse matrix. The matrix object
## can cache its own object.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached invertible matrix")
    return(inv)
  }
 
   matrix_data <- x$get()
  inv <- solve(matrix_data, ...)
  x$setinverse(inv)
  return(inv)
}
