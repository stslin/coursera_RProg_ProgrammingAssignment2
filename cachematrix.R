## Receives a CacheMatrix data structure that contains a matrix
##    and possibily the inverse of the matrix. If the inverse of the
##    matrix is not avaliable in CacheMatrix, one will be calcuated 
##    and stored in the CacheMatrix for future use.
## Returns an inverse matrix of the R matrix type that correspond to the
##    matrix stored in the CacheMatrix data structure. If the value of the
##    inverse matrix is already stored in CacheMatrix, it will be returned
##    without furture calculation. Otherwise, the inverse value is calculated
##    with solve(), stored in the CacheMatrix data structure for future use,
##    and returned as the inverse value.

## makeCacheMatrix:
##  - Stores value of a matrix and the inverse of the matrix.
##  - Set value of matrix and the inverse of the matrix stored.
##  - Get (return) value of matrix and the inverse of the matrix stored.

makeCacheMatrix <- function(x = matrix()) {
        ## Holds value of a matrix and the inverse of the matrix.
  ix <- NULL
  set <- function(y) { ## Set value of matrix.
    x <<- y ## Set (or change) value of matrix.
    ix <<- NULL ## Clears value of inverse.
                ##    (Important when value of matrix is changed.)
  }
  get <- function() x ## Return value of matrix.
  setinverse <- function(inverse) ix <<- inverse ## Store inverse value
  getinverse <- function() ix ## Return value of invervse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve:
##  - Retreives inverse of matrix stored in the "CacheMatrix" data structure,
#         X, if one is avaliable.
##  - If inverse of matrix is not avaliable, calcuate inverse of matrix and 
##        store in the "CacheMatrix" data structure X.
##  - Returns inverse of matrix for the matrix stored in the "CacheMatrix" 
#         data structure, X.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ix <- x$getinverse()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix) ## Immediately returns cached inverse value.
  }
  data <- x$get()
  ix <- solve(data, ...) ## Computes inverse.
  x$setinverse(ix) ## Stores inverse.
  ix ## Returns inverse
}
