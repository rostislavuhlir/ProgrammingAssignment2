## Functions that cache the inverse of a matrix


## A special matrix object that can cache its inverse characteristic

makeCacheMatrix <- function( m = matrix() ) {

	## Commences the inverse characteristic
    i <- NULL

    	## Sets the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    	## Gets the matrix
    get <- function() {
    	## Returns the matrix
    	m
    }

    	## Sets the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    	## Gets the inverse of the matrix
    getInverse <- function() {
      ## Returns the inverse property
        i
    }

    	## Returns a list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated and there were not any
## changes made to the matrix, then the "cachesolve" should get the inverse
## from the cache.

cacheSolve <- function(x, ...) {

    	## Returns a matrix that is the inverse of 'x'
    m <- x$getInverse()

    	## Returns the inverse if already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    	## Gets the matrix from our object
    data <- x$get()

    	## Calculates the inverse through matrix multiplication
    m <- solve(data) %*% data

    	## Sets the inverse to the object
    x$setInverse(m)

    	## Returns the defined matrix
    m
}
