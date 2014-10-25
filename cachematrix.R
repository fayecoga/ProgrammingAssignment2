## A pair of functions for generating and using a cached matrix

## This function is a factory function and enclosure for
## creating a list of functions for managing a matrix
## and its inverse. The inverse function is not
## solved until it is needed when accessed through the cacheSolve
## function. If the inverse is solved, then it will be cached
## so that it can be reused without having to be resolved
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    ## change the underlying matrix object
    setMatrix <- function(y) {
        matrix <<- y
        ## disgard the old inverse if it
        ## was previously computed
        inv_matrix <<- NULL
    }
    ## return the matrix via the x variable
    ## of the enclosing scope. i.e. the closure of this environment.
    getMatrix <- function() x
    ## set the inverse matrix for the underlying matrix
    ## this should never be called on its own, but rather
    ## through the cacheSolve function.
    setInversedMatrix <- function(invertedMatrix) inv_matrix <<- invertedMatrix
    ## return the current value of the inverse matrix for the
    ## underlying matrix x
    getInversedMatrix <- function() inv_matrix
    ## return to the call a list object containing
    ## the functions for accessing the underlying matrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}


## The cacheSolve function should be used to access
## a matrix inverse for a matrix that is wrapped by
## the makeCacheMatrix function.  The cacheSolve
## function detects when the inverse needs to be solved
## and when it has already been solved and cached,
## thus minimizing the overhead of repeated calls to
## solve the inverse of a given matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInversedMatrix()
    ## if m is not null then we have already computed
    ## the inverse matrix so simply return it.
    if(!is.null(m)) {
        message("getting cached inverse of x")
        return(m)
    }
    ## m is null, so get the
    ## underlying matrix and
    ## solve its inverse.
    data <- x$getMatrix()
    m <- solve(data)
    ## now let the function list object
    ## cache the inverse matrix in its
    ## enclosing environment
    x$setInversedMatrix(m)
    m

}
