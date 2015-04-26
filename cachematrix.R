## Sándor Rokob - The Second Programming Assignment


## The task was to write a pair of functions that cache the inverse of
## a matrix. The important part was the understanding of the lexical
## scoping, so we assumed that the input matrix is always invertable, and 
## the calculations came from the solve function.


## In the makeCacheMatrix function, you can see an impementation of creating this
## "special matrix" object using setter/getter method pairs ( can cache its inverse ).
## NOTE: i variable represents the inverse of the matrix  


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        mat <<- NULL
    }
    get <- function() {x}
    setInv <- function(Inv) {i <<- Inv}
    getInv <- function() {i}
    list(Set = set, get = get,
         setInv = setInv, getInv = getInv)
}


## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)){
        message("Getting cahced data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}



