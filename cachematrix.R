# Creates a special matrix (~ a list) containing a matrix m, 
# its cached inverse minv and functions to get / set their values
makeCacheMatrix <- function(m = matrix()) {
    minv <- NULL
    set <- function(new_val) {
        m <<- new_val
        minv <<- NULL
    }
    get <- function() m
    setinv <- function(inv) minv <<- inv
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Checks if the matrix's inverse has already been calculated and return it
# otherwise, calculates it and stores it in the cache via setinv function
cacheSolve <- function(m, ...) {
    minv <- m$getinv()
    if(!is.null(minv)) {
        message("getting cached matrix inverse")
        return(minv)
    }
    data <- m$get()
    minv <- solve(data)
    m$setinv(minv)
    minv
}
