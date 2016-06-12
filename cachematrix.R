## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to set,get the value of the matrix and set,get the value of the inverse


makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setmatrix <- function(inverse) inv <<- inverse
    getmatrix <- function() inv
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## The following function calculates the inverse of the matrix created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setmatrix(inv)
    inv
}
