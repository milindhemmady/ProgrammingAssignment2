## Matrix inversion is usually a costly computation. Caching the 
## inverse of a matrix can benefit rather than computing it repeatedly. 
## Following pair of functions help to improve performance where 
## repeated caclulations need to be done using matrix inverse.


## makeCacheMatrix creates a special "matrix" that allows caching of
## matrix inverse. It is a list containing functions to
##   set and get the value of the matrix
##   set and get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                im <<- inverse
        }
        getinverse <- function() {
                im
        }
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## cacheSolve calculates the inverse of the special "matrix" 
## created with the above function makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if (!is.null(im)) {
                message("getting cached inverse matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}