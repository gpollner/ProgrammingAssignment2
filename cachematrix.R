## These two functions allow to obtain the inverse of a matrix with
## the help of cache memory. This is useful when computing the inverse
## of a very large matrix.

## makeCacheMatrix create list of functions for getting/setting
## the matrix "x" and its inverse "i" to/from cache memory. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve first tries to get the inverse of matrix  "x" from
## cache memory, if its not there it calculates it with solve()

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <-solve(data, ...)
        x$setsolve(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

