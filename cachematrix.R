## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) inv <<- inverse

        getinverse <- function() inv

        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

x <- matrix(c(1, 2, 3,
              0, 1, 4,
              5, 6, 0), nrow = 3, byrow = TRUE)
y <- makeCacheMatrix(x)

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

cacheSolve(y)
