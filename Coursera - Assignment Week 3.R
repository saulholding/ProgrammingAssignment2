makeCacheMatrix <- function(x){ #Note, in contrast to the example where we needed to define the vector x as a numeric vector, here we do not need to define x as a matrix due to the fact that we create matrices with the matrix() function in the argument of the function. (this allows the $ operator to function)
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
