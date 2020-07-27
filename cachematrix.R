### Overview
## functions calculate the inverse of matrix and stores the value so the inverse of the 
## said matrix is not needed to be recomputed everytime. Upon recieving a new matrix, 
## the previous inverse is over-written with NULL, and the new inverse is calculated and stored. 

### makeCachematrix Explanation
## the function, using lexical scoping, is essentially the cache. It stores the matrix and its inverse, 
## after cacheSolve computes the inverse and stores it in the function. Moreover, everytime it is run, 
## it will replace the previous inverse matrix as NULL, ensuring cacheSolve computes a new inverse.
## Essentially, both functions are co-dependent. 

makeCacheMatrix <- function(x = matrix()) {
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


###cacheSolve Explanation
## cacheSolve first checks if there is a stored inverse in makeCacheMatrix, if there is, then it prints out the 
## stored matrix, if not, it then computes the inverse of the matrix in the first function and then stores it in 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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

