
# makeCacheMatrix is the first of the two functions deployed ir order to cache the inverse of a matrix to avoid computing it 
# more than once if avoidable. makeCacheMatrix creates a list containing a function to:
# 1. Set the value of the matrix.
# 2. Get the value of the matrix.
# 3. Set the value of the inverse matrix.
# 4. Get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        value <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invm <<- solve
        getinverse <- function() invm
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve returns the inverse of the matrix. There are two possibilities, a) if the matrix has not changed
## and its inverse has already been computed it will return, it will returned the "cached" result without any 
## further computation. Otherwise it will calculate the inverse and set its value via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinverse(invm)
        invm
}
