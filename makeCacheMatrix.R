
makeCacheMatrix <- function(x = matrix()) {  ##define the argument with model matrix
        i <- NULL  ##the value of matrix inverse
        set <- function(y) { ##assign a new variable with function(y)
                x <<- y  ##value of matrix
                i <<- NULL ##if it is a new matrix, reset value to NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {  ## This function computes the inverse of the special “matrix” returned by makeCacheMatrix.If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}