## Below are two functions that create an invertible matrix and cache the 
## inverse of the matric

## makeCacheMatrix: a list containing a function to
## - set the value of the vector
## - get the value of the vector
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) 
        {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) c <<- inverse
        getinverse <- function() c
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns the inverse of the matrix. However, it checks to 
## see if the inverse has already been computed. If it has, it gets the 
## results and does not do the calculation. If it hasn't, it sets the value
## in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
        c <- x$getinverse()
        if(!is.null(c))
        {
                message("Getting cache data")
                return(c)
        }
        data <- x$get()
        c <- solve(data)
        x$setinverse(c)
        c
}


