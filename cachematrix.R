
## The two functions below are used to cache the inverse of a matrix

## The function, makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL 
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The function, cacheSolve computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should get the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data...")
                return(i)
        }
        
        data <= x$get()
        i <- solve(data) %*% data
        x$setinverse(i)
        i
}
