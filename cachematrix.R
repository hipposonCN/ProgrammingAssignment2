## The pair of functions can store a matrix and calculate the inversive version and return it to user.

## The first function can create and cache a matrix, it will return a list.

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
         x <<- y
         inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) x <<- solve
     getinverse <- function() inverse
     list(set = set, get = get,
          setinverse = setinverse, getinverse = getinverse)
}


## This function is used to identify if the original matrix has been calculated. If it has been, it will 
## return the iverse from the cache, otherwise it will calculate.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            if(x$get() != m){
                message("getting cache data")
                return(m)
            }
        }
        else{
            solve <- solve(x$get())
            x$setinverse(solve)
            solve
        }
}
