## makeCacheMatrix() creates an R-Object returning  a list of 
## functions, which store a matrix and its invers. Using the 
## get and getinverse function stored within, the vectors can 
## be retrieved, while utulising the two set functions allows 
## the vectors to assigned new values.  

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(iv) inverse <<- iv
     getinverse <- function() inverse
     list(set = set, get = get, 
          setinverse = setinverse, getinverse =getinverse)
     
}


## The function cacheSolve() uses an argument that was previously
## returned by makeCacheMatrix(). It retrieves the data stored 
## within the function, creates its inverse, an resets it. 
## Previous to this, it checks whether an inverse has already 
## been calculated, in which case it returns the previously 
## calculated matrix instead. 

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse <- x$getinverse()
     if(is.null(inverse) == FALSE) {
          message("getting cached matrix")
          return(inverse)
     }
     matrixdata <- x$get()
     inverse <- solve(matrixdata, ...)
     x$setinverse(inverse)
     inverse
}
