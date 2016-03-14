## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Sets and gets values for the matrix 

makeCacheMatrix <- function(x = matrix()) {
      
      mx <- NULL
      set <- function(y) {
            x <<- y
            mx <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) mx <<- inverse
      getinv <- function() mx
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
            
      }
      
     


## Write a short comment describing this function
##1st Checks for cached matrix 
##Gets inverse of matrix, including non-square

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      mx <- x$getinv()
      if(!is.null(mx)) {
            message("getting cached data")
            return(mx)
      }
      data <- x$get()
      mx <- solve(crossprod(data, ...))
      x$setinv(mx)
      mx
}
