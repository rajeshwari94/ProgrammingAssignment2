## This function creates  a "matrix" object, that will inverse the cache
makeCacheMatrix <- function(x = matrix()){ ## matrix object has been created
   i <- NULL  ## initialise i as NULL, which will have matrix inverse 
  set <- function(y){  ## set function for assigning new 
    x <<- y   
    i <<- NULL  ## if there is any new matrix, reset to NULL
    
  }
    get <- function() ## define get function 
      x
      setinverse <-function(inverse) ## assigns value of inv in parent
        i <<- inverse  
    getinverse <- function() ## gets the value of inv where called
      i 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes  a inverse of "matrix" object, returned from the above makecachematrix
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache
  
cacheSolve <- function(i, ...) {    ## Return a matrix that is the inverse of 'x' 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

