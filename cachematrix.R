

makeCacheMatrix <- function(x = matrix()){ 
    inv <- NULL                             
    set <- function(y) {                   
      x <<- y                             
      inv <<- NULL                        
    }
    get <- function()                   
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
    ## to the functions with the $ operator
  }
  
  

  ## setinverse function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  ## If the inverse has already been calculated (and the matrix has not changed),
  ## then cacheSolve will retrieve the inverse from the cache
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }

}
