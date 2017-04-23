## These functions help avoid time-consuming computations of a matrix inverse (e.g. in a loop) by simply caching it the first time instead.
## The first function creates a special matrix object that helps compute and cache its inverse, while the second function
## helps to retrieve the values from the cache. 



## The following function creates a special "matrix" object that can cache its inverse.
  makeCacheMatrix <- function(x = matrix()) { ## set an argument with default mode of "matrix"
    inv <- NULL                             ## initialize inv as NULL; It will contain values of the inverse matrix 
    set <- function(y) {                    ## define a set function to assign new 
      x <<- y                             ## value of matrix in parent environment
      inv <<- NULL                        ## if there is a new matrix, then reset inv to NULL
    }
    get <- function() x                     ## define a get function, which returns values of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## required to refer 
    ## to the functions with the $ operator
  }
  



## The following function  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

