## These functions take a maitrix, preserve its value and compute its inverse white caching it. 

## This function takes the value of the matrix and preserve its value in its own environment
## along with other variables.

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function, in conjunction with makeCacheMatrix, analize if the inverse of the input matrix
## has been solved, if not, it will get the inverse of the matrix and then cache its value.

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}







