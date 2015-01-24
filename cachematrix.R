##The following functions help with "caching" the inverse of a matrix
##so that the inverse is not computed every time

## The makeCacheMatrix function takes a matrix as an input and
## returns a list that encapsulates the matrix and provides functions
## for getting (get) and setting (set) the matrix and 
## getting (getinv) and setting (setinv) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## variable for caching the inverse of the matrix
  inv <- NULL
  ## function to set the matrix
  set <- function(y){ 
    x <<- y
    inv <<- NULL
  }
  ## function to get the matrix
  get <- function() x
  ##function to set the inverse of the matrix
  setinv <- function(z = matrix()){
    inv <<- z
  }
  ##function to get the inverse of the matrix
  getinv <- function(){
    inv
  }
  ## special list with the setter and getter functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function takes the special list created by the
## makeCacheMatrix function and checks to see if the inverse is already
## calculated. If the inverse exists it is returned else it computes the
## inverse matrix and sets the inverse for future access and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  ## set the inverse for future accesses
  x$setinv(inv)
  inv
}
