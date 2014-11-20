## this function makes a structured object that stores both data and 
## the inverse of the data (matrix)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(z) {
    x <<- z
    m <<- NULL
  }
  get <- function() x
  
  setInv <- function(inversed) m <<- inversed
  getInv <- function() m
  list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## this function first check the getInv() of the structured object, if 
## it's not null, then return the cached inverse. else calculate the inverse
## of the given data, and stores it in the object using setInv()
cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
          message("getting cached inverse")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
