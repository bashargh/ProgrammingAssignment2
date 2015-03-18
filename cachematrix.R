## Creates a cachable matrix that remembers the matrix inverse
## The cached inverse is invalidated when the $set() function is invoked

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## A wrapper around the cached matrix.
## Check if the inverse has been computed already, 
## if not, calculate and store it using $setInv()
cacheSolve <- function(x) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}

