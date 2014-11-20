## a function call to makeCacheMatrix will 'instantiate'
## a list with the different functions defined as in
## the example provided "Caching the mean of a vector"
## For example in a first step, we create a matrix myMatrix
## and execute the line commmand:
## myList <- makeCacheMatrix( myMatrix )
## it will 'instantiate' makeCacheMatrix in the list
## myList: so 
## calling the named parameters in myList will 
## return the definitions, for example myList$get will
## return the function definition and
## myList$get() will return the matrix itself
## 
## for the inverse
## as an example the matrix
## 1 3 3
## 1 4 3
## 1 3 4
## will return as inverse
##  7 -3 -3 
## -1  1  0
## -1  0  1

makeCacheMatrix <- function(x = matrix()) {
# A call to makeCacheMatrix as in 
# myList <- makeCacheMatrix(myMatrix)
# will set the cached value m to null
# and define the functions  
  m <- NULL
# Once instantiated a list through a call
# to makeCacheMatrix, its matrix value can
# be reassigned by calling 
# makeCacheMatrix$set(myNewMatrix)
# and that will initialize the cached value
# with m <<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve will attempt to get the cached
## value with m <- x$getinverse() and if it is not
## null will return the cached value and if not
## it will calculate it by calling the solve function
## AND it will cache the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
