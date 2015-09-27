
## makeCacheMatrix contains four functions that are used to make a 
## list to cache and retieve the matrix and the inverse the matrix.
## The inverse is actually calculated in the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # makes m empty
  set <- function(y) { # set stores the matrix in the main function
    x <<- y 
    m <<- NULL
  }
  get <- function() x 
  # 'get' is a function that returns the 
  # matrix stored by the set function
  setinverse <- function(solve) m <<- solve
  #'setinverse' stores the inverse of the matrix (m) 
  # calculated from cacheSolve
  getinverse <- function() m
  #'getinverse' retrieves the inverse of the matrix (m)
  
  # list allows the 4 functions to be saved as retrievable object
  # that can be used by cacheSolve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSovle checks to see if there is already an inverse of a
## matrix and returns it if found.  Otherwise it calculates the
##inverse and stores the result

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #gets the inverse matrix, stores in m
  if(!is.null(m)) { #if inverse found, returns stored matrix
    message("getting cached data")
    return(m) #returns stored matrix and ends
  }
  data <- x$get() #gets matrix from makeCacheMatrix
  m <- solve(data, ...) #creates inverse matrix
  x$setinverse(m) #stores the inverse matrix
  m #returns the inverse matrix
}
