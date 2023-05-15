<- function(x = matrix()) {
  inverse <- NULL
 
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
 
  setInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }
  
 
  getInverse <- function() {
    inverse
  }
  
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x) {
  inverse <- x$getInverse()
  
  
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
 
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}
