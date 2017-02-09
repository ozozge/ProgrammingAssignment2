
## Function below creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) sol <<- solve
  getInverseMatrix <- function() sol
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix =  getInverseMatrix)
}


## Function below computes/checks the inverse of the matrix returned by the function above. 
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  sol <- x$getInverseMatrix()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  matris <- x$get()
  sol <- solve(matris, ...)
  x$setInverseMatrix(sol)
  sol
  
}
