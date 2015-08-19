## The functions makeCacheMatrix and cacheSolve work together to create a matrix 
## object that can cache the matrix inverse and return the inverse of the matrix

## makeCacheMatrix function takes a matrix as input and returns a list of  
## functions that will allow user to get matrix, set matrix to a new value,
## set inverse of a matrix, get inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## check if the input is a matrix
  if(!is.matrix(x)){
    message("Matrix is needed as input")
    return()
  }
  
  ## check if input matrix is a not a singular matrix
  ## Singular matricex cannot be inverted
  if(det(x) == 0){
    message("Singular matrix provided as input")
    message("Singular matrix cannot be inverted")
    return()
  }
  
  ## return matrix received as input
  getmatrix <- function() {x}
  
  ## takes a new matrix a input to change the current matrix
  ## resets  inverse to NULL, since it has to be recalculated for the new matrix
  setmatrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## allows users to set the inverse value of the matrix
  ## checks the input and reutrns a message if matrix is not given as input
  setinverse <- function(invmat){
    if(!is.matrix(invmat)){
      message("Matrix is needed as input")
      return()
    }    
    inv <<- invmat
  }
  
  ## returns the inverse value of the matrix
  getinverse <- function() {inv}
  
  ## create list to return the functions
  list(getmatrix = getmatrix, setmatrix = setmatrix, 
       setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated (and matrix has 
## not changed), then the function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  
  ## get the inverse matrix from the object returned ny makeCacheMatrix
  mat_inv <- x$getinverse()
  
  ## if inverse already available, return it from cache
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  
  ## calculate inverse and return it
  mat_obj <- x$getmatrix()
  invert <- solve(mat_obj)
  invert
}

