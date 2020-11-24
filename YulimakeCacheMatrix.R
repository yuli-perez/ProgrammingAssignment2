##Assigmentv 3
makeCaheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, setInverse = setInverse, getInverse = getInverse)
}

##
cachesolve <- function(x, ...){
  inv <<- x$getInverse()
  if(!is.null(inv)){
    message("getting cahed data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$getInverse(inv)
  inv 
}
source("makeCacheMatrix.R")
###
pmatrix <- makeCaheMatrix(matrix(1:16, nrow = 4, ncol = 4))

##print the matrix
pmatrix$get()
##To calculate the inverse 
pmatrix$getInverse()

remove(cachesolve, makeCaheMatrix, pmatrix)