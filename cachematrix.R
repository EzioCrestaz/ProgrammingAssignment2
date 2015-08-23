## Author:   E. Crestaz
## Location: Fano (PU), Italy
## Date:     2015-08-19
## Contact:  ezio.crestaz@giscience.it
##
## Scope:    
## Address the computational intensive task of matrix inversion by creation
## of a closure mantaining both matrix and its inverse, once calculated, so
## that inverse matrix recalculation can be avoided.
##
## Description:
## Creation of a couple of functions, which can be used in combination to 
## manage 'extended matrix' object, storing a matrix and its inverse in cache,
## and to compute matrix inverse or return it from cache (if already computed) 
## Refer to https://en.wikipedia.org/wiki/Invertible_matrix for a refresher 
## of matrix inversion and identity matrix concepts.
##
## Notes:
## No check is currently performed on original matrix, which must be square
## and invertible.

## Create a special 'extended matrix' object, storing in cache both matrix
## and its inverse (if computed, otherwise set to NULL).
##
## Args:
##   x: a matrix
## Returns:
##   A list of functions to set and get matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                      # Inverse matrix not computed
  set <- function(y) {             # Set a new matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x              # Get matrix
  setInv <- function(i) inv <<- i  # Set matrix inverse
  getInv <- function() inv         # Get matrix inverse
  list(set = set,                  # Return created functions
       get = get,                  # to working environment
       setInv = setInv,
       getInv = getInv)
}

## Compute or return (from cache, if available) the inverse of 
## the special matrix object passed in as an argument
##
## Args:
##   x: a makeCacheMatrix object, storing a matrix
## Returns:
##   The inverse of the matrix stored to arg 'x' object
## Notes:
##   The function does not check that matrix is square nor it is
##   invertible
cacheSolve <- function(x, ...) {
  inv <- x$getInv()                # Access matrix inverse (if calculated)
  if(!is.null(inv)) {              # Return cached inverse (if calculated)
    message("Getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()                  # Access original matrix
  inv <- solve(data)               # Computed matrix inverse
  x$setInv(inv)                    # Set matrix inverse in argument object
  inv                              # Returns inverse matrix
}

## Check above functions working by checking Identity matrix resulting from
## multiplication of matrix and its inverse
m <- matrix(c(2,6,3,1),2,2)
cm <- makeCacheMatrix(m)
inv <- cacheSolve(cm)
I <- m %*% inv   # Matrix multiplication should result in the Identity matrix
print(I)

m <- matrix(runif(1000000,0,100),1000,1000)
head(m)
cm <- makeCacheMatrix(m)
inv <- cacheSolve(cm)
I <- m %*% inv   # Matrix multiplication should result in the Identity matrix
print(head(I))
