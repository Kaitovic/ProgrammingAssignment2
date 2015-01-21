## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions for getting and getting the matrix and functions for
## setting and getting the inversed matrix that is stored “internally”.

## It is assumed that the matrix is invertible.

## The idea is similar to the one in object-oriented programming where variables are private
## and methods are public. In this case, the matrix elements and the elements for the inverse
## matrix cannot be accessed directly. 

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL ## set inverse matrix to NULL when the matrix is changed
    message ("Matrix is chaned. Inverse set to NULL")
  }
  get <- function () x
  setinv <- function (inv) invx<<-inv
  getinv <- function () {
    if(is.null(invx)) message ("Inverse not set")
    else invx
  }
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above
## If the inverse matrix is not NULL it returns the stored inverse matrix 
## without unnecessary calculation. In the other case the inverse is calculated
## and set
## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if (!is.null(invx)) {
    message("getting cached value")
    return (invx)
  }
  else {
    message ("Computing the inverse")
    x$setinv(solve(x$get()))
    x$getinv()
  }
}
