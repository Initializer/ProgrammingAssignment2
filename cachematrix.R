## Script to cache inverse of given Matrix
## Prepared for assignment 2

## Uses
## > mat = makeCacheMatrix(matrix(1:4, 2, 2))
## > mat$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## 
## > mat$getinverse()
## NULL
##
## > cacheSolve(mat)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(mat)
## Getting cached matrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

#' Creates a spacial matrix and can cache its inverse
#' 
#' @param mat A matrix or defaulted to blank 1,1 matrix.
#' @return A special matrix object
#' @examples
#'  makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))

makeCacheMatrix <- function(mat = matrix()) {
  invMatrix <- NULL

  #' Sets Matrix to mat and invalidates cache
  #' @param matx A matrix 
  #' @return NULL
  set <- function(matx) {
    mat       <<- matx
    invMatrix <<- NULL
  }

  #' Returns current matrix
  #' @param matx A matrix 
  #' @return matrix
  get <- function() mat

  #' caches passed inverse of current matrix
  #' @param inv Inverted matrix
  setinverse <- function(inv){
     invMatrix <<- inv
  }

  #' Returns inverse of current matrix, if set
  #' @return invMatrix
  getinverse <- function() invMatrix

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Get cached inverse Matrix else Computes inverse of given matrix (if invertible), 
#' saves it and returns
#' @param A special matrix object, created by makeCacheMatrix
#' @return inverse of Matrix 

cacheSolve <- function(mat, ...) {

  matx <- mat$getinverse()

  if(!is.null(matx)) {
    message("Getting cached matrix")
    return(matx)
  }

  origMat <- mat$get()
  invMat <- solve(origMat, ...)
  mat$setinverse(invMat)
  invMat
}