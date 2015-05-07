#########################################################################
## Coursera
## Specialization: Data Science
## Course: R-Programming
## Assignment : 2
## Student: Mohsen Ghazel
## Date: May 7th, 2015
#########################################################################
## Assignment 2: Caching the Inverse of a Matrix
#########################################################################
## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). Your assignment is 
## to write a pair of functions that cache the inverse of a matrix.
## 
## Write the following functions:
## 
## 1) makeCacheMatrix: This function creates a special "matrix" 
##    object that can cache its inverse.
##
## 2) cacheSolve: This function computes the inverse of the special 
##    "matrix" returned by makeCacheMatrix above. If the inverse 
##     has already been calculated (and the matrix has not changed), 
##     then the cachesolve should retrieve the inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.
## 
## For this assignment, assume that the matrix supplied is always 
## invertible.
##########################################################################
## The following is a pair of functions that cache and compute the 
## inverse of a matrix.
##########################################################################
## This function creates a special "matrix" object
## that can cache its inverse.
##########################################################################
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
  
}

#########################################################################
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
########################################################################
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
  
}

#######################################################################
