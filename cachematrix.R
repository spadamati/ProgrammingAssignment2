####################################################################
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix <- function(x = matrix()) {

##}


## Write a short comment describing this function

##cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
##}
##################################################################

## Programming Assignment 2 :  Caching the Inverse of a Matrix
## ************************** -----------------------------------

## Overall Description :
## ---------------------
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 

## The following two functions are used to cache the inverse of a matrix :

##  1. makeCacheMatrix()
##  2. cacheSolve()


## Function 1 :  makeCacheMatrix() 
## ***********   ------------------    
## 
## creates a special "matrix" object that can cache its inverse 
##  which is really a list containing a function to :

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Function 2 : cacheSolve()   
## *********** ------------- 
##
## computes and returns the inverse of the matrix created with the makeCacheMatrix function
##
## It first checks :

## (i)  If the inverse has already been computed (and the matrix has not changed).
##       If so, it gets the result and skips the computation. 
## (ii) If not, it computes the inverse, sets the value in the cache via setinverse() function.


## Assumption : This function assumes that the matrix is always invertible.
## -----------

## For eg: If X is a square invertible matrix, then solve(X) returns its inverse.
## -------

cacheSolve <- function(x, ...) { 
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## ===============================================================
## Executed the above functions with some test values 
## just to see that output was as expected per requirement.
## ===============================================================

## > x  <- matrix(c(1,2,3,4),nrow =2 ,ncol = 2))
## > a <-  makeCacheMatrix(x)

## > summary(a)
##            Length Class  Mode    
## set        1      -none- function
## get        1      -none- function
## setinverse 1      -none- function
## getinverse 1      -none- function

## > a$get()
##       [,1]  [,2]
## [1,]     1     3
## [2,]     2     4

## Run No: 1  (Note that there is no cache for the first run.)
## ---------

## > cacheSolve(a)
##       [,1] [,2]
## [1,]    -2  1.5
## [2,]     1 -0.5

## Run No: 2 (Note that cache is retrieved from the first run when you rerun.)
## ---------

## > cacheSolve(m)
## getting cached data.
##       [,1] [,2]
## [1,]    -2  1.5
## [2,]     1 -0.5

## Run No: 3 Creating a matrix Without passing arguments ie use the set() function 
## --------

## > a <- makeCacheMatrix()
## > summary(a)
##            Length Class  Mode    
## set        1      -none- function
## get        1      -none- function
## setinverse 1      -none- function
## getinverse 1      -none- function

## > a$set( matrix (c(1,2,3,4),nrow =2 ,ncol =2))
## > a$get()
##         [,1] [,2]
##    [1,]    1    3
##    [2,]    2    4

## > cacheSolve(a)
##    [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(a)
## getting cached data.
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5 