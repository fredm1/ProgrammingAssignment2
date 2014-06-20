## See UnitTest_cachematrix_Assigment2 for unit test of cachematrix.R in github.
## The purpose of this program is to cache the inverse of a Matrix. The matrix inversion tends to be a 
## expensive computation for this reason it's a good ideal to cache the value. The makeCacheMatrix function is 
##used to create a special matrix object that is used to cache the inverse. The cacheSolve function is used to
##calculate the inverse of the matrix created by makeCacheMatrix. The last function cacheSolve was added
## to verify that the following:
## 1. The object set in makeCacheMatrix is a valid matrix
## 2. The matrix defined is a square matrix
## 3. The inverse exists for the matrix by using the det() function
## if any of the three tests conditions fail the makeCacheMatrix function will not create the 
## special matrix object.

## The makeCacheMatrix is used to create the special matrix object that caches the inverse matrix. The special matrix object is really a list that contains 4 functions and stores the given
## matrix.
## 1. First function will set the matrix
## 2. Second function will return the matrix
## 3. Third function will set the inverse matrix
## 4. Fourth function will return the inverse matrix
options(warn=1)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  print(c(length(x),class(x)))
  if(isMatrix(x)){
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
  }
}


## The function cacheSolve is used to calucate the inverse of the special matrix created in 
## makeCacheMatrix for the first time. If the special matrix does not change the function will
## return the inversed cached value 
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## The isMatrix is used to determine the matrix defined in makeCacheMatrix is a valid matrix.
## The following conditions will be tested:
##
## 1. Verify the specail matrix objected created in makeCacheMatrix is of class matrix
## 2. Verify the martrix is square matrix
## 3. Verify the matrix has a inverase matrix by check the det() is not equal to zero
##
##if any of the three tests conditions fail the makeCacheMatrix function will not create the 
## special matrix object.
isMatrix <- function(a=matrix(), ...){
  #Verify the object is of type matrix
  if (class(a) == "matrix") {
    print("It's a matrix")
    #Check if it's a square matrix
    if (nrow(a) == ncol(a)){
      print("A square matrix")
      #Determine if the inverse matrix exists
      if (det(a)==0){
        print("No inverase matrix exists for this matrix")
        return(FALSE)
      } else {
        solve(a)
      }
    }else {
      print("Not a square matrix")
      return(FALSE)
    }
  } else {
    print(c("Not a Matrix but is type of",typeof(a)),quote=FALSE)
    return(FALSE)
  }
}
