#Test pushing to github
#Update Master
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
options(warn=0)
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


## Write a short comment describing this function

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
