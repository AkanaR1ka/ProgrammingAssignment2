## This is the second assignment for the R Programming course.
## The script includes two functions which are for caching the inverse of a matrix.
## It is assumed that the matrix being used in this script is always invertible.


## The first function is called makeChaceMatrix.
## It creates a special matrix object that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
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


## The second function is called cacheSolve.
## It firstly check whether the inverse of the matrix exists in the cache.
## If it exists, it simply retrive the inverse matrix from the cache.
## Otherwise it computes the inverse matrix and return it.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
          message("getting cached data")
          return(m)

      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m) 
      m ## Return a matrix that is the inverse of 'x'
}



