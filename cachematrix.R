## There are two functions - makeCacheMatrix and cacheSolve
## The first function makeCacheMatrix makes a list of commands to 
## 1) set and get the values of a matrix
## 2) set and get the inverse of a matrix
## The second function cacheSolve checks whether a matrix passed into the function exists
## 1) If already exists, retrive the inverse from the cache
## 2) If does not exist, calculate the inverse on the spot

## This function creates a list to do four functions
## 1) set the values of a matrix and store in cache
## 2) get the values of a matrix from cache
## 3) set the inverse of a matrix and store in cache
## 4) get the inverse of a matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function checks whether inverse of a matrix passed into the function exists in the cache
## If already exists, retrives inverse matrix from cache
## If does not exist, calculate the inverse for the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  i$setinverse(i)
  i
  
}
