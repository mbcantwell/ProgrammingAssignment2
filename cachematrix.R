q=cbind(c(1,2),c(3,8))

## This function takes x as an invertible matrix, returns a list of four operations: 
#set is a function that defines the matrix "x" as its input, which is an invertible matrix and defines inv as NULL 
#get returns x, setinverse accepts the inverse of x as its input and sets inv to that value, getinverse returns of the inverse of x. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve accepts a makeCacheMatrix as input, checks if a special matrix exists storing its inverse - if so, returns message and its inverse matrix - if not, 
#it calculates, saves to the caches and returns the inverse matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv<- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}

meg_matrix <- makeCacheMatrix(q)
cacheSolve(meg_matrix)
cacheSolve(meg_matrix)
