## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inverse_matrix <- NULL
  
    set <- function(matrix) {
      x <<- matrix
      inverse_matrix <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inverse_matrix <<- inverse
    
    getinverse <- function() inverse_matrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}




# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),then 
# the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse() 
      
      if(!is.null(m)){
        message('getting cached data')
        return (m)  
      }
  
      data <- x$get()
      
      #Ensuring that matrix is non singular that is determinant is not 0
      if (det(data)!=0) {
        
        m <- solve(data)
        
        x$setinverse(m)
        
        m
        
      } else {
          print("Matrix is singular, does not have an inverse")
        }        
      
}

# test <- makeCacheMatrix(matrix(c(1), nrow = 2, ncol = 2))
# cacheSolve(test)
