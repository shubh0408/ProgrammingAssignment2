## caching the inverse of a matrix rather than compute it repeatedly 


## This function is used to creater a special matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix_inverse <- function(inverse) m <<- inverse
  getmatrix_inverse <- function() m
  list(set = set, get = get,
       setmatrix_inverse = setmatrix_inverse,
       getmatrix_inverse = getmatrix_inverse)
}


## This function evaluates the inverse of the matrix
# If the inverse exists, it gives the cache value of the inverse matrix rather
# than computing it again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix_inverse(m)
  m
}

#Testing the functions 

test_matrix <- makeCacheMatrix(matrix(1:4,2,2))
test_matrix$get()
test_matrix$getmatrix_inverse()

cacheSolve(test_matrix)
test_matrix$getmatrix_inverse()
  
cacheSolve(test_matrix)

