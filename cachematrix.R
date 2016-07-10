# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions cache the inverse of a matrix.

# The first function, makeCacheMatrix creates a list containing a function to:

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# The following function calculates the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the inverse from
# the cache and skips the computation. Otherwise, it calculates the inverse and 
# sets the value of the inverse in the cache via setinverse function.

# The function presumes that matrix is always invertible

cacheSolve <- function(x, ...) {
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

# Testing the functions
# > x = rbind(c(3, 5), c(2, -1))
# m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    3    5
# [2,]    2   -1
  
# First run
#> cacheSolve(m)
#            [,1]       [,2]
# [1,] 0.07692308  0.3846154
# [2,] 0.15384615 -0.2307692

# The inverse has already been calculated, so the cache solve retrieve 
# the inverse from the cache
# > cacheSolve(m)
# getting cached data.
#            [,1]       [,2]
# [1,] 0.07692308  0.3846154
# [2,] 0.15384615 -0.2307692
