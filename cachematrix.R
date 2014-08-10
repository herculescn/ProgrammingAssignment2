## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly (there are 
## also alternatives to matrix inversion that we will not discuss here). 


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(xMatrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(yMatrix) {
    xMatrix <<- yMatrix
    inverseMatrix <<- NULL
  }
  get <- function() xMatrix
  setinverseMatrix <- function(newMatrix) inverseMatrix <<- newMatrix
  getinverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setinverseMatrix = setinverseMatrix,
       getinverseMatrix = getinverseMatrix)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverseMatrix(m)
  m
}


# > a <- matrix(12:27, nrow = 4, ncol = 4)
# > a[1,]<- 0
# > a[1,1]<- 5
# > a[2,] <- a[2,] *2
# > a[4,] <- a[4,] *3
# > a[3,] <- a[3,] -8
# > a[,3] <- a[ ,3] -18
# > mx <- makeCacheMatrix(a)
# > cacheSolve(mx)
# > a[1,1]<- 7
# > mx$set(a)
# > cacheSolve(mx)


# > x <- 3:7
# > lx<- makeVector(x)
# > cachemean(lx)
# > lx$set(5:9)
# > cachemean(lx)

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}