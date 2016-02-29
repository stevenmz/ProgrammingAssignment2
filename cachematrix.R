##' The following two functions provide a caching layer around matrix inversion
##' so that you can save computation if you need to reuse the result of a large matrix inversion
##' Example use (Hilbert code from ?solve documentation):
##' hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##' h8 <- hilbert(8); h8
##' > c <- makeCacheMatrix(h8)
##' > cacheSolve(c)
##' ...
##' > cacheSolve(c)
##' getting cached data
##' ...
##' >

##' This function takes in a matrix that you would like to perform matrix inversion on. 
##' The larger the matrix, the more benefit you will see from this function's caching of the inversion
##' result. After calling this method, use the returned vector as input to the cacheSolve method
##' to obtain the matrix inversion result. See the example use at the top of this file.
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinv <- function(invValue) invMat <<- invValue
  getinv <- function() invMat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##' This method returns the matrix inversion either through direct computation, or from a cached value
##' if the same matrix inverse has been computed before. Pass to this method the returned value from the
##' makeCacheMatrix function above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
