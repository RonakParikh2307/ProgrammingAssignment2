## The two functions below, makeCacheMatrix and cacheSolve, caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  cacheMatrix <- NULL
  set <- function(y)
  {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) cacheMatrix <<- inverse
  getInverse <- function() cacheMatrix
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  cacheMatrix <- x$getInverse()
  if(!is.null(cacheMatrix))
  {
    message("getting catched data")
    return(cacheMatrix)
  }
  data <- x$get()
  cacheMatrix <- solve(data, ...)
  x$setInverse(cacheMatrix)
  cacheMatrix
}
