## These functions take a square matrix and return its inverse.  The inverse is
## calculated if it has not been previously done.  If the inverse has already been
## calculated the inverse is returned from an object saved in cache

## This function initializes the inverse to null and returns a list of 3 functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  setInv <- function(Inv) i <<- Inv
  getInv <- function() i
  list(get = get, setInv = setInv,
       getInv = getInv)
}

## This function takes the list of functions from makeCacheMatrix as an argument
## and either calculates the inverse or retrieves the inverse from cache.  The 
## inverse is returned

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
       } else {
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
       }
  i      
}


