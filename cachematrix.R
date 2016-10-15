## This function consists of 2 functions makeCacheMatrix() and 
## cacheSolve(), which take a matrix as input and caches the 
## inverse of the matrix, assuming that the matrix supplied is 
## always square

## makeCacheMatrix takes a matrix as input and stores it in the cache
## as a list, it consists of four nested functions set(), get(),
## setsolve() and getsolve()
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                     ## initializing object s and m
  set <- function(y){
    x <<- y                     ## assigns the value of y to the x object in the parent environment
    s <<- NULL                  ## assigns the value of NULL to the s object in the parent environment
  }
  get <- function() x           ## retrieves the value of x from the parent environment
  setsolve <- function(solve) s <<- solve ## sets the value of s after the setsolve function runs
  getsolve <- function() s
  list(set = set, get = get,    ## list is created with each function named appropriately
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve returns the inverse of the matrix from the cache
## calculated in the previous function or if the data changes, calculates
## the new inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)){                 ## if the value of s is not null,
    message("Getting cached data") ## returns the value of s from the cache
    return(s)
  }
  data <- x$get()                  ## If s is null then it calculates the inverse
  s <- solve(data, ...)            ## by referencing the makeCacheMatrix function
  x$setsolve(s)
  s
}
