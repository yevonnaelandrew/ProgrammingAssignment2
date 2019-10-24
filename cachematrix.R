## This function is used to calculate the inverse of a matrix
## The result will be saved as a cache
## If the calculation is made again, the function will look at the cache
## If valid cache is exist, it will just return the result from the cache
## If valid cache isn't exist, the function will calculate it

## More info about this assignment and Lexical Scoping
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## This function can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Initializing as an object to be later used
  set <- function(y) {
    x <<- y ## Assign an input argument to the x object in the parent env
    ## This line of code below clears any value of m 
    ## that had been cached by a prior execution of cacheSolve().
    m <<- NULL ## Assign NULL to m in the parent env
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, ## Gives the name 'set' to the set() function defined above
       get = get, ## Gives the name 'get' to the get() function defined above
       setsolve = setsolve, ## Gives the name 'setmean' to the setmean() function defined above
       getsolve = getsolve) ## Gives the name 'getmean' to the getmean() function defined above
}


## Compute the cache of the matrix, or retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  ## Since makeCacheMatrix() sets the cached to NULL if new matrix is assigned
  ## So if the value is not equal to NULL, we have a valid cache
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
