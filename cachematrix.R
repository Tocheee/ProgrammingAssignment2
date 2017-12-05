##Tocheee
##Programming Assignment 2
## These functions work together to cache the inverse of a matirx in order to save computation time.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve first checks to see if an inverse has already been calculated and returns that inverse, otherwise it computes a new inverse with the new matrix.


## makeCacheMatrix creates a matrix object that is able to cache its inverse.

makeCacheMatrix <- function (x = matrix()){
  m <- NULL
  set <- function(y){
    x <<-y       ## assigns the argument y to the object x. To set new data without re-running the entire code.
    m <<- NULL   ## clears the value of m that was previously cached 
  }
  get <-function() x    ## returns matrix 'x'
  setinverse <- function(solve) m <<- solve  ## assigns the inverse to be calculated in the second function to 'm'
  getinverse <- function() m  ## returns the inverse 'm'
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## returns a list of functions to be used for retrieving data in cacheSolve
}

## cacheSolve first checks for a saved inverse. If false, it calculates the inverse of the new matrix and saves it in 'm'

cacheSolve <- function(x,...){
  
  m <- x$getinverse() ## retrieving the matrix 'm'
  if (!is.null(m)) {
    message ("getting cached data") ## checks for a saved inverse and returns message if TRUE
  }
  data <- x$get() ## If FALSE, retrieves data from get()
  m <- solve(data) ## Calculates the inverse of the matrix in data and stores it in 'm'
  x$setinverse(m) ## Sets 'm' to the inverse matrix
  m ##print m
} 