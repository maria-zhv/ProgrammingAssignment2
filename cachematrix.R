## These functions calculate and store the inverse of a matrix



## The function makeCacheMatrix creates an R object that stores two variables: a matrix and its inverse, 
## and four functions, which can store and retreive the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## intialization of a matrix inverse: allocation memory for inv and setting its value to 0
  set <- function(y) { ## this function set the values for the matrix and its inverse in the parent environment.
    x <<- y ## set a new value for the matrix
    inv <<- NULL ##value for the inverse matrix is null because new inverse needs to be calculated for a new matrix
  }
  get <- function() x ## this function returns the matrix, which will be used later in cacheSolve to calculate inverse
  
  setinv <- function(solveinv) inv <<- solveinv ## set the value of the inverse matrix calculated with the cacheSolve function 
  getinv <- function() inv ## this function returns the stored inverse matrix
  ## 'list' returns the list that consists of the four functions made above and gives names for each function in the list  
  list(set = set, get = get, 
       setinverse = setinv,
       getinverse = getinv)
  
  
}


## Write a short comment describing this function
## this function take the object returned by the makecacheMatrix 
## and either returns the value of the inverse cached in the makeCacheMatrix environment
## or calculates the new one using functions inside the makecacheMatrix 

cacheSolve <- function(x, ...) { ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinverse() ## reading the cached values of the inverse from the makeCaheMatrix environment
  if(!is.null(inv)) { ## if chached value exists (it is not zero), 
    message("getting cached data") ## the function reports this and
    return(inv) ##  returns the cached value 
  }
  data <- x$get() ## a new matrix is read from the makeCaheMatrix environment,
  inv <- solve(data, ...) ## then its inverse is calculated
  x$setinverse(inv) ##and the new inverse is saved in the makeCaheMatrix environment
  inv ## the function returns the value of the new inverse
  
}