## makeCacheMatrix and cacheSolve are two functions used to create
## a special object that stores a matrix and caches its inverse.

## makeCacheMatrix creates a list containing 4 functions, which
## 1) set the value of a matrix
## 2) get the value of a matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

   makeCacheMatrix <- function(x = matrix()) {
   
	m <- NULL
  
  ## Define a function, set, which takes a new matrix (y) and assigns
  ## it to the variable x. Also clears the inverse m because you are
  ## using a new matrix.
  
	set <- function(y) {
    	x <<- y
    	m <<- NULL
  	}
  
  ## Define a function, get, which simply returns x (the matrix that you
  ## just set using the set function).
  
	get <- function() x
  
  ## Define a function, setinverse, which takes the variable called solve
  ## and assigns that variable to m.
  
	setinverse <- function(solve) m <<- solve
  
  ## Define a function, getinverse, which simply returns the variable m.
  
	getinverse <- function() m
  
  ## The following returns the list of functions that you just defined.
  
	list(set = set, get = get,
       		 setinverse = setinverse,
       		 getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix and then stores
## that inverse using the above makeCacheMatrix functions

   	cacheSolve <- function(x, ...) {

  ## Get current value of the inverse.
  
	m <- x$getinverse()
  
  ## If the current value of the inverse is non-empty, return the
  ## inverse and exit the function.
  
	if(!is.null(m)) {
    		message("getting cached data")
    		return(m)
  		}
  
  ## If the inverse has not been calculated, get the matrix with
  ## the get function and calculate the inverse using solve. Assign
  ## the inverse to m, and then cache the inverse using the setinverse 
  ## function.  Return m, the inverse.
  
	data <- x$get()
  	m <- solve(data, ...)
  	x$setinverse(m)
  	m
}
