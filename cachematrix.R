## Matrix inversion function is costly expensive in computation for this reason after the inverse matrix was found we cache that value 	
## Second time when we try to find out the inverse of that function 

## makeCacheMatrix do the following
## 1. Set the value of matrix
## 2. Get value of the matrix
## 3. Set the value of matrix inverse
## 4. Get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinversaerse <- function(inversaerse) inversa <<- inversaerse
  getinversaerse <- function() inversa
  list(set=set, get=get, setinversaerse=setinversaerse, getinversaerse=getinversaerse)
}	


## Write a short comment describing this function
## cacheSolve
## try to find out if this existing matrix has not already an inverse cache. if yes return that value otherwise calculate it and cache it

cacheSolve <- function(x, ...) {
  inversa <- x$getinversaerse()
  if(!is.null(inversa)) {
    message("getting cached data.")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data)
  x$setinversaerse(inversa)
  inversa
}

