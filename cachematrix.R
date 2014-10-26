## Matrix inversion function is costly expensive in computation for this reason after the inverse matrix was found we cache that value 	
## Second time when we try to find out the inverse of that function 

## makeCacheMatrix do the following
## 1. Set the value of matrix
## 2. Get value of the matrix
## 3. Set the value of matrix inverse
## 4. Get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  ## inversa will be the inverse property
  inversa <- NULL
  ## in this below method we set the matrix
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  ## in this below method you get the matrix
  get <- function() x
  ## in this below method you set inverse of the matrix
  setinversaerse <- function(inversaerse) inversa <<- inversaerse
  ## in this below method you get the inverse of the matrix
  getinversaerse <- function() inversa
  ## return a list that we need
  list(set=set, get=get, setinversaerse=setinversaerse, getinversaerse=getinversaerse)
}	


## Write a short comment describing this function
## cacheSolve
## try to find out if this existing matrix has not already an inverse cache. if yes return that value otherwise calculate it and cache it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$getinversaerse()
   ## Return the inverse if its already in cache(or was already set)
  if(!is.null(inversa)) {
    message("getting cached data.")
    return(inversa)
  }
  ## Get the matrix from X
  data <- x$get()
  ## Calculate the inverse of data matrix
  inversa <- solve(data)
  ## Set the inverse to the object
  x$setinversaerse(inversa)
  ## Return the matrix
  inversa
}

