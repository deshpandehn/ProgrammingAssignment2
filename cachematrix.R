## The functionality below create a special object that stores a matrix and cache's its inverse
## The <<- operator is used to assign a value to an object in an environment 
## that is different from the current environment

## The fisrt function allows to initialize an object using specified matrix or
## get and set the matrix and its inverse values
## For ex. 
## r = rnorm(9)
## mat1 = matrix(r, nrow=3, ncol=3)
## testMatInvCache <- makeCacheMatrix(mat1)

## The second function allows to calculate and retrive the matrix inverse.
## If it is calculated already, the inverse will be retrived from cache.
## matInv <- cacheSolve(testMatInvCache)
## matInv <- cacheSolve(testMatInvCache)

## The function below creates a list containing functions to set or 
## get the value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Declare and initialize variable to NULL to store matrix inversion result
  xinv <- NULL
  
  # This function sets the matrix to the objected created by makeCacheMatrix function
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  # Return the input matrix
  get <- function() x
  
  # Set the inverse of the matrix (using argument inv used along with the setinv function)
  setinv <- function(inv) xinv <<- inv
  
  # Get the inverse of a matrix
  getinv <- function() xinv
  
  # Return list containing functions to set or get matrix and set or 
  # get its inverse using the objected created with the function makeCacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function allows to calculate or retrieve the calculated inverse of matrix

cacheSolve <- function(x, ...) {
  # Get the inverse of the matrix from already created object and associated function getinv()
  xi <- x$getinv()
  
  # If the inverse is already calculated, xi is not NULL. Hence, the loop 
  # below will be executed with return value of inverted matrix  
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  
  # If the inverse is not calculated already, xi will be NULL (as initiated 
  # in makeCacheMatrix), the loop above is skipped.
  # Get the matrix into variable data
  data <- x$get()
  
  # Calculate inverse of the matrix using solve() function
  xi <- solve(data)
  
  # Set and return the matrix inverse of matrix
  x$setinv(xi)
  xi
}
