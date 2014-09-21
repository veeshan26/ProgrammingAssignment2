## Function that cache inverse of a matrix


## A special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ##We got to initialise the inverse property k
  k <- NULL
  
  ##Next the matrix
  
  set <-function(matrix){
    myMatrix <<- matrix
    k <<-NULL
    
  }
  
  ##The Method to get the matrix
  get <- function(){
    
    ## Return the matrix
    myMatrix
  }
  
  ##Now the following method sets the inverse of the matrix
  setInverse <-function(inverse){
    k <<- inverse
    
  }
  
  ##Method the get inverse of matrix
  getInverse <- function(){
    k
  }
  
  ## Now we got to return the list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  myMatrix <-x$getInverse()
  
  #if the inverse is already set, return it
  if(!is.null(myMatrix)){
    message("Cached data")
    return(myMatrix)
  }
  
  #Now we get the matrix from the object
  
  getData <- x$get()
  
  #Calculate the inverse using the matrix multiplication
  myMatrix <- solve (getData) %*% getData
  
  #Set the inverse to the object
  x$setInverse(myMatrix)
  
  ##Finally return the matrix
  myMatrix
  
}
