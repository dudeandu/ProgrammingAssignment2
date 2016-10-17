## this functions allow the user to calculate the inverse of a matrix and 
## assign it to a cache file, so that there is no need to calculate it more 
## than once

## This function creates a list of functions that alow the function cacheSolve
## to compute the inverse of a matrix, and store in a different environment
makeCacheMatrix <- function(mat = matrix()) {
      ## check if x is a matrix
      if(class(mat) != "matrix"){ 
            ## If mat is not a matrix, the function stops, and asks the user for 
            ## a matrix
            stop("x is not a matrix, please give a matrix")
      }
      
      ## intitializes the a vector that will conain the matrix inverse
      mat_inv <- NULL
      
      ## set the values of the matrix
      set <- function(){
            mat <<- y
            mat_inv <<- NULL ##if the matrix has changed, it reasigns it to NULL
      }
      ## gets the value of the matrix
      get <- function() mat
      
      ## sets the inverse of the matrix
      set.inv <- function(solve) mat_inv <<- solve
      
      ## gets the value of the inversed matrix
      get.inv <- function() mat_inv
      
      ## create the list of functions to be returned with the names of each function
      list(
            set = set,
            get = get,
            set.inv = set.inv,
            get.inv = get.inv
      )
}


## The function will check if the inverse has already been calculated 
## and the matrix has not changed. If the inverse has not been calculated, 
## the function will use a list of files created by makeCacheMatrix, and create
## a cached inverse of a matrix. If the inverse has been calculated, and hasn't 
## changed, cacheSolve will simply advise the user, and return the cached inverse
cacheSolve <- function(mat, ...) {
      ## create a vector to check if the cache invese has been created
      invers <- mat$get.inv
      ## first check if the inverse of the matrix has been calculated 
      ## and cached already
      if(!is.null(invers)) {
            message("getting cached data")
            return(invers)
      }
      ## this next sequence of lines creates a cached inverse of the matrix,
      ## and returns it after computing the inverse
      
      ## asign the contents of mat to a vector called 'dat'
      dat <- mat$get()
      ## computes the inverse of the matrix
      invers <- solve(dat)
      ## assigns the inverse to the cache
      mat$set.inv(invers)
      ## returning the inverse
      invers
}
