## This source contains two functions named makeCacheMatrix and cacheSolve
## makeCacheMatrix  creates a special matrix which can cache the inverse 
### in order to get a special cache matrix you can call by makeCacheMatrix()
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

### It creates an list with 4 methods 
###  set = takes a regular matrix as a parameters and stores it
###  get = which returns the regular matrix 
###  getCachedInverseMatrix = gets the cached inverse matrix
###  setCachedInverseMatrix which sets the cached inverse matrix
### if function is called without a parameter this function creates a default one
### 
### Assumptions 
### Assumed that the matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) {
        ## Create cached inverse
        cachedInverseMatrix <- NULL 
        ##Set function
        set<-function(y){
          if(!is.null(cachedInverseMatrix)){
            ##Check if the new values are not equal then set cache with the inverse of new values
            if(dim(get())==dim(y)&&all(get() == y)){
              message("New values are equal")
            } else {
              setCachedInverseMatrix(solve(y));
            }
          } else {
            cachedInverseMatrix <- NULL             
          }
          x<<-y
        }
        ##Get data for the original matrix
        get<-function() x
        ##Get the cache from the matrix
        getCachedInverseMatrix<-function(){
          cachedInverseMatrix
        }
        ## Set the inverseMatrix to be cached
        setCachedInverseMatrix<-function(anInverseMatrix){
          cachedInverseMatrix<<-anInverseMatrix
        }
        list(set = set , 
             get = get,
             getCachedInverseMatrix = getCachedInverseMatrix, 
             setCachedInverseMatrix = setCachedInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It checks to see if the cache of an inverse matrix is available
## and if so it does not calculate the inverse of a matrix but returns the one from the cache
## if not the calculates the inverse of matrix data and sets the inverse matrix to cache and   returns inverse
cacheSolve <- function(x, ...) {
       ##Get the cachedInverseMatrix from x matrix
       inverseMatrix<-x$getCachedInverseMatrix()
       if(!is.null(inverseMatrix)){
            message("Getting cached data")
            return(inverseMatrix)
       }
       ## Get data from original matrix  
       data<-x$get()
       ##Find inverse of a data
       inverseMatrix<-solve(data)
       ##Set the cachedInverseMatrix to x
       x$setCachedInverseMatrix(inverseMatrix)
       ## Return a matrix that is the inverse of 'x'
       inverseMatrix
}
