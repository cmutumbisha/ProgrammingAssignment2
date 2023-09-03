## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMatrix consist of get, set, set inverse, set inverse
## It gets matrix as a input, set the value of the matrix, get the value of the matrix, set the inverse of the matrix,
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL             #set inverse as NULL
        setMatrix <- function(y){
                x <<- y
                invvmatrix <<- NUll
                }
getMatrix <- function()x                                        # function to get the value of the matrix
setInverse <- function(inverse) invmatrix <<- inverse           # function to set the value of the invertible matrix
getInverse <- function() invMatrix                              # function to get the value of the invertible matrix 
                          
                          
 list(setMatrix = setMatrix, getMatrix = getMatrix, 
      setInverse = setInverse, getInverse = getInverse) 

}
        
## Write a short comment describing this function
## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Compute the inverse of the matrix from makeCacheMatrix function
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix))
                message("getting cached matrix data!")
                return(invMatrix)
        #find the inverse value
        MatrixData <- x$getMatrix()                      
        invMatrix <- solve(MatrixData, ...)             
        x$setInverse(invMatrix)                         
        return(invMatrix)                  #return the inverse Matrix                          
}



