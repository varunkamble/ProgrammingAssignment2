## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        my_inverse <- NULL
        set <- function(y){
                x <<- y
                my_inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) my_inverse <<- inverse
        getinverse <- function() my_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        my_inverse <- x$getinverse()
        
        if (!is.null(my_inverse)){
                message("getting cache data")
                return(my_inverse)
        }
        
        ## Checking if it a square matrix
        #dimensions <- function(x){ if (dim(x)[1] == dim(x)[2]) a <- dim(x)[1]} 
        #my_dimensions <- dimensions(x)
        
        ## identity matrix 
        #I <- matrix(c(1,0,0,1), nrow = my_dimensions, ncol = my_dimensions)
        
        ## Check if Inverse of matrix X exits
        #if (determinant(x, logarithm = FALSE) == 0){
         #       message("determinant is zero, hence inverse doesnot exist")
          #      return(0)
        #}
        #else {
        my_matrix <- x$get()
        my_inverse <- solve(my_matrix)
        x$setinverse(my_inverse)
        #}        
        ## Return a matrix that is the inverse of 'x'
        my_inverse
}
