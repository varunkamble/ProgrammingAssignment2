## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates getter and setter methods for the matrix 'x'
## and initialises NULL to the inverse matrix variable.
## It returns the list of methods defined to operate on matrix 'x'.
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

## This function is to calculate the inverse of the given matrix but it first 
## checks the cached value by calling the method 'getinverse()' defined in the
## above method 'makeCacheMatrix()' to get the cached value of the inputted
## matrix x.
## If cached value is NULL the it proceeds to calculate the inverse using the 
## 'solve()' function and then caching its result using the 'setinverse()'
## method defined in the 'makeCacheMatrix()' method.

cacheSolve <- function(x, ...) {
        
        my_inverse <- x$getinverse()
        
        if (!is.null(my_inverse)){
                message("Getting cached Data!!!")
                return(my_inverse)
        }
        
        my_matrix <- x$get()
        my_inverse <- solve(my_matrix)
        x$setinverse(my_inverse)
        
        ## Return a matrix that is the inverse of 'x'
        my_inverse
}
