## Put comments here that give an overall description of what your
## functions do


#' @param x A matrix
#' @return A list with the functions to set and get the value of the
#'     matrix. Also contains the functions that set and get the inverse
#'     of the matrix


makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     
     setM <- function(y = matrix()) {                      
          x <<- y*
          m <<- NULL
     }

     getM <- function() x
     
     setInverse <- function(solve) m <<- solve
     getInverse <- function() m
     
     list(setM = setM, getM = getM,
          setInverse = setInverse,
          getInverse = getInverse)  
}

#' @param x a matrix.
#' @return  inverse of the matrix. It first checks if
#    the inverse has already been computed. If so, it 
#    gets the result in the cache and returns the inverse. 
#    If not, it computes the inverse of the matrix and sets
#    thevalue in the cache.
#    This function supposes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     mat <- x$getInverse()
     
     if(!is.null(mat)) {
          message("Getting the cached data.")
          return(mat)
     }
     data <- x$getM()
    
     mat <- solve(data)
     x$setInverse(mat)
     mat
}

## Sample run:
# > x = rbind(c(1, 3), c(2, 0))
# > m = makeCacheMatrix(x)
# > m$getM()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    0

## No cache in the first run
# > cacheSolve(m)
#           [,1]       [,2]
# [1,] 0.0000000  0.5000000
# [2,] 0.3333333 -0.1666667

## Recovering from the cache in the second run
# > cacheSolve(m)
# Getting the cached data.
#           [,1]       [,2]
# [1,] 0.0000000  0.5000000
# [2,] 0.3333333 -0.1666667



