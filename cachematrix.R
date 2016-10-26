## The complete function of this script is to: stop the script from calculating 
## the inverse of a matrix everytime by caching the inverse of that matrix in 
## memory

## makeCacheMatrix: The function creates a named list object with elements 
## set,get,setInv,getInv which are in turn functions defined inside 
## makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Checks if the inverse of the matrix is already present in cache. If not then 
## calculates the inverse, sets that in cache and return a matrix that is the 
## inverse of data

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
        
}
