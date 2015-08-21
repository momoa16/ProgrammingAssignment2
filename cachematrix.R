## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix works as a cache for the matrix and its inverse. 
#   Itcreates a list of 4 items : a setter/getters for the matrix and 
#       a setter/getter for the inverse matrix
# cacheSolve either retrieves the cached inverse matrix from the list parameter or
#   calculates the inverse and stores the result in it.


## Write a short comment describing this function
# receives a matrix
# creates a setter and getter for the matrix. The setter resets the inverse matrix
# creates a setter and getter for the inverse matrix.
# returns a list with the 2 setters and 2 getters

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# receives a list as created previously
# If the getter of the inverse matrix returns a value. This value is simply returned
# Otherwise, the inverse matrix is calculated then stored in the list and finally returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
