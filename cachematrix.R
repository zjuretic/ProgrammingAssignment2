
# INSTRUCTIONS: 
# tau <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0)
# tau <- matrix(tau, ncol=5, byrow=TRUE) 
# m <- makeCacheMatrix(tau)
# cacheSolve(m)
# cacheSolve(m)
#
# after calling cacheSolve for the second time you should see 'getting cached data' message

# INPUT: a matrix (default parameter is 0x0 matrix)
# OUTPUT: List of available functions for an object of the class makeCacheMatrix
# DESC: 'class' that encapsulates a matrix and provides the functionality for caching its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL   # variable to hold cached inverse
    
    # function to set the matrix
    # when you set new matrix cached data is no longer valid, set cache to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # function to return the matrix
    get <- function(){
        x
    }
    
    # function to assign calculated inverse to the 'placeholder' variable
    setInverse <- function(inverse){
        m <<- inverse
    }
    
    # function to return the matrix inverse from cache
    getInverse <- function(){
        m
    }
    
    # return the list of available functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# INPUT: an object of class makeCacheMatrix as input parameter
# OUTPUT: inverse of the matrix
# DESC: for a given objet of class makeCacheMatrix returns matrix inverse
# from cache if available otherwise it calculates it
cacheSolve <- function(x, ...) {
    # get inverse by calling getInverse function from object x (class makeCacheMatrix)
    m <- x$getInverse()
    
    # if the returned inverse is not null, then there is no need to calculate it
    # use cached object as the output of the function
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # returned inverse is NULL, hence we need to calculate the inverse
    # 1. Retrieve the data, our initial matrix
    # 2. Calculate its inverse by using 'solve' function
    # 3. Store calculated inverse to cache for future use
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    
    m   # return the matrix inverse
}
