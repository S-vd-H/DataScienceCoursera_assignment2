## This set of functions will provide the ability to either solve the inverse of 
# a matrix input, or extract that solution from the cache, where it has 
# previously been solved and stored.

################################################################################
## The makeCacheMatrix function will take a matrix as an input and output a 
# special 'matrix' which can contain a cached solution of the inverse 
# of the input.

makeCacheMatrix <- function(x = matrix()) {
    # Validate input
    if(length(dim(x)) != 2){
        stop("The input is not a matrix. The input should have 2 dimensions.")
    }
    else if(dim(x)[1] != dim(x)[2]){
        stop("The input matrix is not square. An inverse cannot be solved for
              rectangular matrices.")
    }
    
    # Set default for the inverse variable inv 
    inv <- NULL
    
    # The set function will set values based on the input
    set <- function(y) {
        x <<- y # Set x equal to y in the parent environment
        inv <<- NULL # Set inv equal to NULL in the parent environment and clear
        # any previous cached values
    }
    
    get <- function() x # The get function gets the value of x from the parent
    # environment (makeCacheMatrix environment)
    
    setinv <- function(inverse) inv <<- inverse # The setmean function saves 
    # the inverse in the parent (makeCacheMatrix) environment under inv 
    
    getinv <- function() inv # The getinv function gets the value of inv 
    # from the parent (makeCacheMatrix) environment 
    
    # The output is a list of the computed outputs of each function
    # and the functions themselves
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)

}

################################################################################
## The cacheSolve function will compute the inverse of the matrix input based on
# the solve function (which equates the matrix with an identity matrix and 
# solves). If the inverse for the input matrix is already computed and cached,
# it will retrieve the cached data. This will save computation time and effort.

cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse data
    inv <- x$getinv()
    # If the object is not NULL, display the cached data
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Else, get the input data and compute the inverse. Set and print it 
    # afterwards
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}