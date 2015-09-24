## These functions will compute and cache an inverse of a matrix.
## 


## makeCacheMatrix creates a list of functions required for storing and 
## getting values for the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(matrixOne) m <<- matrixOne
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## cacheSolve checks if the inverse of matrix X has been created and stored.
## It returns the stored value if it has beem created and if not, calculates the inverse anc returns and stores it


cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setMatrix(m)
    m
}
## Return a matrix that is the inverse of 'x'
