## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    get <- function() {    
	m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }
## Return a list of the methods    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
#Compute the inverse of the special matrix returned by "makeCacheMatrix"
cacheSolve <- function(x,y) {
    m <- x$getInverse()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
## Calculate the inverse using matrix multiplication    
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}