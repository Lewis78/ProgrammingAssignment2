## The following two funcitons cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the invers

makeCacheMatrix <- function(x = matrix()) {
        
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        
        setsolve <- function(solve)  s <<- solve  
        getsolve <- function() s
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        

        s <- x$getsolve()
        if(!is.null(s)) {                        ##check If the inverse has already been calculated
                message("getting cached data")   ##if so, gets the inverse from the cache and skips
                return(s)                        ##the computation
        }
        data <- x$get()
        s <- solve(data, ...)       ##otherwise calculates the inverse of the data and
        x$setsolve(s)               ##sets the value of the inverse in the cache via 
        s                           ##the setsolve function
}
