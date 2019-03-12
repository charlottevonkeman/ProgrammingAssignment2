## The two functions below are able to cache the inverse of a square matrix.
## The first function, makeCacheMatrix() creates an R object that stores a 
## matrix and its inverse. The second function, cacheInverse() requires an 
## argument that is returned by makeCacheMatrix() in order to retrieve the 
## inverse from the cached value that is stored in makeCacheMatrix() object's 
## environment.


## The makeCacheMatrix() function below builds 4 functions, set(), get(), 
## setinverse(), and getinverse(), as well as two data objects, x and m. 
## It returns these functions within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Below I've used the makeCacheMatrix function on an example matrix and
## have stored the results in myMatrix.

myMatrix<-makeCacheMatrix(matrix(c(2, 4, 3, 1, 5, 7, 9, 4, 5), nrow=3, ncol=3))

## cacheSolve() populates and/or retrieves the inverse from an object of 
## type makeCacheMatrix(). This function returns a matrix that is the 
## inverse of 'x'.

cacheSolve <- function(x, ...) {
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }

## Below I've used the cacheSolve function on myMatrix

cacheSolve(myMatrix)