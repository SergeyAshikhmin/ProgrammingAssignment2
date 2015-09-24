## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Create CacheMatrix from matrix
makeCacheMatrix <- function(x = matrix()) {
        #local invert variable
        i <- NULL
        
        #set CacheMatrix from input matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #get matrix from CacheMatrix
        get <- function()
                x
        #set inverse local variable
        setinverse <- function(inverse)
                i <<- inverse
        #get inverse local variable
        getinverse <- function()
                i
        #Functions to construct
        list(
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


##claculate inverse matrix from CacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Get inverse variable
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                #return cache result
                return(i)
        }
        #get matrix
        data <- x$get()
        #calculte inverse
        i <- solve(data, ...)
        #set inverse
        x$setinverse(i)
        #return calculated result
        i
}
