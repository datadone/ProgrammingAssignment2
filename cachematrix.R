## The functions take a matrix and return the inverted matrix. 
## The answer is cached so computation time is reduced the second time the function is called with the same matrix.

## The function makeCacheMatrix stores the inverted matrix and flushes the memory when a new matrix is passed to the function.
makeCacheMatrix <- function(x = matrix()) {
		## Set initial value to NULL so m will not be lookud up in different environments
		m <- NULL
		
		## Cache function, used when a new matrix is created
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
		
		## Value of the cached matrix
		get <- function() x
		
		## Store the inverse of the Matrix
		setinverse <- function(solve) m <<- solve
		
		## Get the cached matrix
		getinverse <- function() m
		
		## Return the four functions
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## This function checks whether the solution for the given matrix is stored in memory. When stored, it will retrieve the answer. 
## If not, it will compute the inverse and pass it along to the function makeCacheMatrix to store in memory.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        ## Check if the inverse matrix is stored in the cache.
        if(!is.null(m)) {
                message("getting cached data")
                
                ## Return the cached version.
                return(m)
        }
        data <- x$get()
        
        ##Calculate the inverse of the given matrix
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
