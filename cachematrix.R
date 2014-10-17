## Put comments here that give an overall description of what your
## functions do
	
	## Computes the inverse of a matrix and puts it into memory. 
	## In successive calls using the cache.
	
	## First we generate a square matrix to test the functions.
	## (Example taken from the "solve" function.)
	
	hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
	h8 <- hilbert(8)
	h8
	sh8 <- solve(h8)		# inverse
	round(sh8, 3)
	round(sh8 %*% h8, 3)	# Checking
	
	## To use the functions:
	
	mat <- makeCacheMatrix(h8)
	cacheSolve (mat)


## Write a short comment describing this function
#	This function creates a special "matrix" object 
#	that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		ma <- NULL
        set <- function(y) {				# set the value of the matrix
                x <<- y
                ma <<- NULL
        }
        get <- function() x					# get the value of the matrix
        setsolve <- function(solve) ma <<- solve	# set the value of inverse
        getsolve <- function() ma			# get the value of the inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function
#	This function computes the inverse of the special "matrix" 
#	returned by makeCacheMatrix above. 
#	If the inverse has already been calculated 
#	(and the matrix has not changed), then the cachesolve 
#	should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ma <- x$getsolve()
        if(!is.null(ma)) {					# Check if it already exists.
                message("getting cached data")
                return(ma)
        }
        data <- x$get()
        ma <- solve(data, ...)				# Calculate the inverse matrix.
        x$setsolve(ma)						# Save the calculation in memory.
        ma
}
