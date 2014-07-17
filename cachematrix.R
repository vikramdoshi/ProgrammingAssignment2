# ---------------------------------------------------------------------------
# Matrix inversion is usually a costly computation and their may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
# The following code is a pair of functions that cache the inverse of a matrix.

# i.e. function makeCacheMatrix creates a special matrix object, and then 
# function cacheSolve calculates the inverse of the matrix. If the matrix inverse 
# has already been calculated, it will find it in the cache and return it.
# ---------------------------------------------------------------------------
# Example Usage:
# x <- matrix(rnorm(25), nrow = 5, ncol = 5)	// Create a random matrix x
# cache_x <- makeCacheMatrix(x)		// Create the special matrix as required

# cache_x$get()					// Return the matrix
# cacheSolve(cache_x)				// Return the inverse of the matrix
# cacheSolve(cache_x)				// Call the inverse fn again, therefore
# 							// the cached inverse will be returned
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# makeCacheMatrix
# ---------------------------------------------------------------------------
# This function creates a special 'matrix' object that can cached its inverse.

# Input: A square invertible matrix

# Returns: a list containing a function to:
	# 1. Set the value of the matrix
	# 2. Get the value of the matrix
	# 3. Set the value of the inverse
	# 4. Get the value of the inverse
# ---------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
	# inv
	# free variable to store the (cached) inverse matrix
	inv <- NULL

	# Setter function for the matrix
    	set <- function(y) 
	{
		# Setting the values in the the parent environment
		x <<- y
		inv <<- NULL
	}
	
	# Getter function for the matrix
	get <- function() x

	# Setter function for the inverse
	setinv <- function(inverse) 
	{
		# Setting the value of free variable in the the parent environment
		inv <<- inverse
	}

	# Getter for the inverse
	getinv <- function() inv

	# Return a list containing the above defined functions
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# cacheSolve
# ---------------------------------------------------------------------------
# This function computes the inverse of the special 'matrix' returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.

# Input: A square invertible matrix x

# Returns: the (cached) inverse 
# ---------------------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
	inv <- x$getinv()

	# If the inverse is already calculated, return the cached inverse
	if (!is.null(inv)) 
	{
		message("getting cached data")
		return(inv)
    	}

	# If the inverse is not yet calculated, we calculate it
	data <- x$get()
	inv <- solve(data, ...)
	
	# Cache the inverse for future use
	x$setinv(inv)
	
	# Return it
	inv
}
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Matrix inversion is usually a costly computation and their may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
# The following code is a pair of functions that cache the inverse of a matrix.

# i.e. function makeCacheMatrix creates a special matrix object, and then 
# function cacheSolve calculates the inverse of the matrix. If the matrix inverse 
# has already been calculated, it will find it in the cache and return it.
# ---------------------------------------------------------------------------
# Example Usage:
# x <- matrix(1:16, nrow = 4, ncol = 4)	// Create a matrix x
# cache_x <- makeCacheMatrix(x)		// Create the special matrix as required

# cache_x$get()				// Return the matrix
# cacheSolve(cache_x)			// Return the inverse of the matrix
# cacheSolve(cache_x)			// Call the 2nd time, 
# 					// the cached inverse will be returned
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# makeCacheMatrix
# ---------------------------------------------------------------------------
# This function creates a special 'matrix' object that can cached its inverse.

# Input: A square invertible matrix

# Returns: a list containing a function to:
	# 1. Set the value of the matrix
	# 2. Get the value of the matrix
	# 3. Set the value of the inverse
	# 4. Get the value of the inverse
# ---------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
	# inv
	# free variable to store the (cached) inverse matrix
	inv <- NULL

	# Setter function for the matrix
    	set <- function(y) 
	{
		# Setting the values in the the parent environment
		x <<- y
		inv <<- NULL
	}
	
	# Getter function for the matrix
	get <- function() x

	# Setter function for the inverse
	setinv <- function(inverse) 
	{
		# Setting the value of free variable in the the parent environment
		inv <<- inverse
	}

	# Getter for the inverse
	getinv <- function() inv

	# Return a list containing the above defined functions
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# cacheSolve
# ---------------------------------------------------------------------------
# This function computes the inverse of the special 'matrix' returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.

# Input: A square invertible matrix ??

# Returns: the (cached) inverse 
# ---------------------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
	inv <- x$getinv()

	# If the inverse is already calculated, return the cached inverse
	if (!is.null(inv)) 
	{
		message("getting cached data")
		return(inv)
    	}

	# If the inverse is not yet calculated, we calculate it
	data <- x$get()
	inv <- solve(data, ...)
	
	# Cache the inverse for future use
	x$setinv(inv)
	
	# Return it
	inv
}
# ---------------------------------------------------------------------------
