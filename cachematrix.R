## Coursera R Programming Course - Week 3 - Programming Assignment 2
## Example:
#m <- matrix(c(3, 2, 5, 2, 3, 2, 5, 2, 4), nrow = 3, ncol = 3, byrow = TRUE)
#m
#     [,1] [,2] [,3]
#[1,]    3    2    5
#[2,]    2    3    2
#[3,]    5    2    4
#inv <- makeCacheMatrix(m)
#cacheSolve(inv)
#           [,1]        [,2]       [,3]
#[1,] -0.29629630 -0.07407407  0.4074074
#[2,] -0.07407407  0.48148148 -0.1481481
#[3,]  0.40740741 -0.14814815 -0.1851852
#cacheSolve(inv)
#getting cached inverse
#            [,1]        [,2]       [,3]
#[1,] -0.29629630 -0.07407407  0.4074074
#[2,] -0.07407407  0.48148148 -0.1481481
#[3,]  0.40740741 -0.14814815 -0.1851852



## Creates a matrix that can cache it's inverse
# Args: A matrix(x)
# Returns: A matrix with functions to get/set value & get/set inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.
# Args: A matrix (x)
# Returns: inverse of the matrix

cacheSolve <- function(x, ...) {
		inv <- x$getinv()
		if (!is.null(inv)){
			message("getting cached inverse")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		return(inv)
}
