#makeCacheMatrix function returns a list of functions.
#Its purpose is to store a matrix and a cached inverse of a matrix.
#Contains the following functions
#setmatrix() sets the value of matrix
#getmatrix() gets the value of matrix
#setinverse() sets the value of inverse of a matrix
#getinverse() gets the cached value (inverse of a matrix)

makeCacheMatrix <- function(x = matrix()) {
	# set inverse to null if cacheSolve has not yet been used
	inv <- NULL    
	# set x to new matrix , y, and resets the matrix inverse, inv, to NULL.
	setmatrix <- function(y) {     
		x <<- y   
		inv <<- NULL   
	}
	# returns the matrix x
	getmatrix <- function() x   
	# sets the inverse, inv, to solve 
	setinverse <- function(solve) inv <<- solve 
	# returns the matrix m    
	getinverse <- function() inv    
	# returns the list containing all the functions just defined
	list(setmatrix = setmatrix, getmatrix = getmatrix,     
	setinverse = setinverse, getinverse = getinverse)
}

#The cacheSolve function computes the inverse of the special "matrix" returned by `makeCacheMatrix. 

cacheSolve <- function(x=matrix(), ...) {
	# get the cached value
	inv <- x$getinverse() 
	# if a cached value exists return it   
	if(!is.null(inv)) {     
		message("getting cached data")
		return(inv)   
	}
	# otherwise get the input matrix, calculate is inverse and store it in cache
	y <- x$getmatrix()     
	x$setmatrix(y)     
	inv <- solve(y)    
	x$setinverse(inv)     
	# return the inverse
	inv     
}

#Test the code
#mat<-matrix(data=c(4,7,6,9), nrow=2, ncol=2)
#mat2<-makeCacheMatrix(mat)
#cacheSolve(mat2)
#cacheSolve(mat2)