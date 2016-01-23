## Put comments here that give an overall description of what your fuctions do

## Use lexical scoping and caching to reduce computation time for inverting an 
## invertible matrix repeatedly. The functions are simple modifications of the 
## "Caching the Mean" example given in the course notes.
 
## To run it the following commands may be used at R console command prompt -
## source("cachematrix.R")
## a <- makeCacheMatrix() ## stores list of functions in variable a
## a$set(B) ## stores n x n invertible matrix B as an example
## cacheSolve(a) ## calculates and prints inverse of B on 1st invocation and 
##               ## gives message "getting cached data" along with output of
##               ## cached inverse matrix on subsequent invocations

## Following invertible 4 x 4 matrix may be used to check the code

## B = matrix(c(6,4,1,1,4,6,1,1,1,1,5,2,1,1,2,5),nrow=4,ncol=4,byrow = TRUE)

## Write a short comment describing this function

## The function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse. 

makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL
	set <- function(y)
	{
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)	
}

## Write a short comment describing this function

## The function cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then  cacheSolve  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
	m <- x$getsolve()
	if(!is.null(m))
	{
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)	
      ## Return a matrix that is the inverse of 'x'
	m
}
