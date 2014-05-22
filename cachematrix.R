#Homework Assignment 2: Caching a matrix



#This function creates a matrix and caches it.
#It acts like a class object would in other 
#programming languages.  It has functions for 
#retreiving/setting the matrix contents and matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    #variable to store the matrix inverse
    mInverse <- NULL
    
    #set the matrix to whatever y is 
    #also initializes inverse variable to NULL
    set <- function(y) {
        #updates matrix
        x <<- y
        #initialize inverse to NULL
        mInverse <<- NULL
    }  
    
    #return the matrix contents
    get <- function() x
    
    #set the inverse (cache it)
    setInverse <- function(inverse) mInverse <<- inverse 
    
    #get the cached inverse
    getInverse <- function() mInverse
    
    #this is required so we can use the functions by name in the cacheSolve function
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)  
    
}


#This function retrieves the cached matrix inverse
#if it exists, if not it calculates it and caches the 
#inverse

cacheSolve <- function(x, ...) {
    #get inverse
    m <- x$getInverse()
    
    #if the inverse exists, return the cached one
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #if the matrix inverse doesn't exist then set it
    #get the matrix
    data <- x$get()
    #calculate the inverse    
    m <- solve(data)
    #set the inverse
    x$setInverse(m)
    #return the inverse
    m
}