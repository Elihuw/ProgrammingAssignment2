#This code follows the example quite closely with a few
#modifications for matrix math instead of taking a mean.
#The entire purpose of these functions is basically to check
#if a computation has been done previously, before attempting
#it.

#My comments will come after the line which I am describing.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     
     set <- function(y) {
          x <<- y
          #Can be used to give makeCacheMatrix a new
          #matrix if we want.
          i <<- NULL
          #The reason we set i back to NULL is so that if
          #we want to compute a new matrix inverse using a
          #new matrix, then we don't want cacheSolve below
          #picking up the old inverse, so this is like a reset
     }
     get <- function() x
     #Fairly straightforward, this part just looks up x
     #in this function's parent environment.
     setinverse <- function(inverse) i <<- inverse
     #Here is where it gets interesting, setinverse
     #takes as an argument an inverse (calculated elsewhere)
     #and passes that to i in the parent environment.
     getinverse <- function() i
     #getinverse just looks up i in the parent environment
     
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     #This is what gets returned when the function 
     #makeCacheMatrix is called. It is a list of functions.
}

#cacheSolve is intended to be used as a "wrapper function"
#for makeCacheMatrix, 
cacheSolve <- function(x, ...) {
     #The argument x is intended to be a list-vector
     #created by makeCacheMatrix.
     i <- x$getinverse()
     #Here is a new i in a new environment which is given
     #whatever the last i was in the parent environment of
     #getinverse
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     #this is basicall a double negative,
     #asking if i is not null, then returning i
     #otherwise, cacheSolve will continue
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     #this section procures the old x matrix from 
     #makeCacheMatrix and calculates its inverse,
     #then uses setinverse to cache it in the parent env.
}