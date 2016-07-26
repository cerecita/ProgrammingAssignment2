##  INVERSE MATRIX  dim(2,2)
## Load a vector g <- makeCacheMatrix(1:4)

makeCacheMatrix <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
           
	    x <<- y
          m <<- NULL
     }
     get <- function() matrix(byrow=TRUE,nrow=2,x)
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

##  RETURN  INVERSE MATRIX   i <- cacheSolve(g)

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
