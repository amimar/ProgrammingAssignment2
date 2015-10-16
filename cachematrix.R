#makeCacheMatrix
# create a list of functions for handling & caching
# inverse matrices

#cacheSolve
#create an inverse matrix or
#retrive it from the cache


makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
	oldmat <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
	setmat <- function(mat) oldmat <<- mat # keep mat in cache
        getmat <- function() oldmat            # retrive mat from cache   
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
	     setmat = setmat, getmat = getmat)
}


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
	mat <- x$get()
	oldmat <- x$getmat()
        if(!is.null(m) && identical(oldmat, mat)) { # check that the inverse is not NULL
                                                  # and that input matrix was not changed
                message("getting cached inverse matrix")
                return(m)
        }
        m <- solve(mat) # create an inverse matrix
        x$setinverse(m) # keep inverse mat in cache
        x$setmat(mat)   # keep input matrix in cache
        m
}


