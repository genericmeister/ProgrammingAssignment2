## The following functions calculate the inverse of a Matrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the previously calculated inverse matrix is used and returned 
## instead of redoing the calculation
##    
##    1.makeCacheMatrix: This function creates a special "matrix" object 
##                            that can cache its inverse.
##    2.cacheSolve: This function computes the inverse of the special "matrix" 
##                                returned by makeCacheMatrix above. 


## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        # m is the cache
        m <- NULL
        set <- function(y) {
                x <<- y # assign input matrix to variable x in parent environment. 
                m <<- NULL # since Matrix has changed, reset the cache to NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m
        list(   set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## The following function creates the inverse of the matrix created thru 
## makeCacheMatrix function. Before the inverse is calculated the function checks
## if the inverse has already been calculated before. If it has been for the same 
## matrix then it "gets" the inverse from the cache. If not, it "sets" the inverse
## via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m<-x$getinverse()
        if (!is.null(m))
        {
                message("getting cached data")
                return (m)
        }
        
        matrix<-x$get() 
        m<-solve(matrix,...)
        x$setinverse(m)
        
        return(m)
}


