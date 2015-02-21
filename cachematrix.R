## These below two functions are built to use lexical scoping and caching functions
## as a means for reducing computation time for solving for a variable.

## This is a comment I added to see the changes in my Git Repo

## This function "makeCacheMatrix" stores a cached version of a inverse matrix
## It does the following in order:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of matrix created with the above function 
## it first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the inverse matrix in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        m
        
}

## tests:
## > x <- matrix(11:14,2,2)
## > m<- makeCacheMatrix (x)
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -7  6.5
## [2,]    6 -5.5
## > 