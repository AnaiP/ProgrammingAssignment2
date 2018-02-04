## The objetive: to write a pair of funtions that cache the inverse of a matrix
## The advantage of caching is to avoid doing unnecessary computations (assuming
## the result of those computations if the same)

## The first function, `makeCacheMatrix` returns a special auxiliary "wrapper"
## (a list with 4 functions) that handle the process of storing and fetching 
## from the cache.
## The wrapper consists in a list containing 4 auxiliary function that allow
## us to:
## 1.  store a matrix
## 2.  get the previously stored matrix
## 3.  store the inverted matrix
## 4.  get the previously inverted matrix

makeCacheMatrix <- function(m = matrix()) {
        inverse <- NULL
        
        set <- function (y){
                m <<- y
                inverse <<- NULL
        }
        
        get <- function() m
        
        setinverse <- function(y) inverse <<- y
        
        getinverse <- function() inverse

        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##Computing the inverse of a square matrix can be done with the `solve`
##function in R. For example, if `X` is a square invertible matrix, then
##`solve(X)` returns its inverse.

##This function computes the inverse of the matrix contained in the special
##"wrapper" returned by `makeCacheMatrix`. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
## it works as follows:

## 1. we try get the cached inverse (using the getinverse auxiliary function) 
## 2. if the value is not null: this means that the inverse has been stored
##     before in the cache; we return it and the execution finishes here
## 
## 3. else: this means the value is null; we get the matrix (using the get auxiliary function),
##    then we compute the inverse; then we store the inverse (using the auxiliary setinverse
##    function); and finally we return the inverse that we just computed;
## 4. the next time cacheSolve is called, the value will be fetched from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n
}

## The following commands can be used to see if a temporary matrix 
## can get its inverse by running the former functions 

## assign a matrix a that can be invertible (squared, etc.)
## invertible matrices: http://mathworld.wolfram.com/InvertibleMatrixTheorem.html
## subset 

#matrix_a <- matrix(c(1,2,2,1), nrow=2, ncol=2, byrow=TRUE)
#temp <- makeCacheMatrix(matrix_a)

## getting the cache data with the return message: "getting cached data"
#cacheSolve(temp)
#cacheSolve(temp)