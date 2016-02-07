## makeCacheMatrix caches the input matrix and it's inversed value
## the function has four usages: 
## 1. cache the input matrix
## 2. get input matrix from cache
## 3. cache the inversed value
## 4. get the inversed value from cache


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<-NULL
    }
    get <- function()
        x
    setinv <- function(inverse_value) 
        inv <<- inverse_value
    getinv <- function()
        inv
    list(
        set = set, get = get,
        setinv = setinv,
        getinv = getinv
    )
    
}


# cacheSolve reads in makeCacheMatrix with input matrix 
## and calculate the inversed value# and use makeCaheMatrix 
## to chace the caculated inversed value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    else {
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
    }
       
        
}


