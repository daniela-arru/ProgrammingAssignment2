#makeCacheMatrix is a set of functions to 
# $set: store data, i.e. a matrix, in the cache
# $get: retrive data, i.e. a matrix, from the cache
# $setinvert: compute and store the invert of the matrix
# $getinvert: retrieve the invert of a matrix from the cache

makeCacheMatrix <- function(x=numeric()) {
    solve_invert <- NULL
    set <- function(y) {
        x <<- y
        solve_invert <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) solve_invert <<- solve
    getinvert <- function() solve_invert
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)  
}

# cacheSolve is a function that search in the cache for the resolved 
# matrix of x. If it finds the value, the function retrives it, else computes it and 
# stores it in the chache for future computation

cacheSolve <- function(x, ...) {
  
    solve_invert<-x$getinvert()
   
     if(!is.null(solve_invert)) {
        message ("getting cached data")
        return(solve_invert)
    }
    data<-x$get()
    solve_invert<-solve(data)
    x$setinvert(solve_invert)
    solve_invert
    
}