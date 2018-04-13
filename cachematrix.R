## makeCacheMatrix: It creates a 'matrix' object X that can cache its inverse
## cacheSolve: It computes the inverse of X returned by makeCacheMatrix.
##             If the inverse of X has already been calculated, it is used

## Caching the inverse of a Matrix (we assume it is invertible)
makeCacheMatrix <- function(X = matrix()) {
    iX <- NULL
    setmat <- function(Y) {
     X <<- Y
     iX <<- NULL
    }
    getmat <- function() X
    setinvm <- function(solve) iX <<- solve
    getinvm <- function() iX
    list(setmat = setmat, getmat = getmat, setinvm = setinvm, getinvm = getinvm)
}

## cacheSolve: Return a matrix that is the inverse of 'X'
cacheSolve <- function(X, ...) {
    iX <- X$getinvm()
    if(!is.null(iX)) {
        message("getting cached data")
        return(iX)# it ends here
    }
    data <- X$getmat()
    iX <- solve(data, ...)
    X$setinvm(iX)
    iX
}

## To test it (you can check it deleting the #):

#A<-matrix(sample(1:16),nrow=4)
#if (det(A)!=0 ) {
#    x<-makeCacheMatrix(A)
#    cacheSolve(x)
#} else {
#    print("Non invertible matrix")
#}
#solve(A)
