## These codes creates and saves a set of functions to calculate and store  
## the inverse of a matrix and call on it if available.

## This function creates the special matrix and stores the inverse

makeCacheMatrix<-function(x=matrix()) {
  invr<-NULL
  set<-function(y) {
    x<<-y
    invr<<-NULL
  }
  get<-function() {x}
  setinverse <- function(solve) {invr <<- solve}
  getinv <- function() {invr}
  list(set = set, get = get,
       setinverse = setinverse,
       getinv = getinv)
}


## This function calls on the already calculated inverse, if 
## the inverse has not been calculated yet, it does it and sets it.

cacheSolve<-function(x) {
  invr<-x$getinv()
  if(!is.null(invr)) {
    message("getting cached inverse")
    return(invr)
  }
  mat<-x$get()
  invr<-solve(mat)
  x$setinverse(invr)
  invr
}
