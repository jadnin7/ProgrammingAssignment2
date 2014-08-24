## This code creates a function that creates a special matrix which can cache   
## the inverse and use it if required instead of recalculating

## This function creates the special matrix and caches the solve function

makeCacheMatrix<-function(x=matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() {x}
  setinverse <- function(solve) {inv <<- solve}
  getinv <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinv = getinv)
}


## This function calls on the cached inverse, if 
## the inverse is not found, it calculates it and sets it.

cacheSolve<-function(matm) {
  inv<-matm$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat<-matm$get()
  inv<-solve(mat)
  matm$setinverse(inv)
  inv
}
