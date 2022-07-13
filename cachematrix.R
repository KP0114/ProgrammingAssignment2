#First function will compute inverse of matrix passed as an input
makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b){
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Second function function will check if inverse was previously cached :
#If so, already solved inverse will be output
#Otherwise,setInverse is called and inv is solved
cacheSolve <- function(a, ...) {
  inv <- a$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data)
  a$setInverse(inv)
  inv      
}
