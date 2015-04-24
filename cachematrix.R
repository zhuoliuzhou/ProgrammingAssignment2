## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  if(!exists("m"))  m <- NULL #First check whether there are already cached variable
                              #if not, then create one with null
  set <- function(y){         #this saves the new matrix
    h <<- y
    m <<- NULL
      
  }
  get <- function() h
  setinv <- function(inv) assign("m",inv,envir = .GlobalEnv) #this caches the inverse in the global environment
  getinv <- function() m
  list(set=set,get=get,setinv = setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- makeCacheMatrix(x)$getinv()
  if(!is.null(m)){
    if(all(makeCacheMatrix()$get()==x)){ #this checks whether the matrix we want to calculate equals the one cached already
      message("getting cached data")
      return(m)
    }
  }
  makeCacheMatrix()$set(x)
  data <- x
  m <- solve(data,...)
  makeCacheMatrix(x)$setinv(m)
  m
}



