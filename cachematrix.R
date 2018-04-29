## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matx = matrix()) {
  matinv <- NULL
  ## set the matrix at global
  set <- function(maty){
    matx <<- maty
    matinv <<- NULL
  }
  ## get function to read the matrix
  get <-function() return(matx)
  ## set inverse value 
  setinverse <- function(maty) matinv <<- maty
  ## get inverse value
  getinverse <- function() return(matinv)
  return(list(set=set, get=get, setinverse=setinverse, getinverse=getinverse))
}


## Write a short comment describing this function

cacheSolve <- function(matX, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## call getinversion function from makeCacheMatrix
  matinv<-matX$getinverse()
  
  if(!is.null(matinv))
  {## if cached data exists
    message("getting cached data")
    return(matinv)
  }
  ## if no cached data
  data <- matX$get()
  ## calculate inverse of matrix
  matinv <-solve(data,...)
  ## call setinverse to set global value
  matX$setinverse(matinv)
  return(matinv)
}
