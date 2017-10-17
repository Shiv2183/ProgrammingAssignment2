## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setmatrix<-function(y) ## function assigns y matrix to x
  {
    x<<-y
    inv<<-NULL
  }
  getmatrix<-function() x  ## function returns matrix in cache memory
  setinverse<-function(inverse) inv<-inverse ## function assigns inverse matrix 
  getinverse<-function() inv ##function returns inverse matrix from cache memory
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## The second function calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse
  if(!is.null(inv)) ## Checks if inverse has already been calculated or not
  {
    print("getting cache data")
    return(inv) ## Returns cache memory data if calculated
  }
  matrix<-x$getmatrix 
  inverse<-solve(matrix) ##Calculates inverse of matrix after getting data from cache memory
  x$setinverse(inverse)  ##Assigns inverse matrix
  inverse
}
