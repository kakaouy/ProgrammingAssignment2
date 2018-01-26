# generate a special "matrix" object that can cache its inverse
makeCacheMatrix<- function(x = matrix()) {    # default argument: matrix object
  s <- NULL                                   # set local value as NULL
  set <- function(y) {                        # set the value of the matrix
    x <<- y                                   # set value matrix in containing environment
    s <<- NULL                                # set value from solving matrix in containing environment
  }
  get <- function() x                         # get the value of the matrix
  setsolve <- function(solve) s <<- solve     # set the value of the solved matrix
  getsolve <- function() s                    # get the value of the solved matrix
  list(set = set, get = get,                  # set the objects in a list
       setsolve = setsolve,
       getsolve = getsolve)
}

# comput the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  s <- x$getsolve()                        # assign the value of the solved matrix
  if(!is.null(s)) {                      # If the inverse has already been calculated...    
    message("getting cached data")
    return(s)
  }
  data <- x$get()                          # assign the matrix we want to use
  s <- solve(data, ...)                    # invert the matrix with solve function
  x$setsolve(s)                            # set the solved matrix we have already done
  s
}

