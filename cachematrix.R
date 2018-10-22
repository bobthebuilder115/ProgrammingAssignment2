## This matrix invesrion function utilises caching to save costly computation. 
## It utilises 2 functions makecachematrix and cachesolve functions.


# Makecachematrix function 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of matrix
# 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) 
  {
    my_inv <- NULL
        set <- function(y) 
         {
            x <<- y
            my_inv <<- NULL
         }
    
    get <- function() x
    setinverse <- function(inverse) my_inv <<- inverse
    getinverse <- function() my_inv
    list(set = set, get = get, setmeaninverse = setinverse,getinverse = getinverse)
  
  }


## cachesolve checks if the inverse of the matrix has been cached. If yes, then it returns
# its value else calculates it.

cacheSolve <- function(x, ...) 
  {
    my_inv <- x$getinverse()
      
          if (!is.null(my_inv)) 
            {
              return(my_inv)
            }
    data <- x$get()
    my_inv <- solve(data)
    x$setinverse(my_inv)
    my_inv
  }
  
