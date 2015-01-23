## Put comments here that give an overall description of what your functions do
# see additional code and comments below the R code for proof that functions works

# take m1 to be a matrix which can be inverted
# the code matrix1 <- makeCacheMatrix(m1) makes a list named "matrix1"

# matrix1 is a list of lists containing four entries, each entry contains 
# a function which can be called by "cacheSolve" which check wether a solution 
# has been made. If not it is calculated, if exist then old result is returned.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # m = ?
    set <- function(y) { 
        # set(y)  input y output is direct assign x and clears m
        x <<- y
        m <<- NULL
    }    
    get <- function() x # searches for value x in other environment if needed
    setInverse <- function(Inverse) m <<- Inverse
    getInverse <- function() m
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

### code to test makeCacheMatrix and cacheSolve

# First create inversable matrixes to test  functions with
if(1==1){ # change zero to one to execute R code
    hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
    h8 <- hilbert(8); h8
    h2 <- hilbert(2); h2
    
}

# message "getting cached data"  appears and retreived matrix is identical to stored
if(1==0){ # change zero to one to execute R code
    t2 <- makeCacheMatrix(h2)
    cacheSolve(t2)
    # output
    # > cacheSolve(t2)
    # [,1] [,2]
    # [1,]    4   -6
    # [2,]   -6   12
    cacheSolve(t2)
    # output
    # > cacheSolve(t2)
    # getting cached data
    # [,1] [,2]
    # [1,]    4   -6
    # [2,]   -6   12
    
    t8 <- makeCacheMatrix(h8)
    cacheSolve(t8)
    cacheSolve(t8)
}