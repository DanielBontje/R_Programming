## Put comments here that give an overall description of what your functions do


# see additional code and comments below the R code for proof that functions works
# also see comments inline with the code


# ----------------------------------
### Overall description of the functions makeCacheMatrix and cacheSolve do
# ----------------------------------
# take m1 to be a matrix which can be inverted
# the code matrix1 <- makeCacheMatrix(m1) makes a list named "matrix1"
# matrix1 is a list of lists containing four entries, each entry contains 
# a function which can be called by "cacheSolve" which check wether a solution 
# has been made. If no solution exist it is calculated, if solution exist then 
# previous calculated result is returned.

# ----------------------------------
# makeCacheMatrix
# ----------------------------------
# The function makeCacheMatrix creates a special "matrix" but the result of
# calling makeCacheMatrix is actually really a list with four entries:
# - first entry stores matrix m1 in the list matrix1    
# - second entry is a function to get the value of matrix m1    
# - third entry is a function which set the solution of the inversion of m1    
# - the fourth entry is function which retreives the cached inversion
#
# example:  matrix1 <- makeCacheMatrix(matrixtoinverse)

# ----------------------------------
# cacheSolve 
# ----------------------------------
## Write a short comment describing "cacheSolve"
# example:
# -    h2 is matrix to inverse
# -    t2 is structure to hold the list with four entries from makeCacheMatrix()
# -    t2 <- makeCacheMatrix(h2)
#  calling makeCacheMatrix does not solve the matrix it only prepares 
#  a storage structure, named t2 in this case, to hold a potential solution
#
# The solution of to the inversion is calculated by cacheSolve() 
# cacheSolve first checks for the existence of the solution in the fourth entry of t2
# if the solution is not stored (thus cached) the solution is calculated and stored in t2
# Thus cacheSolve modifies the storag structure if needed and assings the 
# solution to a waiting struture if called like below:
# test1 <- cacheSolve(t2)
# test1 only contains the solution of the inversion of h2 and nothing complicated



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # exists only in environment matrix1 and subfucntion
    
    # running set(y) creates/changes  x and m in the calling envirionment
    set <- function(y) { 
        # set(y):  input y, output is direct assign x and clears m
        x <<- y 
        m <<- NULL
    }    
    
    get <- function() x # returns value of x, and searches 
    # environment if needed, this envirionment is matrix1 in this example
    
    setInverse <- function(Inverse) m <<- Inverse 
    # direct assign m with solution of the inversion  
    
    getInverse <- function() m # returns value of m, and searches 
    # environment if needed, this envirionment is matrix1 in this example
    
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
    # the four entries assinged to example matrix "matrix1"
}






cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) 
        # return solution of inversion and exit function to prevent spending time 
        # on obtaining solution again
    }
    
    # retreive the matrix to inverse
    data <- x$get()
    m <- solve(data, ...) # inverse matrix
    x$setInverse(m) # store solution in 
    m # report solution to 
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
    test1 <- cacheSolve(t2); test1
    # output
    # > cacheSolve(t2)
    # [,1] [,2]
    # [1,]    4   -6
    # [2,]   -6   12
    test2 <- cacheSolve(t2); test2
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