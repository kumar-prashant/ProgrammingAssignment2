## R Programming : Assignment 2

## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.
## Returns : A list containing functions


makeCacheMatrix <- function(x = matrix()) { 
    # set m to NULL
    m<-NULL
    
    # takes a matrix as an argument, setting x to this matrix 
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    # takes no arguments and returns the initial matrix
    get<-function() x
    
    # takes matrix as an argument and sets to this matrix 
    setmatrix<-function(solve) m<<- solve
    
    # takes no arguments and returns the inverted matrix stored in the variable 
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## cacheSolve : This function computes the inverse of the special "matrix" 
#               returned by makeCacheMatrix above. If the inverse has already been 
#               calculated (and the matrix has not changed), then the cachesolve 
#               should retrieve the inverse from the cache.

## Returns : Either the cached inverse matrix (if it is saved) or calculates the inverse and 
#           stores it in the makeCacheMatrix object 

cacheSolve <- function(x, ...) {
    # obtain matrix (if none, returns NULL)
    m<-x$getmatrix()
    
    # test to see if matrix is not NULL
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    #if no matrix stored, sets the value of in the cache via the setmatrix function
    matrix<-x$get()    
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m    
}
