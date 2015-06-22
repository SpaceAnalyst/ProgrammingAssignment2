# These 2 functions completed in fullfillment of Coursera-JHU-R_Programming-
#         ProgrammingAssignment2
# 
# This functiontion 'makeCacheMatrix' creates a special matrix, which is a really
# a list of containing  containing a function to
# 
# 1. set the matrix (setMatrix(argument: x = matrix()))
# 2. retrieve the matrix (getMatrix(no argument))
# 3. set the inverse of the argument matrix (setInverseMatrix(argument: inverse 
#                                                             matrix))
# 4. retrieves the inverse matrix (getInverseMatrix(no argument))
# 
# The setmatrix() and setInverseMatrix() functions use the '<<-' operator to 
# assign values in the cached environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y){
                x<<- y
                m<<- NULL
        }
        
        getMatrix <- function() x
        setInverseMatrix <- function(inverse = NULL){m <<- inverse}
        getInverseMatrix <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
                
}


# The function 'cacheSolve()' calculates the inverse matrix of the special matrix
# created by the 'makeCacheMatrix' function above.  However, it first checks to 
# see if the inverse matrix has already been calculated.  If so, it retrieves the
# inverse matrix from the cache and skip the computation.  Otherwise, it 
# calculates the inverse matrix using the solve() function and sets the value of
# inverse matrix into cache using the 'setInverseMatrix' function.  'cacheSolve'
# takes as an argument the matrix created by 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverseMatrix()
        
        if(!is.null(m)) {
                message("Getting cached inverse matrix")
                return(m)
        }
        
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m        
}
