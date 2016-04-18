#  The makeCacheMatrix defines a set of fuction and returs it as a list
makeCacheMatrix <- function(x = matrix()) {
    #  Note: Make sure to set matrix M in the Global environment before using
    m <- NULL   # "m" is the inverse of Matrix "x" 
    #   SetMatrix should be called for a new Matrix ONLY
    #   Set the value of a Matrix (For a new matrix "x")
    setMatrix <- function(y) {
        M <<- y   # "y" is an input matrix whose inverse is being evaluated
        mI <<- NULL  # Inverse of M (y) being set in Global environment
    }
    
    #   Get the value of Matrix M from Global environment
    getMatrix <- function() M # Returns the value of Global value of M
    
    #   Set the valye of Inverse matrix in the Memory
    setIMatrix <- function(InverseMatrix) {
        m <<- InverseMatrix  # Having calculated the inverse update its value 
    }
    
    #  Get the current value invers "m" from Global environment 
    getIMatrix <- function() m  # Here m is a free variable (Lexical scoping)
    
    #   The list of functions is returned by this function
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setIMatrix = setIMatrix,
         getIMatrix = getIMatrix)
}


cacheSolve <- function(x, ...) {
    
    #  Check the value of the value of inverse stored in memory
    m <- x$getIMatrix()
    #  When inverse exist, it's value is returned
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #  "data" is the current value of matrix in question 
    data <- x$getMatrix()
    
    #   Calculates the inverse of the matric "data"
    m <- solve(data)
    
    #   Now set the value of inverse in Global environment for future use
    x$setIMatrix(m)
    
    #   Returns the value of inverse
    m
}