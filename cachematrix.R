## A set of wrapper functions used to facilitate in the solving for an inverse of a matrix.  Once the inverted matrix has been calculated,
## it is cached so that it can be reused without the need of solving for it again.

## ****************************************************************************************************************
## Name: makeCacheMatrix
## Description: A constructor function that instantiates a set of functions to be used to prepare and solve for 
##              an invertable matrix
## Parameters: x: A square (invertable) matrix.  Defaults to an empty (NA) 1x1 matrix.
## Returns: A list containing a set of functions to facilitate in the maintenance of the entered matrix and the 
##          caching of its inverse.
## ****************************************************************************************************************
makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    
    set <- function(y)
    {
        # Check to see if the entered object is a matrix
        if (class(y) != "matrix")
        {
            message("Error: Class of object is not a matrix")
        }
        # Ensure that the matrix is invertable (i.e. a square)
        else if (nrow(y) != ncol(y))
        {
            message("Error: Matrix must be a square to be invertable")
        }
        else 
        {
            # Set to the new matrix to solve and reset the cached inverted matrix
            x <<- y
            inverse <<- NULL
        }
    }
    
    get <- function()
    {
        return (x)
    }
    
    setInvertedMatrix <- function(i)
    {
        # Check to see if the entered object is a matrix
        if (class(i) != "matrix")
        {
            message("Error: Class of object is not a matrix")
        }
        else
        {
            # Cache the inverse of the entered matrix object.
            inverse <<- i
        }
    }
    
    getInvertedMatrix <- function()
    {
        return (inverse)
    }
    
    return(list(set = set, get = get, setInvertedMatrix = setInvertedMatrix, getInvertedMatrix = getInvertedMatrix))

}


## ****************************************************************************************************************
## Name: cacheSolve
## Description: A function that solves for the inverse of a matrix, or uses a cached version of it if it exists.
## Parameters: x: An object that has been created by the construction function 'makeCacheMatrix' containing the matrix
##                to solve the inverse for.
##           ...: Additional paramters that are to be passed through into the 'solve' function that solves for the inverse
##                of the instantiated matrix.
## Returns: The inverse of the instantiated matrix, or the pre-existing cached version.
## ****************************************************************************************************************
cacheSolve <- function(x, ...)
{
    # Check to make sure that the object has been correctly instantiated by the 'makeCacheMatrix' function.
    # If there are any functions within the entered object not in the expected list of functions (or 'names(x)' resolves
    # to NULL), the logical list will resolve to FALSE and the subsequent inverted sum should resolve to a number > 0
    if (sum(!(list("set", "get", "setInvertedMatrix", "getInvertedMatrix") %in% names (x))) > 0)
    {
        message ("Object not instantiated correctly.  Please use 'makeCacheMatrix' function.")
    }
    else
    {
        m <- x$getInvertedMatrix()
        if (!is.null(m))
        {
            message ("Getting cached version of inverted matrix")
            return (m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setInvertedMatrix(m)
        return (m)
    }
}
