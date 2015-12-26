## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    
    set <- function(y)
    {
        if (class(y) != "matrix")
        {
            message("Error: Class of object is not a matrix")
        }
        else if (nrow(y) != ncol(y))
        {
            message("Error: Matrix must be a square to be invertable")
        }
        else 
        {
            x <<- y
            inverse <- NULL
        }
    }
    
    get <- function()
    {
        return (x)
    }
    
    setInvertedMatrix <- function(i)
    {
        if (class(i) != "matrix")
        {
            message("Error: Class of object is not a matrix")
        }
        else if (nrow(i) != ncol(i))
        {
            message("Error: Matrix must be a square to be invertable")
        }
        else
        {
            inverse <<- i
        }
    }
    
    getInvertedMatrix <- function()
    {
        return (inverse)
    }
    
    return(list(set = set, get = get, setInvertedMatrix = setInvertedMatrix, getInvertedMatrix = getInvertedMatrix))

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
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
