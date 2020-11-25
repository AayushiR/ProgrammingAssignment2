#Function to make a special matrix
makeCacheMatrix <- function(x = matrix()) #initialise function and pass matrix as argument{
        i <- NULL #initialise a null character
        set <- function(y) #function to set matrix value
                {
                x <<- y
                i <<- NULL
        }
        get <- function() x #get the matrix
        setinverse <- function(inverse) i <<- inverse #set inverse of matrix
        getinverse <- function() i #get inverse of matrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse) #make a list 
}

cacheSolve <- function(x, ...) #initialise function to solve for inverse{
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...) #returns inverse of matrix
        x$setinverse(i)
        i
}