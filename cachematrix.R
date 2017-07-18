## Put comments here that give an overall description of what your
## functions do

#The input must be a square invertible matrix

# 'a' is an object that will be assigned later

#The function 'set' will assign the input to 'x' in the parent environment
#Also clears any old value in the parent environment = "a"

#The function 'get' retrieves the value of 'x' from the parent environment.

#The function 'setinv' uses <<- to assign the input value 'a' in the
#parent environment.

#function 'getinv' finds 'a' to retrieve its value.

#The function 'a' tries to retrive an inverse of the original input
#'x' by calling the 'getinv' function on the input object.

## Write a short comment describing this function

#The purpose of the function 'CacheMatrix' is to assign
#the correct values within the parent environement to be used in a
#later function. In storing these values within the parent function,
#it allows future functions the chance to save time. It makecacheMatrix
#does this by storing the previously calculuated matrix inverse output.
#If that same matrix inverse is called on again later the cached value can 
#be used.

#The input must be a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {

        a <- NULL
        set <- function(y) {
                x <<- y                      
                a <<- NULL
        }
        
        get <- function() x
        setinv <- function(solve) a <<- solve
        getinv <- function() a
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

#The 'cachesolve' function is where the inverse of the matrix is actually
#calculuated. Once the inverse matrix is calculated, it is stored so that 
#the function 'makeCacheMatrix' can check next time if the stored matrix is
#the same, or a different matrix. If it is the same matrix, cachesolve will
#output the cached value. If it is a different output, the cachesolve will
#know because it will be given 'NULL' as its input. This will then tell
#cachesolve to use the original input matrix and will solve the matrix. The
#output will be the inverse of the original input matrix.

#The 'cachesolve' function requires as its input,the output from
#'makeCacheMatrix'. 

cacheSolve <- function(x, ...) {
        a <- x$getinv()
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        
        data <- x$get()
        a <- solve(data, ...)
        x$setinv(a)
        a
        
        ## Return a matrix that is the inverse of 'x'
}
