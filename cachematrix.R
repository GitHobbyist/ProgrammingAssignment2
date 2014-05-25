## The makeCacheMatrix function creates a special matrix object that can cache it's inverse.    
## Since matrix inversion can result in time consuming computations, this function will      
## enable us to cache it instead of recalculating it, to avoid repetition. For the input of 
## the function we assume that the matrix supplied is always n by n squared invertible. 
##Currently the default input has been set to 2 by 2 matrix. 
## Think of this function as a class that retuns methods and properties as a list.   

makeCacheMatrix <- function(x = matrix(data=numeric(),nrow=2,ncol=2)) {
        m <- NULL                                                       ##Object assigned undefined value. Needed for first function run.
        set <- function(y) {                                            ## Set value of original matrix, set to a 2 by 2 matrix.     
                x <<- matrix(data=y,nrow=2,ncol=2)
                m <<- NULL                                              ##Overwrites the 'm' value from a different function environment.                 
        }
        get <- function() x                                             ##Get the the original matrix.   
     
        setinv <- function(inverse) m <<- inverse                       ##Set the inverse of the original matrix using cacheSolve function.     
        getinv <- function() m                                          ##Get the inverse of the original matrix, enables to get matrix from  
                                                                        ##cache instead of recalculating it.  
        list(set = set, get = get, setinv = setinv,                     ##List that makes up the special matrix object, containing four subfunctions  
             getinv = getinv)                                           ## as elements.     
        
                                                                        ##This object is feed to the cacheSolve(object) function.    
}


## The cacheSolve function when feed the special matrix object will evaluate
## either to calculate the inverse (solve) of the original matrix or get it
## from cache as it may have been already calculated.  

cacheSolve <- function(x, ...) {
        m <- x$getinv()                           ## Initially NULL assigned from makeCacheMatrix (Parent environment).                                                  
        if(!is.null(m)) {                         ## If 'm' NOT NULL also means inverse already calculated.  
                message("getting cached data")
                return(m)                         ## Return 'm' brought from cache instead of resolving it's inverse.   
        }
        matrix <- x$get()                         ##Else get original matrix,        
        m <- solve(matrix)                        ##solve it's inverse, 
        x$setinv(m)                               ##set it's inverse for special matrix object    
        m                                         ## AND 
        
        
        ## Return a matrix that is the inverse of 'x'
}
