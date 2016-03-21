## Below are functions that are used to create an object taht stores a matirx
## and caches its inverse

## This function creates a matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()){
         m <- NULL
         set <- function(y){
               x <<- y
               m <<- NULL
          }
       get<-function()x
       setInverse<-function(inverse)m<<-inverse
       getInverse<-function()m
       list(set=set,
            get=get,
            setInverse=setInverse,
            getInverse=getInverse)
}


## This function computes the inverse. If the inverse has already been 
## calculated, then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        if (!is.null(m)){
             message("get cached data")
             return (m)
        }
        mat<-x$get()
        m<-solve(mat,...)
        x$setInverse(m)
        m
}
