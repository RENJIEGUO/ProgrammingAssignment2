## The function makeCacheMatrix creates an object which is a matrix
## and the function cacheSolve will calculate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
}
        get<-function()x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function()inv
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        }


## The function cacheSolve will return the inverse of the matrix created by the function makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached inverse matrix")
                return(inv)
                }
        else{
                inv<-solve(x$get()}
                           x$setinverse(inv)
                           return(inv)
                           }
}
