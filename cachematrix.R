## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
             inv<-NULL #set the value of matrix
             set<-function(y){
               x<<-y
               inv<<-NULL
             }
             get<-function()x #get the value of matrix
             setinv<-function(inverse)inv<<-inverse #set the value of inverse of the matrix
             getinv<-function()inv #get the value of inverse of the matrix
             list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv() #get the inverse from the cache
        if(!is.null(inv)){ #if the inverse is calculated or not
          message("getting cached data")
          return(inv)
        } 
        data<-x$get() #if not, it gets the matrix and calculate via solve function
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}

#here some example to try: 
m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
n <- cacheSolve(m)
m$get()
n
m$getinv()
