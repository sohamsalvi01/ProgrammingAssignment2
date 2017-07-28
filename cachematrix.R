## Put comments here that give an overall description of what your
## 'makeCacheMatrix 'function gets the value of matrix and creates the set and get functions.
## 'cacheSolve' functions takes the matrix and calcuates the inverse and checks if the value for inverse is present for same matrix. if yes then it prints it else it calcuates and prints it 

## Write a short comment describing this function
# makeCacheMatrix <- function(x = matrix()) {
# 		'makeCacheMatrix 'function gets the value of matrix and creates the set and get functions.
# }

## Write a short comment describing this function
# cacheSolve <- function(x, ...) {
# 		'cacheSolve' functions takes the matrix and calcuates the inverse and checks if the value for inverse is present for same matrix. if yes then it prints it else it calcuates and prints it   ## Return a matrix that is the inverse of 'x'
# }




makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                        
  set <- function(y) {                #sets the matirx value to x
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(s) m <<- s   #sets the inverse value of matrix to m 
  getinverse <- function() m
  list(set = set, get = get,          #lists functions the objects of the cache matrix   
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  m <- x$getinverse()                 #calls the inverse function and sets the inverse of the matrix  
  if(!is.null(m)) {                   #is inverse already exist then calls the cache objects  
    message("getting cached data")
    return(m)
  }
  data <- x$get()                     #otherwise gets the matrix and solve() calcultes the inverse and then set the value of matrix
  m <- solve(data, ...)               #does the inverse of matrix
  x$setinverse(m)
  m
}



#############
#RUNNING THE ABOVE CODE
#############
# > x<-c(2,3,5)
# > y<-c(3,6,8)
# > mt<-cbind(x,y)
# > mt
# x y
# [1,] 2 3
# [2,] 3 6
# [3,] 5 8
# > solve(mt)
# Error in solve.default(mt) : 'a' (3 x 2) must be square
# > z<-c(6,9,3)
# > mt<-cbind(mt,z)
# > mt
# x y z
# [1,] 2 3 6
# [2,] 3 6 9
# [3,] 5 8 3
# >mt
# x y z
# [1,] 2 3 6
# [2,] 3 6 9
# [3,] 5 8 3
# > m1<-makeCacheMatrix(mt)
# > cacheSolve(m1)
# [,1]        [,2]          [,3]
# x  1.5000000 -1.08333333  2.500000e-01
# y -1.0000000  0.66666667 -9.251859e-17
# z  0.1666667  0.02777778 -8.333333e-02
# > cacheSolve(m1)
# getting cached data
# [,1]        [,2]          [,3]
# x  1.5000000 -1.08333333  2.500000e-01
# y -1.0000000  0.66666667 -9.251859e-17
# z  0.1666667  0.02777778 -8.333333e-02
# > cacheSolve(m1)


