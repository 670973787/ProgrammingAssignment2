## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.
+        
+##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
 
+##   1. set the value of the matrix
+##   2. get the value of the matrix
+##   3. set the value of the inverse
+##   4. get the value of the inverse
+ makeCacheMatrix <- function(x = matrix()) {
+         inv<-NULL
+         set<-function(y){
+                x<<-y
+                inv<<-NULL
+         }
+         get <- function() x
+         setinverse <- function(inverse) inv<<-inverse
+         getinverse <- function() inv
+         list(set=set, get=get,
+              setinverse = setinverse,
+              getinverse = getinverse)
 }
## The following function calculates the inverse of the special "matrix" created with the above function. 
+## First it will check if the matirx is a square matirx and if it is reversible.                 
+## If it has an inverse, it then will checks to see if the inverse has already been calculated. 
+## If so, it gets the mean from the cache and skips the computation.
+## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
+         a <- x$get()
+         if(ncol(a)!=nrow(a))
+                    print("we need a square matrix")
+         else if (det(a)==0)
+                    print("this matrix is irreversible")
+         else{
+                    if(!is.null(inv)) {
+                              message("getting cached data")
+                              return(inv)
+                              }
+                    inv<-solve(a)
+              }
+         x$setinverse(inv)
+         inv
+}    
