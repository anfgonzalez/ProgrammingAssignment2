+	## these functions help to resolve the inverse of a matrix specially when 
+	## this procedure it's executed more than one time or in a loop process
+	 
+	## "makeMatrix" function creates a list that contains four functions that help to calculate 
+	## the inverse of a matrix: 1) a function to set the content of a matrix,
+	## 2) the function to get the matrix , 3) a function to calculate and set the inverse, 
+	## and finally 4) the function to get the inverse  of the matrix.
 	
+	makeCacheMatrix <- function(x = matrix()) { 
+	      inv<-NULL                                   ##stores default value for the inverse
+	      setmatrix<-function(y){                     ## function to set the values of the matrix and the inverse
+	            x<<-y                                 
+	            inv<<-NULL                            ##if a new matrix it's entered then the inverse value it's set to default value
+	      }
+	      getmatrix<-function() x                     ##show the matrix using an anonymus function
+	      setinv<-function(invmatrix) inv<<-solve     ##solve the inverse of the matrix assigning solve to the anonymus function inv
+	      getinv<-function() inv                      ##show the inverse calculated
+	      list(setmatrix = setmatrix, getmatrix = getmatrix,   ##assign to the list every function described before
+	           setinv = setinv, getinv = getinv )
 	}
 	
 	
 	cacheSolve <- function(x, ...) {
 	        ## Return a matrix that is the inverse of 'x'
+	      inv<-x$getinv()
+	      if(!is.null(inv)){                       ##Evaluate if there is another value stored for the inverse
+	            message("Getting cached inverse")  ##Alert Message
+	            return(inv)                        ##return the stored inverse
+	      }
+	      inv<-solve(x$getmatrix(),...)            ##solve the inverse of the matrix using the function "solve"
+	      x$setinv(inv)                            ##set the value of the inverse int the special list
+	      inv                                      ## show the value for the inverse calculated
+	      
 	}
