## This function creates a matrix, that cashes its inverse. After computing the inverse it
## saves the invers, which saves the computational effort.
## This method creates a matrix and its methods: set(changes the values of matrix), get(gets the values of matrix), 
##set_inv (changes the values of the invers matrix), get_inv(gets the values of the invers matrix).



makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
  x <<- y
  inv_x<<- NULL
  }
  
  get <- function() x
  set_inv <- function(inv) inv_x <<- inv
  get_inv <- function() inv_x
  
  list(set=set, get=get,
       set_inv=set_inv,
       get_inv=get_inv)
  }

## Here we see whether the inverse is already cashed. In case it it, we return the values. In case it is not, 
##we do the inversion and save in inv_x

cacheSolve <- function(x, ...) {
  ## Return the invers of x
  temp_inv=x$get_inv()
  if(!is.null(temp_inv)) {
    return(temp_inv)
  }
  
  
  x_matrix <- x$get()
  temp_inv <- solve(x_matrix,...)
  x$set_inv(temp_inv)
  temp_inv
}
