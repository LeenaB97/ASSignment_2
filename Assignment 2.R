makeVector <- function(x = numeric()) {
  m <- NULL  # Initialize the cached mean as NULL
  set <- function(y) {  # Function to set the value of the vector
    x <<- y           # Assign to `x` in the parent environment
    m <<- NULL        # Reset the cached mean when vector changes
  }
  get <- function() x  # Function to retrieve the vector
  setmean <- function(mean) m <<- mean  # Function to cache the mean
  getmean <- function() m  # Function to retrieve the cached mean
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()  # Retrieve the cached mean
  if (!is.null(m)) {  # If the mean is already cached
    message("getting cached data")
    return(m)  # Return the cached mean
  }
  data <- x$get()  # Retrieve the vector
  m <- mean(data, ...)  # Compute the mean
  x$setmean(m)  # Cache the computed mean
  m  # Return the computed mean
}
