
# Function to check that credMass is sane
checkCredMass <- function(x)  {
  if(length(x) != 1)
    stop("credMass must be a single value")
  if(is.na(x) || x <= 0 || x >= 1)
    stop("credMass must be between 0 and 1")
}
