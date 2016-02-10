
# Function to check that credMass is sane
checkCredMass <- function(x)  {
  if(is.na(x) || length(x) != 1 || x <= 0 || x >= 1)
    stop("credMass must be in 0 < credMass < 1")
}
