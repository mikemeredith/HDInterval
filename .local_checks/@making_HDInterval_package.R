# Individual checks

# setwd("D:/Github/HDInterval_package")
# setwd("../..")
dir()

# Spell check
# ===========
library(devtools)
sIg <- scan("spellcheckIgnore.txt", what='character', comment="#")
tmp <- spell_check("HDInterval", ignore=c(hunspell::en_stats, sIg), "en_GB")
length(tmp)  # number of misspellings found
tmp  # error if length == 0

# Development
# ===========
devtools::load_all("HDInterval")
system("R CMD INSTALL HDInterval") # Use this for a "dev" install.

# Build and check
# ===============
unlink(list.files(pattern="Rplots.pdf", recursive=TRUE))
system("R CMD build HDInterval")  # Produces the .tar.gz file
pkg <- "HDInterval_0.2.3.tar.gz"  # <-- fix version number here ################

# Pick one to check:
## on desktop
system(paste("R CMD check ", pkg))
system(paste("R CMD check ", pkg, "--as-cran"))  # as-cran now runs donttest
## on laptop
system(paste("R CMD check ", pkg, "--no-manual"))
system(paste("R CMD check ", pkg, "--as-cran --no-manual"))

# Pick one to install
system(paste("R CMD INSTALL ", pkg))            # install only
system(paste("R CMD INSTALL ", pkg, "--build")) # install and produce the .zip binary

library(testthat)
test_package("HDInterval")

# Try it out
# ==========
library(HDInterval)
?HDInterval

example(hdi)
tst <- rgamma(1e5, 2.5, 2)
hist(tst)
hdi(tst)
hdi(tst, 0.8)

dens <- density(tst)
hdi(dens)

tst <- data.frame(mu=rnorm(1e4,4,1), sigma=rlnorm(1e4))
hdi(tst)
tst$txt <- LETTERS[1:25]
hdi(tst)

hdi(qgamma, shape=2.5, rate=2)

# Test methods for classes
test <- new.env()
load("../mcmc_test_objects.Rdata", test)
test <- as.list(test)
test <- test[order(names(test))]
names(test)
for(i in seq_along(test)) {
  cat("\n*** ", names(test)[i], " ***\n\n")
  tst <- try(hdi(test[[i]]))
  if(inherits(tst, "try-error"))
    next
  print(tst)
}

# Wierd input
hdi(rep(NA_real_, 5))
hdi(rep(Inf, 5))
hdi(1:10, 2) # error
hdi(LETTERS)