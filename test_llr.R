context("Check local linear regression function")
source("llr_functions.R")

library("Matrix")

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})


test_that("make_weight_matrix works on simple cases", {
  ## check that the output is a diagonal matrix, that all the elements are positive, that the weights are correct in simple cases where you know what the output shuold be
  W <- make_weight_matrix(z,x,omega)
  
  # Note: I thought using  Matrix::isDiagonal would be more efficient and implemented it over the weekend
  # Diagonal
  expect_equal(
    isDiagonal(W),
    TRUE
  )
  
  # Positivity (well, non-negative)
  expect_equal(
    all(diag(W) >= 0),
    TRUE
  )
  
  # Simple case diagonal all 0 (unit norm)
  expect_equal(
    diag(make_weight_matrix(0, rep(1,10), 1)),
    rep(0,10)
    
  )
})

test_that("make_predictor_matrix works on simple cases", {
  ## write tests to check that the dimensions are correct, the first column is all 1's, etc.
  
  # Test set-up
  X <- make_predictor_matrix(x)
  
  # Dimensions
  expect_equal(
    dim(X),
    c(15, 2)
  )
  
  # First column all ones
  expect_equal(
    X[,1],
    rep(1,15)
  )
  
  # Second column x vector
  expect_equal(
    X[,2],
    x
  )
})


data("french_fries")
french_fries = french_fries[complete.cases(french_fries),]
z = seq(0, 15, length.out = 100)
fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 12)
plot(z, fits)
