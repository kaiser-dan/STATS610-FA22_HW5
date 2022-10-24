# Source functions
source("llr_functions.R")

# Create dummy data
# Note: log10(N) ~ [1,3,6]
x_s <- 1:10^1
x_m <- 1:10^2
x_l <- 1:10^3
y_s <- 2*x_s
y_m <- 2*x_m
y_l <- 2*x_l
z <- 0
omega <- 3

# Benchmark performance
print(microbenchmark::microbenchmark(
  small = llr(x=x_s, y=y_s, z=z, omega=omega),
  medium = llr(x=x_m, y=y_m, z=z, omega=omega),
  large = llr(x=x_l, y=y_l, z=z, omega=omega)
))
