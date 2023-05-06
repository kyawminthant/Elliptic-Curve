# Define the elliptic curve parameters
a <- -1
b <- 1
p <- 23

# Define the point addition function
point_addition <- function(p1, p2) {
  x1 <- p1[1]
  y1 <- p1[2]
  x2 <- p2[1]
  y2 <- p2[2]
  
  if (x1 == x2 && y1 == y2) { # Point doubling
    lambda <- ((3*x1^2 + a) * inverse_mod(2*y1, p)) %% p
  } else { # Point addition
    lambda <- ((y2 - y1) * inverse_mod(x2 - x1, p)) %% p
  }
  
  x3 <- (lambda^2 - x1 - x2) %% p
  y3 <- (lambda*(x1 - x3) - y1) %% p
  
  return(c(x3, y3))
}

# Define the scalar multiplication function
scalar_multiplication <- function(p, k) {
  result <- c(0, 0)
  
  while (k > 0) {
    if (bitwAnd(k, 1) == 1) {
      result <- point_addition(result, p)
    }
    p <- point_addition(p, p)
    k <- bitShiftR(k, 1)
  }
  
  return(result)
}

# Define the inverse modulo function
inverse_mod <- function(a, m) {
  t <- 0
  newt <- 1
  r <- m
  newr <- a
  
  while (newr != 0) {
    quotient <- floor(r / newr)
    t <- newt
    newt <- t - quotient * newt
    r <- newr
    newr <- r - quotient * newr
  }
  
  if (r > 1) {
    stop("a is not invertible")
  }
  
  if (t < 0) {
    t <- t + m
  }
  
  return(t)
}

# Test the functions with an example
x1 <- 5
y1 <- 1
x2 <- 5
y2 <- 16
p1 <- c(x1, y1)
p2 <- c(x2, y2)

# Point addition
result <- point_addition(p1, p2)
cat("Point addition result:", result[1], result[2], "\n")

# Scalar multiplication
k <- 3
result <- scalar_multiplication(p1, k)
cat("Scalar multiplication result:", result[1], result[2], "\n")
