# Define a class for the elliptic curve
EllipticCurve <- function(a, b, p) {
  list(a = a, b = b, p = p)
}

# Check if a point is on the curve
isOnCurve <- function(x, y, curve) {
  left <- y^2 %% curve$p
  right <- (x^3 + curve$a * x + curve$b) %% curve$p
  return(left == right)
}

# Add two points on the curve
pointAddition <- function(p1, p2, curve) {
  x1 <- p1[1]
  y1 <- p1[2]
  x2 <- p2[1]
  y2 <- p2[2]
  
  if (x1 == x2 && y1 == y2) { # Point doubling
    lambda <- (3 * x1^2 + curve$a) * inverse_mod(2 * y1, curve$p) %% curve$p
  } else { # Point addition
    lambda <- (y2 - y1) * inverse_mod(x2 - x1, curve$p) %% curve$p
  }
  
  x3 <- (lambda^2 - x1 - x2) %% curve$p
  y3 <- (lambda * (x1 - x3) - y1) %% curve$p
  
  return(c(x3, y3))
}

# Multiply a point on the curve by a scalar
scalarMultiplication <- function(p, k, curve) {
  result <- c(0, 0)
  
  while (k > 0) {
    if (k %% 2 == 1) {
      result <- pointAddition(result, p, curve)
    }
    p <- pointAddition(p, p, curve)
    k <- k %/% 2
  }
  
  return(result)
}

# Find the modular inverse of a number
inverse_mod <- function(a, n) {
  t <- 0
  newt <- 1
  r <- n
  newr <- a
  
  while (newr != 0) {
    quotient <- r %/% newr
    t <- newt
    newt <- t - quotient * newt
    r <- newr
    newr <- r - quotient * newr
  }
  
  if (r > 1) {
    stop("a is not invertible")
  }
  
  if (t < 0) {
    t <- t + n
  }
  
  return(t)
}

# Encryption function
encrypt <- function(publicKey, plaintext, curve) {
  # Generate a random secret key
  secretKey <- sample(1:(curve$p - 1), 1)
  
  # Compute the shared secret point
  sharedPoint <- scalarMultiplication(publicKey, secretKey, curve)
  
  # Encode the plaintext
  encodedPlaintext <- utf8ToInt(plaintext)
  
  # Encrypt each character using the x-coordinate of the shared secret point
  encryptedChars <- sapply(encodedPlaintext, function(char) {
    point <- scalarMultiplication(sharedPoint, char, curve)
    return(point[1])
  })
  
  return(encryptedChars)
}

# Decryption function
decrypt <- function(privateKey, ciphertext, curve) {
  # Compute the shared secret point
  sharedPoint <- scalarMultiplication(privateKey, curve$p - 1, curve)
  
  # Decrypt each character using the x-coordinate of the shared secret point
  decryptedChars <- sapply(ciphertext, function(char) {
    point <- scalarMultiplication(sharedPoint, char, curve)
    return(as.integer(point[1]))
  })
  
  # Decode the decrypted characters
  decryptedPlaintext <- intToUtf8(decryptedChars)
  
  return(decryptedPlaintext)
}

# Example usage
curve <- EllipticCurve(2, 2, 17)
privateKey <- 5
publicKey <- scalarMultiplication(curve$G, privateKey, curve)
plaintext <- "Hello, world!"

# Encryption
encrypted <- encrypt(publicKey, plaintext, curve)
cat("Encrypted ciphertext:", encrypted, "\n")

# Decryption
decrypted <- decrypt(privateKey, encrypted, curve)
cat("Decrypted plaintext:", decrypted, "\n")
