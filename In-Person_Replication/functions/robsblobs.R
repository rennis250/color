lab_delta <- 6/29

# for the PA271Q monitor
monXYZ <- t(matrix(c(203.63636364, 64.68, 65.8,
       105, 218.68, 26.32,
       9.54545455, 24.64, 346.54666667), nrow = 3, ncol = 3))
RGB2XYZ <- monXYZ
XYZ2RGB <- solve(RGB2XYZ)
monWP <- c(334.11636364,  350, 380.73212121)

xyY2XYZ <- function(xyY) {
  x <- xyY[1]
  y <- xyY[2]
  Y <- xyY[3]

  X <- (Y / y) * x
  Z <- (Y / y) * (1 - x - y)

  return(c(X, Y, Z))
}

lab_inv_f <- function(t) {
  if (t > lab_delta) {
    return (t ^ 3)
  } else {
    return (3 * (lab_delta ^ 2) * (t - 4/29))
  }
}

lab2xyz <- function(LAB) {
  l <- LAB[1]
  a <- LAB[2]
  b <- LAB[3]

  x_pre_f <- (l + 16) / 116 + (a / 500)
  y_pre_f <- (l + 16) / 116
  z_pre_f <- (l + 16) / 116 - b / 200

  X <- monWP[1] * lab_inv_f(x_pre_f)
  Y <- monWP[2] * lab_inv_f(y_pre_f)
  Z <- monWP[3] * lab_inv_f(z_pre_f)

  return (c(X, Y, Z))
}

xyz2rgb <- function(xyz) {
  return (XYZ2RGB %*% xyz)
}

lab2rgb <- function(lab) {
  xyz <- lab2xyz(lab)
  return (xyz2rgb(xyz))
}

lab_f <- function(t) {
  if (t > lab_delta^3) {
    return (t^(1/3))
  } else {
    return (t / (3 * lab_delta^2) + 4 / 29)
  }
}

xyz2lab <- function(XYZ) {
  xyz_s <- XYZ / monWP
  xsf <- lab_f(xyz_s[1])
  ysf <- lab_f(xyz_s[2])
  zsf <- lab_f(xyz_s[3])

  l <- 116 * ysf - 16
  a <- 500 * (xsf - ysf)
  b <- 200 * (ysf - zsf)

  return (c(l, a, b))
}

rgb2xyz <- function(rgb) {
  return (RGB2XYZ %*% rgb)
}

rgb2lab <- function(rgb) {
  xyz <- rgb2xyz(rgb)
  return (xyz2lab(xyz))
}