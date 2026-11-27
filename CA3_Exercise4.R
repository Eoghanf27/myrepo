Q1

install.packages('devtools')
devtools::install_github('Keefe-Murphy/IMIFA')




library(IMIFA)

class(olive)



class(olive) <- c("mydf", class(olive))
class(olive)



print.mydf <- function(x, n = 5) {
  
  x <- as.data.frame(x)
  
  print(x[1:min(n, nrow(x)), 1:min(n, ncol(x))])
  
}
print(olive[-1,])



print.mydf <- function(x, n = 5) {
  
  x <- as.data.frame(x)
  
  nr <- min(n, nrow(x))
  nc <- min(n, ncol(x))
  
  if (n > nrow(x) | n > ncol(x)) {
    warning("n exceeds the number of rows or columns")
  }
  
  message("Printing ", nr, " rows and ", nc, " columns.")
  
  print(x[1:nr, 1:nc, drop = FALSE])
  
}
olive



print(olive, n = 1000)



?density



max_dens <- function(x, ...) {
  UseMethod("max_dens")
}



max_dens.density <- function(x, ...) {
  
  max_y <- max(x$y)
  
  max_x <- x$x[which.max(x$y)]
  
  c(y = max_y, x = max_x)
  
}



my_density <- density(olive$palmitic)
max_dens(my_density)


library(IMIFA)
olive



numeric_vars <- names(olive)[sapply(olive, is.numeric)]



density_matrix <- vapply(numeric_vars, function(var) {
  
  d <- density(olive[[var]], kernel = "epanechnikov")
  
  peak <- max_dens(d)
  
  c(x = peak["x"], y = peak["y"])
  
}, FUN.VALUE = c(x = 0, y = 0))



d <- density(olive$palmitic, kernel = "epanechnikov")
peak <- max_dens(d)
plot(d, main = "palmitic")
segments(
  x0 = peak["x"],
  y0 = 0,
  x1 = peak["x"],
  y1 = peak["y"],
  col = "green"
)

q2

f <- function(x) {
  x + 3
}

g <- function(x) {
  x^2 - 4
}




curve(f(x), from = -4, to = 4,
      ylim = c(-6, 12))

curve(g(x), from = -4, to = 4,
      add = TRUE)




root1 <- uniroot(function(x) f(x) - g(x),
                 interval = c(-4, -2),
                 tol = 1e-8)

root2 <- uniroot(function(x) f(x) - g(x),
                 interval = c(3, 4),
                 tol = 1e-8)

a <- root1$root
b <- root2$root




area_f <- integrate(f, lower = a, upper = b)

area_g <- integrate(g, lower = a, upper = b)

area <- area_f$value - area_g$value

area

q3

df <- with(mtcars, data.frame(
  y = mpg,
  x1 = disp,
  x2 = hp,
  x3 = wt
))
head(df)



nll_lm <- function(par, data) {
  
  beta <- par[1:4]
  sigma2 <- par[5]
  
  y <- data$y
  
  X <- cbind(1, data$x1, data$x2, data$x3)
  
  e <- y - X %*% beta
  
  n <- length(y)
  
  nll <- (n/2) * log(2*pi*sigma2) +
    sum(e^2)/(2*sigma2)
  
  return(nll)
}



initial <- c(
  mean(df$y),
  0,
  0,
  0,
  var(df$y)
)



result <- optim(
  par = initial,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = c(-Inf, -Inf, -Inf, -Inf, 1e-8),
  upper = c(Inf, Inf, Inf, Inf, Inf),
  hessian = TRUE
)



beta_hat <- result$par[1:4]

sigma2_hat <- result$par[5]

beta_hat
sigma2_hat



sqrt(sigma2_hat)



X <- cbind(1, df$x1, df$x2, df$x3)

y <- df$y
beta_matrix <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat
beta_matrix



fitted_values <- X %*% beta_matrix
residuals <- y - fitted_values
SSE <- sum(residuals^2)
sigma2_matrix <- SSE / length(y)

sigma2_matrix
sigma2_hat
sigma2_unbiased <- SSE / (length(y) - 4)
sigma2_unbiased



hessian = TRUE
cov_matrix <- solve(result$hessian)

standard_errors <- sqrt(diag(cov_matrix)[1:4])
standard_errors

model <- lm(y ~ x1 + x2 + x3, data = df)


lm_coefficients <- coef(model)


lm_sigma <- sigma(model)


lm_coefficients
lm_sigma
