L <- c(0, 5)
x <- runif(10, L[1], L[2])
m <- data.frame(a = sample(letters, length(x)), b = seq_along(x))
X <- lpp1(x, domain = L, marks = m)
expect_true(inherits(X, "lpp"))
expect_error(lpp1(x, domain = c(5,0)))
