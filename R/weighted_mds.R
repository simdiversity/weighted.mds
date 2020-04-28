library(wordcloud)

weighted_mds <- function(dissimilarity, weights) {
  n <- nrow(dissimilarity)
  H = diag(n) - rep(1, n) %*% t(weights)
  # H %*% H - H about zero; OK
  B = -0.5 * H %*% dissimilarity %*% t(H)
  K = diag(sqrt(weights)) %*% B %*% diag(sqrt(weights))

  lambda = eigen(K)$values
  lambda = pmax(0, lambda)
  lambda_rel = lambda/sum(lambda)

  round(100*lambda_rel, 1)

  U = eigen(K)$vectors
  Xtilde = diag(1/sqrt(weights)) %*% U %*% diag(sqrt(lambda))
  result = list(Xtilde, lambda)
  names(result) = c("mds.Xtilde", "mds.lambda")
  result
}
