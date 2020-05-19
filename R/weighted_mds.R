#' Weighted MDS
#'
#' @param D a distance or dissimilarity
#' @param f the weights
#'
#' @export
weighted_mds <- function(D, f) {
  n <- nrow(D)
  H = diag(n) - rep(1, n) %*% t(f)
  # H %*% H - H about zero; OK
  B = -0.5 * H %*% D %*% t(H)
  K = diag(sqrt(f)) %*% B %*% diag(sqrt(f))

  lambda = eigen(K)$values
  lambda = pmax(0, lambda)
  lambda_rel = lambda/sum(lambda)

  round(100*lambda_rel, 1)

  U = eigen(K)$vectors
  Xtilde = diag(1/sqrt(f)) %*% U %*% diag(sqrt(lambda))
  result = list(Xtilde, lambda, f)
  names(result) = c("mds.Xtilde", "mds.lambda", "weights")
  result
}
