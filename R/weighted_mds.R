#' Weighted MDS
#'
#' @param D a distance or dissimilarity
#' @param f the weights
#'
#' @export

mds <- function(D, f) {
  n <- nrow(D)
  H = diag(n) - rep(1, n) %*% t(f)
  # H %*% H - H about zero; OK
  B = -0.5 * H %*% D %*% t(H)
  K = diag(sqrt(f)) %*% B %*% diag(sqrt(f))

  lambda = eigen(K)$values
  if (any(lambda<0)) {
    warning(paste("Negative values found for lambda, normalising to 0.\n", lambda[which(lambda<0)]))
    lambda = pmax(rep(0, length(lambda)), lambda)
  }
  lambda_rel = lambda/sum(lambda)

  round(100*lambda_rel, 1)

  U = eigen(K)$vectors
  Xtilde = diag(1/sqrt(f)) %*% U %*% diag(sqrt(lambda))
  result = list(Xtilde, lambda, f)
  names(result) = c("Xtilde", "lambda", "weights")
  class(result)="mds"
  result
}

#' Plot Weighted MDS
#' @import graphics
#' @importFrom wordcloud textplot
#' @importFrom grDevices rainbow rgb
#' @importFrom stringr str_sort
#'
#' @param x an mds object
#' @param dimentions the dimentions to be plotted ex: c(1,2)
#' @param group_by a vector containing a group for each individual
#' @param ... graphical params
#'
#' @export
#'
plot.mds <- function(x, dimentions, group_by, ...) {
  Xtilde = x$Xtilde
  lambda = x$lambda
  weights = x$weights
  d1 = dimentions[1]
  d2 = dimentions[2]
  contributions = round(lambda/sum(lambda), 4) * 100
  y_labels <- paste("Dimension", d1, "(", contributions[d1], "% )")
  x_labels <- paste("Dimension", d2, "(", contributions[d2], "% )")
  labels <- rownames(Xtilde)
  title <- c()
  colors <- grDevices::rainbow(length(unique(group_by)))
  names(colors) <- stringr::str_sort(unique(group_by), numeric = TRUE)
  # check for additional function arguments
  if ( length(list(...)) ) {
    Lst <- list(...)
    if ( !is.null(Lst$zlim) ) {
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if ( !is.null(Lst$y_labels) ) {
      y_labels <- c(Lst$y_labels)
    }
    if ( !is.null(Lst$x_labels) ) {
      x_labels <- c(Lst$x_labels)
    }
    if ( !is.null(Lst$main) ) {
      title <- Lst$main
    }
    if ( !is.null(Lst$colors) ) {
      colors <- Lst$colors
    }
    if ( !is.null(Lst$labels) ) {
      labels <- Lst$labels
    }
  }
  # check for null values
  if ( is.null(x_labels) ) {
    x_labels <- c(1:ncol(Xtilde))
  }
  if ( is.null(y_labels) ) {
    y_labels <- c(1:nrow(Xtilde))
  }
  par(mar = c(18.1, 4.1, 4.1, 4.1))
  # construction of a cex scale such that min(weights) = a and max(weights) = b
  a <- 0.5
  b <- 6
  cexf <- (sqrt(weights) - min(sqrt(weights)))/(max(sqrt(weights)) - min(sqrt(weights)))*(b - a) + a
  plot(
    -Xtilde[, d1],
    Xtilde[, d2],
    cex = (cexf/3),
    main = title,
    xlab = x_labels,
    ylab = y_labels,
    cex.axis = 1.1,
    cex.lab = 1.1,
    col = colors[group_by]
  )

  wordcloud::textplot(
    -Xtilde[, d1],
    Xtilde[, d2],
    labels,
    show.lines = TRUE,
    new = FALSE,
    cex = .6,
    col = rgb(.11, .11, .11, .33)
  )

  if (length(unique(group_by)) < 10) {
    legend(
      "bottom",
      legend = stringr::str_sort(unique(group_by), numeric = TRUE),
      col = colors[stringr::str_sort(unique(group_by), numeric = TRUE)],
      pch = 2,
      xpd = TRUE,
      horiz = TRUE,
      inset = c(0, -.08),
      cex = .8,
      bty = "o"
    )
  } else {
    par(mar=c(0, 0, 0, 0))
    legend(
      "bottom",
      legend = str_sort(unique(group_by), numeric = TRUE),
      col = colors[str_sort(unique(group_by), numeric = TRUE)],
      pch = 2,
      xpd = TRUE,
      ncol = 5,
      inset = c(0, -.075 - (.008 * length(unique(group_by))/5)), # 0.01
      cex = .8,
      bty = "o"
    )
  }

  abline(h = 0)
  abline(v = 0)
  layout(1)
}
