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

  K_eigen = eigen(K)
  lambda = K_eigen$values
  if (any(lambda<0)) {
    if (!all.equal(lambda, rep(0,n))) warning(paste(
        "Negative values found for lambda:\n",
        paste(sprintf("%.0E", lambda[which(lambda<0)]), collapse = ", " ),
        "\nnormalising to 0.\n"
    ))
    lambda = pmax(rep(0,n), lambda)
  }
  U = K_eigen$vectors
  Xtilde = diag(1L/sqrt(f)) %*% U %*% diag(sqrt(lambda))
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
#' @example
#' D <- as.matrix(eurodist)[1:5, 1:5]
#' f <- rep(1/5, 5)
#' mds <- weighted_mds(D, f)
#' plot(mds, c(1,2), c(1,1,3,3,4))
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
  point_labels <- rownames(Xtilde)
  if (is.null(point_labels)) point_labels <- as.character(seq(nrow(Xtilde)))
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
    if ( !is.null(Lst$point_labels) ) {
      point_labels <- Lst$point_labels
    }
  }
  # check for null values
  if ( is.null(x_labels) ) {
    x_labels <- c(1:ncol(Xtilde))
  }
  if ( is.null(y_labels) ) {
    y_labels <- c(1:nrow(Xtilde))
  }


  par(mar=c(5, 4, 4, 2) + 0.1, oma = c(6,0,0,0), pty="s")
  a <- 0.5
  b <- 2
  cexf <- ((sqrt(weights) - min(sqrt(weights)))/(max(sqrt(weights)) - min(sqrt(weights)))*(b - a) + a)
  cexl <- rep(0.5, length(cexf))
  x_max <- max(-Xtilde[, d1])
  y_max <- max(Xtilde[, d2])
  x_min <- min(-Xtilde[, d1])
  y_min <- min(Xtilde[, d2])

  plot(
    -Xtilde[, d1],
    Xtilde[, d2],
    cex = cexf,
    main = title,
    xlab = x_labels,
    ylab = y_labels,
    cex.axis = .7,
    cex.lab = .7,
    col = colors[group_by],
    xlim = c(-.1 * (x_max - x_min) + x_min, .1 * (x_max - x_min) + x_max),
    ylim = c(-.1 * (y_max - y_min) + y_min, .1 * (y_max - y_min) + y_max)
  )

  wordcloud::textplot(
    -Xtilde[, d1],
    Xtilde[, d2],
    point_labels,
    show.lines = TRUE,
    new = FALSE,
    cex = cexl,
    col = rgb(.11, .11, .11, .33),
    xlim = c(-.1 * (x_max - x_min) + x_min, .1 * (x_max - x_min) + x_max),
    ylim = c(-.1 * (y_max - y_min) + y_min, .1 * (y_max - y_min) + y_max)

  )
  abline(h = 0)
  abline(v = 0)
  par(mar=c(0,0,0,0), oma=c(0,0,10,0), pty="m")
  if (length(unique(group_by)) < 10) {
    legend(
      "bottom",
      legend = stringr::str_sort(unique(group_by), numeric = TRUE),
      col = colors[stringr::str_sort(unique(group_by), numeric = TRUE)],
      pch = 2,
      horiz = TRUE,
      cex = .5,
      bty = "o",
      xpd = TRUE
    )
  } else {
    legend(
      "bottom",
      legend = str_sort(unique(group_by), numeric = TRUE),
      col = colors[str_sort(unique(group_by), numeric = TRUE)],
      pch = 2,
      ncol = 5,
      cex = .5,
      bty = "o",
      xpd = TRUE
    )
  }
  layout(1)
}


