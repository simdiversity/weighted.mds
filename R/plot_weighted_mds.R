library(wordcloud)

plot.mds <- function(Xtilde, lambda, dimentions, weights, group_by, ...) {
  d1 = dimentions[1]
  d2 = dimentions[2]
  contributions = round(lambda/sum(lambda), 4) * 100
  y_labels <- paste("Dimention", d1, "(", contributions[d1], "% )")
  x_labels <- paste("Dimention", d2, "(", contributions[d2], "% )")
  labels <- rownames(Xtilde)
  title <- c()
  colors <- rainbow(length(unique(group_by)))

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
  par(mar = c(10.1, 4.1, 4.1, 0))
  # construction of a cex scale such that min(weights) = a and max(weights) = b
  a = 0.5
  b = 6
  cexf = (sqrt(weights) - min(sqrt(weights)))/(max(sqrt(weights)) - min(sqrt(weights)))*(b - a) + a
  plot(
    -Xtilde[, d1],
    Xtilde[, d2],
    cex = (cexf/3),
    main = title,
    xlab = x_labels,
    ylab = y_labels,
    cex.axis = 1.1,
    cex.lab = 1.1,
    col = colors
  )
  text(
    -Xtilde[, d1],
    Xtilde[, d2] + 0.002,
    labels = labels,
    cex = .5
  )

  legend(
    "bottom",
    legend = unique(group_by),
    col = colors,
    pch = 1,
    xpd = TRUE,
    horiz = TRUE,
    inset = c(0, -.08),
    bty = "n",
    cex = .8
  )

  abline(h = 0)
  abline(v = 0)
  layout(1)
}
