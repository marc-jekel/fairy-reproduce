# Draws a table (a character matrix or data frame) as a PDF page, so that it can be opened and looked at directly.
table_pdf <- function(x, file, title = NULL, row_names = TRUE, cex = 0.8, note = NULL, header_expr = NULL, pad = 0.18) {
  # header_expr: optional list of plotmath expressions (one per column, NULL = use the column name), e.g. for P with sub- and superscripts
  x <- as.matrix(x); mode(x) <- "character"; x[is.na(x)] <- ""
  if (row_names && !is.null(rownames(x))) { x <- cbind(` ` = rownames(x), x) }
  nr <- nrow(x); nc <- ncol(x)
  cn <- colnames(x); if (is.null(cn)) cn <- rep("", nc)
  # column widths from the widest entry (in inches)
  grDevices::cairo_pdf(file, width = 6, height = 4)       # only for measuring
  op <- par(family = "serif"); w <- sapply(seq_len(nc), function(j) max(strwidth(c(cn[j], x[, j]), units = "inches", cex = cex))) + pad
  # the page is at least as wide as the title and the note
  tt <- max(if (is.null(title)) 0 else strwidth(title, units = "inches", cex = cex * 1.15, font = 2),
            if (is.null(note)) 0 else strwidth(note, units = "inches", cex = cex * 0.85))
  dev.off()
  h <- 0.27; tw <- sum(w); th <- (nr + 1) * h + (if (is.null(title)) 0 else 0.45) + (if (is.null(note)) 0 else 0.4) + 0.3
  pw <- max(tw, tt) + 0.4
  grDevices::cairo_pdf(file, width = pw, height = th)
  on.exit(dev.off())
  par(mar = c(0, 0, 0, 0), family = "serif"); plot.new()
  plot.window(xlim = c(0, pw), ylim = c(th, 0))
  y0 <- 0.2
  if (!is.null(title)) { text(0.2, y0 + 0.12, title, adj = c(0, 0.5), font = 2, cex = cex * 1.15); y0 <- y0 + 0.45 }
  xs <- 0.2 + c(0, cumsum(w))
  segments(0.2, y0, tw + 0.2, y0); segments(0.2, y0 + h, tw + 0.2, y0 + h)
  for (j in seq_len(nc)) text(xs[j] + w[j] / 2, y0 + h / 2, if (!is.null(header_expr) && !is.null(header_expr[[j]])) header_expr[[j]] else cn[j], font = 2, cex = cex)
  for (i in seq_len(nr)) {
    yc <- y0 + h * i + h / 2
    for (j in seq_len(nc)) text(xs[j] + w[j] / 2, yc, x[i, j], cex = cex)
  }
  yb <- y0 + h * (nr + 1); segments(0.2, yb, tw + 0.2, yb)
  if (!is.null(note)) text(0.2, yb + 0.22, note, adj = c(0, 0.5), cex = cex * 0.85)
  message("written: ", file)
}
