# Replication across labs where not every lab has every probability.

# Project an H-representation onto some of its probabilities (the others are integrated out: the shadow of the polytope).
# Exact: Fourier-Motzkin elimination on the H-representation with rational arithmetic (no vertices needed);
# redundant rows are removed after every elimination step.
project_h <- function(h, keep) {
  n <- ncol(h) - 2L
  keep <- sort(unique(keep))
  if (identical(as.integer(keep), seq_len(n))) return(h)
  nm <- colnames(h)
  as_q <- gmp::as.bigq
  rows <- list()                                       # b + a.p >= 0, as (b, a1, ..., an)
  for (i in seq_len(nrow(h))) {
    v <- as_q(as.character(h[i, -1]))
    rows[[length(rows) + 1L]] <- v
    if (as.character(h[i, 1]) == "1") rows[[length(rows) + 1L]] <- -v   # equality = two inequalities
  }
  mat <- function(R) cbind("0", t(vapply(R, function(v) as.character(v), character(length(R[[1]])))))
  tidy <- function(R) {
    out <- list(); seen <- character()
    for (v in R) {
      m <- max(abs(v)); if (m == 0) next
      v <- v / m; k <- paste(as.character(v), collapse = ",")
      if (!(k %in% seen)) { seen <- c(seen, k); out[[length(out) + 1L]] <- v }
    }
    out
  }
  prune <- function(R) {
    if (length(R) < 2L) return(R)
    rr <- redundant(mat(R))$output
    lapply(seq_len(nrow(rr)), function(i) as_q(as.character(rr[i, -1])))
  }
  rows <- tidy(rows)
  for (j in setdiff(seq_len(n), keep)) {
    s <- vapply(rows, function(v) sign(as.numeric(v[1L + j])), numeric(1))
    new <- rows[s == 0]
    for (p in rows[s > 0]) for (q in rows[s < 0]) new[[length(new) + 1L]] <- p * (-q[1L + j]) + q * p[1L + j]
    rows <- prune(tidy(new))
  }
  if (length(rows) == 0) stop("The model has no allowed pattern, so it cannot be projected.")
  H <- redundant(mat(lapply(rows, function(v) v[c(1L, 1L + keep)])))$output
  # an opposite pair of inequalities is an equality
  q <- lapply(seq_len(nrow(H)), function(i) as_q(as.character(H[i, -1])))
  used <- rep(FALSE, nrow(H)); out <- NULL
  for (i in seq_len(nrow(H))) {
    if (used[i]) next
    used[i] <- TRUE; typ <- "0"
    for (k in seq_len(nrow(H))) {
      if (!used[k] && all(as.character(q[[k]]) == as.character(-q[[i]]))) { used[k] <- TRUE; typ <- "1"; break }
    }
    out <- rbind(out, c(typ, as.character(H[i, -1])))
  }
  attr(out, "representation") <- "H"
  colnames(out) <- c("", "", nm[2L + keep])
  out
}

#' Replicate a model across labs, with probabilities missing in some labs
#'
#' The generalisation of [fairy_replicate()]: the labs do not all have to
#' contain every probability. `present` is a table with one row per lab and
#' one column per probability: `TRUE` if the lab has this probability.
#'
#' A lab that lacks a probability follows the model *projected* onto the
#' probabilities it has (for `p1 > p2 > p3` and a lab without p2 this is
#' `p1 > p3`, which follows from the model).
#'
#' * `"joint"`: every lab follows the model, with no link between the labs.
#'   The volume is the product of the volumes of the labs.
#' * `"substitutable"`: the order of the model holds whichever lab's
#'   probability is plugged in for each term (only where the lab has it).
#' * `"identical"`: a probability is the same in all labs that have it (equalities
#'   between its copies, so the volume is zero when two labs share a probability),
#'   or, with `approx_tol > 0`, differs between them by at most that much.
#' * `"average"`: the model holds for the averages over the labs that have a
#'   probability; the model has one parameter per lab and probability.
#'
#' @param h A model (string with `n_p`, or H-representation).
#' @param present A logical matrix with one row per lab and one column per
#'   probability, or a whole number: the number of labs, all with every
#'   probability.
#' @param type `"joint"`, `"substitutable"`, `"identical"` or `"average"`.
#' @param labs Optional lab names.
#' @param n_p Number of probabilities for a model string.
#' @param reduce Remove redundant constraints? Default `TRUE`.
#' @param approx_tol For `"identical"`: the labs may differ by up to this much (0 = exactly equal). The model holds in every lab, and any two labs differ by at most this much.
#' @param all_cells Keep a parameter for every combination of lab and
#'   probability, also where a lab does not have the probability? Such a
#'   parameter is free (only between 0 and 1), which changes neither the volume
#'   nor the largest possible Bayes factor. The app does this, so that all
#'   replication models of the same size live in the same space. Default `FALSE`.
#'
#' @return An H-representation. The parameters are ordered lab by lab (only the
#'   probabilities a lab has, unless `all_cells = TRUE`) and are named
#'   `p^(lab)`.
#'
#' @examples
#' m <- fairy_h("p1 > p2 > p3", n_p = 3)
#' present <- rbind(c(TRUE, TRUE, TRUE), c(TRUE, FALSE, TRUE))   # lab 2 has no p2
#' fairy_info(fairy_replicate_labs(m, present, "joint"))
#' @seealso [fairy_replicate()] when every lab has every probability,
#'   [fairy_replicate_items()] for replication across items.
#' @importFrom rcdd d2q q2d redundant scdd makeV
#' @export
fairy_replicate_labs <- function(h, present, type = c("joint", "substitutable", "identical", "average"),
                                 labs = NULL, n_p = NULL, reduce = TRUE, approx_tol = 0,
                                 all_cells = FALSE) {
  type <- match.arg(type)
  h <- resolve_h(h, n_p = n_p)
  n <- ncol(h) - 2L
  if (length(present) == 1L && is.numeric(present)) present <- matrix(TRUE, as.integer(present), n)
  present <- as.matrix(present); storage.mode(present) <- "logical"
  if (ncol(present) != n) stop("'present' needs one column per probability (", n, ").")
  r <- nrow(present)
  if (r < 1L) stop("There must be at least one lab.")
  if (any(rowSums(present) == 0L)) stop("Every lab needs at least one probability.")
  if (is.null(labs)) labs <- seq_len(r)
  base_names <- colnames(h)[-(1:2)]
  if (is.null(base_names)) base_names <- paste0("p", seq_len(n))
  used <- which(colSums(present) > 0L)

  # one parameter for every (lab, probability) that exists, lab by lab
  idx <- matrix(NA_integer_, r, n); k <- 0L
  for (l in seq_len(r)) for (i in which(present[l, ])) { k <- k + 1L; idx[l, i] <- k }
  nn <- k
  cell_names <- character(nn)
  for (l in seq_len(r)) for (i in which(present[l, ])) cell_names[idx[l, i]] <- paste0(base_names[i], "^(", labs[l], ")")

  rows <- list(); add <- function(t_, b_, a_) rows[[length(rows) + 1L]] <<- c(t_, b_, a_)
  finish <- function(rows, names_) {
    m <- unique(do.call(rbind, rows))
    hh <- d2q(m); colnames(hh) <- c("", "", names_)
    if (reduce && nrow(hh) > 1L) { rr <- redundant(hh)$output; colnames(rr) <- c("", "", names_); hh <- rr }
    if (all_cells && nn < r * n) hh <- all_lab_cells(hh, present, base_names, labs)
    hh
  }

  if (type == "identical" && approx_tol <= 0) {
    # exactly identical: the model for the first copy of every probability, and every other copy equal to it
    first <- vapply(used, function(i) which(present[, i])[1L], integer(1))
    hn <- q2d(project_h(h, used))
    for (k in seq_len(nrow(hn))) {
      a <- numeric(nn); a[idx[cbind(first, used)]] <- hn[k, -(1:2)]; add(hn[k, 1], hn[k, 2], a)
    }
    for (jj in seq_along(used)) for (l in setdiff(which(present[, used[jj]]), first[jj])) {
      a <- numeric(nn); a[idx[first[jj], used[jj]]] <- 1; a[idx[l, used[jj]]] <- -1; add(1, 0, a)
    }
    reduce <- FALSE    # the rows are already minimal up to the trivial bounds of the copies
    return(finish(rows, cell_names))
  }

  if (type == "identical") {
    # approximately identical: the model holds in every lab (no lab is the reference); any two labs differ by at most approx_tol
    for (l in seq_len(r)) {
      S <- which(present[l, ]); hn <- q2d(project_h(h, S))
      for (k in seq_len(nrow(hn))) { a <- numeric(nn); a[idx[l, S]] <- hn[k, -(1:2)]; add(hn[k, 1], hn[k, 2], a) }
    }
    for (i in used) { ls <- which(present[, i]); if (length(ls) < 2L) next
      for (a_ in seq_len(length(ls) - 1L)) for (b_ in (a_ + 1L):length(ls)) for (sg in c(1, -1)) {
        a <- numeric(nn); a[idx[ls[a_], i]] <- sg; a[idx[ls[b_], i]] <- -sg; add(0, approx_tol, a)
      }
    }
    return(finish(rows, cell_names))
  }

  proj <- lapply(seq_len(r), function(l) project_h(h, which(present[l, ])))

  if (type == "joint") {
    for (l in seq_len(r)) {
      S <- which(present[l, ]); hn <- q2d(proj[[l]])
      for (i in seq_len(nrow(hn))) { a <- numeric(nn); a[idx[l, S]] <- hn[i, -(1:2)]; add(hn[i, 1], hn[i, 2], a) }
    }
    hh <- finish(rows, cell_names)
    if (!(all_cells && nn < r * n)) attr(hh, "fairy_joint") <- list(hs = proj)
    return(hh)
  }

  if (type == "substitutable") {
    pool <- list()
    for (l in seq_len(r)) {
      S <- which(present[l, ]); hn <- q2d(proj[[l]])
      for (i in seq_len(nrow(hn))) {
        a <- numeric(n); a[S] <- hn[i, -(1:2)]
        pool[[paste(hn[i, 1], hn[i, 2], paste(a, collapse = ","))]] <- c(hn[i, 1], hn[i, 2], a)
      }
    }
    if (any(vapply(pool, function(x) x[1] == 1, logical(1)))) stop("Substitutable replication needs a model without equality constraints (use approx_tol to relax equalities).")
    for (rw in pool) {
      b_ <- rw[2]; a_ <- rw[-(1:2)]; S <- which(a_ != 0)
      choices <- lapply(S, function(i) which(present[, i]))
      if (any(lengths(choices) == 0L)) next
      combos <- as.matrix(expand.grid(choices, KEEP.OUT.ATTRS = FALSE))
      for (kk in seq_len(nrow(combos))) {
        a <- numeric(nn); a[idx[cbind(combos[kk, ], S)]] <- a_[S]; add(0, b_, a)
      }
    }
    return(finish(rows, cell_names))
  }

  # average: the model holds for the average of each probability over the labs that have it
  hU <- q2d(project_h(h, used)); cnt <- colSums(present)
  for (i in seq_len(nrow(hU))) {
    a <- numeric(nn)
    for (jj in seq_along(used)) { j <- used[jj]; cf <- hU[i, 2L + jj]; if (cf != 0) a[idx[present[, j], j]] <- cf / cnt[j] }
    add(hU[i, 1], hU[i, 2], a)
  }
  for (c_ in seq_len(nn)) { a <- numeric(nn); a[c_] <- 1; add(0, 0, a); a[c_] <- -1; add(0, 1, a) }
  finish(rows, cell_names)
}

# add a free parameter (0 <= p <= 1) for every lab x probability that does not exist; columns lab by lab
all_lab_cells <- function(h, present, base_names, labs) {
  r <- nrow(present); n <- ncol(present)
  full <- as.vector(vapply(seq_len(r), function(l) paste0(base_names, "^(", labs[l], ")"), character(n)))
  have <- as.vector(t(present))
  out <- matrix("0", nrow(h), 2L + r * n)
  out[, 1:2] <- h[, 1:2]
  out[, 2L + which(have)] <- h[, -(1:2)]
  free <- which(!have)
  bounds <- matrix("0", 2L * length(free), 2L + r * n)
  for (k in seq_along(free)) {
    bounds[2L * k - 1L, c(2L, 2L + free[k])] <- c("1", "-1")      # p <= 1
    bounds[2L * k, 2L + free[k]] <- "1"                           # p >= 0
  }
  out <- rbind(out, bounds)
  attr(out, "representation") <- "H"
  colnames(out) <- c("", "", full)
  out
}

#' Replicate a model across items
#'
#' Several items (trials, stimuli, conditions, repetitions) per probability,
#' where every probability can have its own number of items. Item `k` of one
#' probability goes with item `k` of the others; a probability with fewer items
#' is missing in the later ones, where the model is projected onto the rest.
#' This is [fairy_replicate_labs()] with the items in the place of the labs.
#' There is no `"joint"` type, because items are not independent of each other.
#'
#' * `"substitutable"`: the order of the model holds whichever item is plugged
#'   in for each term.
#' * `"identical"`: a probability has the same value in all its items (or
#'   differs between them by at most `approx_tol`).
#' * `"average"`: the model holds for each probability's average over its items.
#'
#' That item `k` of one probability means the same thing as item `k` of another
#' is something you decide.
#'
#' @inheritParams fairy_replicate_labs
#' @param items Number of items: one number for all probabilities, or one per
#'   probability.
#' @param type `"substitutable"`, `"identical"` or `"average"`.
#' @param all_cells Keep a (free) parameter for every item and probability, also
#'   where a probability does not have that item? Default `FALSE`.
#'
#' @return An H-representation. The parameters are ordered item by item and are
#'   named `p^(item)`.
#'
#' @examples
#' # p1 and p2 have two items, p3 has three
#' h <- fairy_replicate_items("p1 > p2 > p3", items = c(2, 2, 3), n_p = 3)
#' colnames(h)[-(1:2)]
#' fairy_info(h)
#' @export
fairy_replicate_items <- function(h, items, type = c("substitutable", "identical", "average"),
                                  n_p = NULL, reduce = TRUE, approx_tol = 0, all_cells = FALSE) {
  type <- match.arg(type)
  h <- resolve_h(h, n_p = n_p)
  n <- ncol(h) - 2L
  if (length(items) == 1L) items <- rep(items, n)
  if (length(items) != n) stop("`items` needs one number, or one per probability (", n, ").")
  if (anyNA(items) || any(items < 1 | items != round(items))) stop("`items` must be positive whole numbers.")
  present <- outer(seq_len(max(items)), as.integer(items), "<=")
  fairy_replicate_labs(h, present, type, reduce = reduce, approx_tol = approx_tol, all_cells = all_cells)
}
