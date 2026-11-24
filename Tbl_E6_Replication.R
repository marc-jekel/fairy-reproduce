# Reproduces Table E6 of Online Supplement E (without the frequentist p-values), the replication analysis: Models 4, 9 and 10, each as identical, joint and substitutable
# replication model, fitted to the three hypothetical data sets jointly (Mike's, Marc's and Meichai's data = three labs, 18 probabilities).
# Bayesian p-values (Bp) and Bayes factors (BF, against the unconstrained model) come from multinomineq via fairy_analyze_multinomineq().
# Frequentist p-values (Fp) are computed by QTest and are not part of this script.
# Needs the R packages fairy, multinomineq and coda; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_E6_Replication.R   (writes Tbl_E6_Replication.pdf; a few minutes)
# What is computed here and what is not:
#  * identical: fitted directly (6 free probabilities).
#  * joint: every lab has its own probabilities, so the labs are independent and the Bayes factor is the product of the three per-lab Bayes factors.
#    This is fast. The Bayesian p-value of the joint model needs the 18-dimensional posterior and is "not computed" unless compute_direct <- TRUE.
#  * substitutable: the 18-dimensional model with many constraints (Model 10: 4,032) cannot be handled quickly: these cells read "not computed"
#    unless compute_direct <- TRUE. TRUE fits the joint and substitutable models directly in 18 dimensions (expect many hours, and the
#    Bayes-factor estimate may fail for the largest models).
compute_direct <- FALSE
source("_models.R"); source("_table_pdf.R")

labs <- list(c(60, 64, 68, 60, 50, 38) , c(150, 160, 170, 150, 125, 95), c(44, 40, 36, 50, 60, 70))
n    <- c(100, 250, 100)                                       # observations per cell in the three labs
dat  <- data.frame(probability = paste0("p", 1:18), n = rep(n, each = 6), frequency = unlist(labs))

settings <- list(bf_samples = 1e6, seed = 2026)
fmt_p  <- function(x) ifelse(is.na(x), "not computed", ifelse(x < 0.001, "<0.001", formatC(x, digits = 2, format = "fg")))
fmt_bf <- function(x) ifelse(is.na(x), "not computed", ifelse(x < 1 / settings$bf_samples, paste0("<", formatC(1 / settings$bf_samples, digits = 1, format = "e")),
                      ifelse(x < 1e-3, formatC(x, digits = 2, format = "e"), formatC(x, digits = 3, format = "fg"))))

fit <- function(h) {
  for (seed in settings$seed + 0:4) {                # the posterior sampler sometimes cannot start: then try another seed
    r <- fairy_analyze_multinomineq(list(model = h), data = dat, n_p = 18, settings = modifyList(settings, list(seed = seed)))$results
    if (is.finite(r$ppp)) break
  }
  c(Bp = r$ppp, BF = r$bf_0u)
}

# one lab at a time (6 probabilities), for the Bayes factor of the joint model
fit_lab <- function(h, k) {
  d <- dat[(6 * (k - 1) + 1):(6 * k), ]; d$probability <- paste0("p", 1:6)
  for (seed in settings$seed + 0:4) {
    r <- fairy_analyze_multinomineq(list(model = h), data = d, n_p = 6, settings = modifyList(settings, list(seed = seed)))$results
    if (is.finite(r$ppp)) break
  }
  c(Bp = r$ppp, BF = r$bf_0u)
}

rows <- list()
for (nm in c("M4", "M9", "M10")) for (type in c("identical", "joint", "substitutable")) {
  message(nm, " ", type); t0 <- Sys.time()
  out <- c(Bp = NA_real_, BF = NA_real_)
  if (type == "identical") out <- fit(fairy_replicate(m[[nm]], 3, "identical"))
  else if (type == "joint") {
    out["BF"] <- prod(vapply(1:3, function(k) fit_lab(m[[nm]], k)[["BF"]], numeric(1)))   # independent labs: the Bayes factors multiply
    if (compute_direct) out["Bp"] <- fit(fairy_replicate(m[[nm]], 3, "joint"))[["Bp"]]
  } else if (compute_direct) out <- fit(fairy_replicate(m[[nm]], 3, "substitutable"))
  message("   ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
  rows[[length(rows) + 1]] <- c(model = sub("M", "Model ", nm), type = type, setNames(out, c("Bp", "BF")))
}
res <- do.call(rbind, lapply(rows, function(r) data.frame(model = r[[1]], type = r[[2]], Bp = as.numeric(r[[3]]), BF = as.numeric(r[[4]]))))
print(res)
tab <- data.frame(Model = res$model, `Replication type` = res$type, Bp = fmt_p(res$Bp), BF = fmt_bf(res$BF), check.names = FALSE)
table_pdf(tab, "Tbl_E6_Replication.pdf", "Order-constrained inference for the replication models (all three data sets jointly)", row_names = FALSE, cex = 0.7,
          note = "Bp: Bayesian p-value; BF: Bayes factor against the unconstrained model. Joint: BF = product of the three labs.\nnot computed: needs the 18-dimensional model (see compute_direct in the script). Frequentist p (Fp): QTest.")
