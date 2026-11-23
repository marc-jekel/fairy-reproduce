# Reproduces Tables E3 and E5 of Online Supplement E (without the frequentist p-values): order-constrained inference for the hypothetical cringyness data
# (Mike's data, Marc's replication with 2.5 times the sample size, and Meichai's data), Models 1 to 10.
# Bayesian p-values (Bp) and Bayes factors (BF, against the unconstrained model) come from the R package multinomineq,
# run through fairy_analyze_multinomineq(). Frequentist p-values (Fp) are computed by the software QTest and are not part of this script.
# Needs the R packages fairy, multinomineq and coda; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_E3_E5_DataAnalysis.R   (writes Tbl_E3_E5_DataAnalysis.pdf; 30 to 80 seconds)
source("_models.R"); source("_table_pdf.R")


# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
# the hypothetical data (Mike's, Marc's and Meichai's; Tables E2 and E4) are defined in _models.R: data_sets

settings <- list(bf_samples = 1e6, seed = 2026)     # more samples = more precise Bayes factors

analyse <- function(dat) {
  # one call per model (the models are H-representations, Models 7 and 8 with approximate equalities, Model 10 the mixture)
  res <- lapply(names(m), function(nm) {
    for (seed in settings$seed + 0:4) {      # the posterior sampler sometimes cannot start: then try another seed
      r <- fairy_analyze_multinomineq(setNames(list(m[[nm]]), nm), data = dat, n_p = 6,
                                      settings = modifyList(settings, list(seed = seed)))$results
      if (is.finite(r$ppp)) break      # Model 6 has exact equalities: the sampler often cannot start, and for Marc's data it never does (shown as N/A)
    }
    data.frame(model = nm, Bp = r$ppp, BF = r$bf_0u)
  })
  do.call(rbind, res)
}
res <- lapply(data_sets, analyse)
print(res)       # Bayesian p-value (Bp) and Bayes factor (BF) for every model and data set


# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
fmt_p  <- function(x) ifelse(is.na(x), "N/A", ifelse(x < 0.001, "<0.001", formatC(x, digits = 2, format = "fg")))
# a Bayes factor of 0 means "smaller than the sampler can resolve" (1 / number of samples)
fmt_bf <- function(x) ifelse(x < 1 / settings$bf_samples, paste0("<", formatC(1 / settings$bf_samples, digits = 1, format = "e")),
                      ifelse(x < 1e-3, formatC(x, digits = 2, format = "e"), ifelse(x >= 100, formatC(x, digits = 3, format = "fg"), formatC(x, digits = 2, format = "fg"))))
tab <- do.call(cbind, lapply(names(res), function(d) {
  out <- cbind(fmt_p(res[[d]]$Bp), fmt_bf(res[[d]]$BF)); colnames(out) <- paste(d, c("Bp", "BF")); out }))
rownames(tab) <- sub("M", "Model ", names(m))
table_pdf(tab, "Tbl_E3_E5_DataAnalysis.pdf", "Order-constrained inference for the hypothetical data (Bayesian p and Bayes factor)", cex = 0.6,
          note = "Bp: Bayesian p-value; BF: Bayes factor against the unconstrained model. Frequentist p-values (Fp) come from QTest and are not shown.")
