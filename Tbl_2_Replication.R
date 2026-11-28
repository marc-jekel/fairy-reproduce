# Reproduces Table 2 of the paper: replication models (identical, joint, substitutable) for r = 3 labs.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_2_Replication.R   (writes Tbl_2_Replication.pdf)
# Note: the constraints of the replication models are written out directly (reduce = FALSE); no redundant constraints arise in these models.
source("_models.R"); source("_table_pdf.R")

list_all_vertices_model_10 <- FALSE   # Model 10 (the mixture of Models 4 and 9) is always in the table. Its substitutable version (18 dimensions,
                                      # 4,032 constraints; the paper reports 46,740 extreme cases) is too large for a quick run: listing the extreme cases
                                      # takes many hours and the Bayes-factor bound is a long random walk, so these two cells read "not computed".
                                      # TRUE computes them too (expect more than a day of computing time).


# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
set.seed(2026)                                   # the volume of the 18-dimensional substitutable model is a random-walk estimate (volesti)

rep_table <- function(h, r = 3, skip_vertices = FALSE) {
  ident <- fairy_info(fairy_replicate(h, r, "identical", reduce = FALSE), volume = FALSE)
  joint <- fairy_info(fairy_replicate(h, r, "joint", reduce = FALSE), method = "exact")
  sub_h <- fairy_replicate(h, r, "substitutable", reduce = FALSE)
  if (skip_vertices) {                           # same numbers as fairy_info(), but without listing the extreme cases
    subst <- data.frame(dim = ncol(sub_h) - 2 - sum(sub_h[, 1] == 1), vertices = NA_real_, inequalities = sum(sub_h[, 1] == 0),
                        equalities = sum(sub_h[, 1] == 1), volume = NA_real_, max_bf = NA_real_)
  } else subst <- fairy_info(sub_h, volume = FALSE)
  if (!skip_vertices) subst$max_bf <- fairy_max_bf(sub_h, method = "volesti", settings = list(error = 1e-4))   # volesti estimate with a small error setting (default 0.1). Models 4 and 9: about 1.36e11 to 1.38e11 depending on the seed, about 1 s;
                                                 # the paper reports 1.37e11. Model 10: very long (a test on 24 November 2026 had no result after 20 minutes).
  ident$max_bf <- NA_real_                       # volume zero: no upper bound
  out <- rbind(identical = ident, joint = joint, substitutable = subst)
  out[, c("dim", "vertices", "inequalities", "equalities", "max_bf")]
}

t4 <- do.call(rbind, lapply(c("M4", "M9", "M10"), function(nm)
  cbind(model = sub("M", "Model ", nm), rep_table(m[[nm]], skip_vertices = nm == "M10" && !list_all_vertices_model_10), type = c("identical", "joint", "substitutable"))))
print(t4)

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
pdf_tab <- data.frame(Model = t4$model, Version = t4$type, Dimension = t4$dim, Vertices = ifelse(is.na(t4$vertices), "not computed", format(t4$vertices, big.mark = ",")), Inequalities = format(t4$inequalities, big.mark = ","),
                      Equalities = t4$equalities, `Max Bayes factor` = ifelse(is.na(t4$max_bf), ifelse(t4$type == "identical", "none (volume 0)", "not computed"), format(signif(t4$max_bf, 3), scientific = TRUE)), check.names = FALSE)
table_pdf(pdf_tab, "Tbl_2_Replication.pdf", "Table 2: replication models, r = 3 labs", row_names = FALSE, cex = 0.7)
