# Reproduces Table 4 of the paper: replication models (identical, joint, substitutable) for r = 3 labs.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_4_Replication.R   (writes Tbl_4_Replication.pdf)
# Note: the constraints of the replication models are written out directly (reduce = FALSE); no redundant constraints arise in these models.
source("_models.R"); source("_table_pdf.R")

include_model_10 <- FALSE    # TRUE adds Model 10 (the mixture of Models 4 and 9). Building and analysing it is fast, but listing the extreme cases of its
                             # substitutable version (18 dimensions, 4,032 constraints, 46,739 extreme cases) takes many hours


# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
set.seed(2026)                                   # the volume of the 18-dimensional substitutable model is a random-walk estimate (volesti)

rep_table <- function(h, r = 3) {
  ident <- fairy_info(fairy_replicate(h, r, "identical", reduce = FALSE), volume = FALSE)
  joint <- fairy_info(fairy_replicate(h, r, "joint", reduce = FALSE), method = "exact")
  sub_h <- fairy_replicate(h, r, "substitutable", reduce = FALSE)
  subst <- fairy_info(sub_h, volume = FALSE)
  subst$max_bf <- fairy_max_bf(sub_h, method = "volesti")     # an estimate (about 1.3e11 to 1.5e11 depending on the seed); the paper reports 1.37e11
  ident$max_bf <- NA_real_                       # volume zero: no upper bound
  out <- rbind(identical = ident, joint = joint, substitutable = subst)
  out[, c("dim", "vertices", "inequalities", "equalities", "max_bf")]
}

t4 <- do.call(rbind, lapply(c("M4", "M9", if (include_model_10) "M10"), function(nm)
  cbind(model = sub("M", "Model ", nm), rep_table(m[[nm]]), type = c("identical", "joint", "substitutable"))))
print(t4)

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
pdf_tab <- data.frame(Model = t4$model, Version = t4$type, Dimension = t4$dim, Vertices = t4$vertices, Inequalities = t4$inequalities,
                      Equalities = t4$equalities, `Max Bayes factor` = ifelse(is.na(t4$max_bf), "none (volume 0)", format(signif(t4$max_bf, 3), scientific = TRUE)), check.names = FALSE)
table_pdf(pdf_tab, "Tbl_4_Replication.pdf", "Table 4: replication models, r = 3 labs", row_names = FALSE, cex = 0.7)
