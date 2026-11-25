# Reproduces Table 1 of the paper: relationships between Models 1 to 10, parsimony, and upper bounds on Bayes factors.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_1_Relationships.R   (writes Tbl_1_Relationships.pdf; takes a few seconds)
source("_models.R"); source("_table_pdf.R")


# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
cmp <- fairy_compare(m, method = "exact")     # exact volumes (the package default is a faster estimate)
print(cmp)

# diagonal: volume of each model; above: volume of the overlap; below: nested (subset sign / superset sign) or dimension of the overlap


# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
# the table in the layout of the paper: diagonal and above = volume (overlap), below = nested (subset/superset sign) or dimension of the overlap
k <- nrow(cmp$volume)
fmt <- function(z) if (is.na(z)) "" else if (z == 0) "0" else if (z < 0.001) formatC(z, digits = 2, format = "e") else formatC(z, digits = 2, format = "fg")
tab <- matrix("", k + 1L, k, dimnames = list(c(paste("Model", seq_len(k)), "max BF"), seq_len(k)))
for (i in seq_len(k)) for (j in seq_len(k)) tab[i, j] <- if (i <= j) fmt(cmp$volume[i, j]) else cmp$relation[i, j]
tab[tab == "subset"] <- "\u2282"; tab[tab == "superset"] <- "\u2283"
tab[k + 1L, ] <- ifelse(is.infinite(cmp$max_bf), "Inf", formatC(cmp$max_bf, digits = 3, format = "fg"))
table_pdf(tab, "Tbl_1_Relationships.pdf", "Table 1: model relationships, parsimony, and upper bounds on Bayes factors", cex = 0.6,
          note = "Diagonal: volume of the model. Above: volume of the overlap. Below: subset / superset sign (nested) or dimension of the overlap.")
