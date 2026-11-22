# Reproduces Tables E2 and E4 of Online Supplement E: the hypothetical data of the cringyness study
# (Mike's data and Marc's replication with 2.5 times the sample size: Table E2; Meichai's data: Table E4).
# These tables are data, not results: nothing is computed by the fairy. The same data (data_sets in _models.R) are the input of Tbl_E3_E5_DataAnalysis.R.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_E2_E4_Data.R   (writes Tbl_E2_Data.pdf and Tbl_E4_Data.pdf)
source("_models.R"); source("_table_pdf.R")

# ===== PART 1: the data. Frequencies of "cringe" out of n per cell (order: illogical jokes, then puns; children, teens, adults). =====
mike <- data_sets[["Mike's data"]]; marc <- data_sets[["Marc's data"]]; meichai <- data_sets[["Meichai's data"]]
print(mike); print(marc); print(meichai)

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
rows <- c("Illogical: children", "Illogical: teens", "Illogical: adults", "Puns: children", "Puns: teens", "Puns: adults")
one <- function(d) { n <- d$n; cbind(d$frequency, n - d$frequency) }          # cringe, no cringe
col <- function(d) sprintf("%s (n = %d)", c("Cringe", "No cringe"), d$n[1])
e2 <- cbind(one(mike), one(marc)); colnames(e2) <- c(paste("Mike:", col(mike)), paste("Marc:", col(marc))); rownames(e2) <- rows
e4 <- one(meichai); colnames(e4) <- paste("Meichai:", col(meichai)); rownames(e4) <- rows
print(e2); print(e4)
table_pdf(e2, "Tbl_E2_Data.pdf", "Table E2: Mike's made-up data and Marc's 'perfect' replication with 2.5 times the sample size", cex = 0.8, pad = 0.5)
table_pdf(e4, "Tbl_E4_Data.pdf", "Table E4: Meichai's made-up replication data", cex = 0.8, pad = 0.5)
