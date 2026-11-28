# Reproduces every figure and table of the paper and of the online supplement that the fairy produces: one command, all scripts in turn.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript run_all.R          (all scripts; one to two minutes)
#       Rscript run_all.R quick    (skips the slow data analysis of Tables E3 and E5)
# Some very large models are "not computed" by default. To compute them too (many hours), set the switch at the top of the script to TRUE:
#       list_all_vertices_model_10 <- TRUE   in Tbl_2_Replication.R   (Model 10, substitutable version, Table 2)
#       compute_direct <- TRUE               in Tbl_E6_Replication.R (joint and substitutable replication models, Table E6)
# Every script writes its PDFs next to itself; this script lists them at the end together with the run time of every script
# and writes sessionInfo.txt (R, fairy and package versions) and Reproduction_report.pdf: one file with the console output of every script,
# its figures and tables, and the versions (needs the R package qpdf; without it the report is skipped).
scripts <- c("Figure_2_Hrep_Model4.R", "Figure_3_4_Vrep.R", "Figure_5_Polytopes.R", "Tbl_1_Relationships.R", "Tbl_2_Replication.R",
             "Figure_D01-D20_Hrep_Vrep_Models.R", "Tbl_E2_E4_Data.R", "Tbl_E3_E5_DataAnalysis.R", "Tbl_E6_Replication.R", "Tbl_F1_HSM.R", "Tbl_F2_HSM.R", "Tbl_F3_HSM.R", "Tbl_F4_HSM.R")
if ("quick" %in% commandArgs(TRUE)) scripts <- setdiff(scripts, "Tbl_E3_E5_DataAnalysis.R")
rscript <- file.path(R.home("bin"), "Rscript")
before  <- list.files(pattern = "\\.pdf$")
logs <- file.path(tempdir(), paste0(sub("\\.R$", "", scripts), ".txt"))     # what every script prints to the console
secs <- status <- numeric(length(scripts))
for (i in seq_along(scripts)) {
  cat(sprintf("[%d/%d] %s ... ", i, length(scripts), scripts[i])); flush.console()
  t0 <- Sys.time()
  status[i] <- system2(rscript, scripts[i], stdout = logs[i], stderr = logs[i])
  secs[i] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(if (status[i] == 0) "done" else "FAILED", sprintf("(%.0f s)\n", secs[i]))
}
made <- setdiff(list.files(pattern = "\\.pdf$"), before)
print(data.frame(script = scripts, seconds = round(secs), ok = status == 0), row.names = FALSE)
cat("\nFiles written:", length(made), "\n"); cat(paste0("  ", sort(list.files(pattern = "^(Figure|Tbl)_.*\\.pdf$"))), sep = "\n")

# the environment: R and package versions, so that others can see with what the results were made
pk <- c("fairy", "rcdd", "gmp", "geometry", "multinomineq", "volesti", "coda")
vers <- vapply(pk, function(x) tryCatch(as.character(packageVersion(x)), error = function(e) "not installed"), "")
writeLines(c(paste("Run on", format(Sys.time(), "%Y-%m-%d %H:%M")), "", "Package versions:", paste0("  ", pk, " ", vers), "", capture.output(sessionInfo())), "sessionInfo.txt")
cat("\nEnvironment written to sessionInfo.txt\n")

# ===== the report: one PDF with, for every script, its console output and its figures and tables =====
if (!requireNamespace("qpdf", quietly = TRUE)) {
  message("Reproduction_report.pdf skipped: install.packages(\"qpdf\") to get it.")
} else {
  text_pages <- function(file, title, lines, per_page = 56, width = 100) {      # text as PDF pages (monospace)
    lines <- unlist(lapply(lines, function(l) if (nchar(l) <= width) l else substring(l, seq(1, nchar(l), width), pmin(seq(width, nchar(l) + width - 1, width), nchar(l)))))
    pages <- split(lines, ceiling(seq_along(lines) / per_page)); if (!length(pages)) pages <- list("")
    grDevices::cairo_pdf(file, width = 8.27, height = 11.69, onefile = TRUE, family = "mono"); on.exit(grDevices::dev.off())
    for (k in seq_along(pages)) {
      par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(xlim = c(0, 1), ylim = c(1, 0))
      text(0.05, 0.03, if (k == 1) title else paste(title, "(continued)"), adj = c(0, 0.5), font = 2, cex = 0.95)
      text(0.05, 0.06 + (seq_along(pages[[k]]) - 1) * 0.0165, pages[[k]], adj = c(0, 0.5), cex = 0.62)
    }
  }
  groups <- list(c("Figure_2_Hrep_Model4.R", "^Figure_2_"), c("Figure_3_4_Vrep.R", "^Figure_[34]_"), c("Figure_5_Polytopes.R", "^Figure_5_"),
                 c("Tbl_1_Relationships.R", "^Tbl_1_"), c("Tbl_2_Replication.R", "^Tbl_2_"), c("Figure_D01-D20_Hrep_Vrep_Models.R", "^Figure_D"),
                 c("Tbl_E2_E4_Data.R", "^Tbl_E[24]_"), c("Tbl_E3_E5_DataAnalysis.R", "^Tbl_E3_E5"), c("Tbl_E6_Replication.R", "^Tbl_E6_"), c("Tbl_F1_HSM.R", "^Tbl_F1_"), c("Tbl_F2_HSM.R", "^Tbl_F2_"), c("Tbl_F3_HSM.R", "^Tbl_F3_"), c("Tbl_F4_HSM.R", "^Tbl_F4_"))
  parts <- character(); tmp <- tempfile(); dir.create(tmp)
  toc <- c(paste("Reproduction report,", format(Sys.time(), "%Y-%m-%d %H:%M")), "", "Contents (script: result files)", "")
  for (g in groups) if (g[1] %in% scripts) toc <- c(toc, paste0(g[1], ": ", paste(sort(list.files(pattern = paste0(g[2], ".*\\.pdf$"))), collapse = ", ")))
  si <- readLines("sessionInfo.txt")
  f0 <- file.path(tmp, "00_contents.pdf"); text_pages(f0, "Reproduction report", c(toc, "", si)); parts <- f0
  for (k in seq_along(groups)) {
    g <- groups[[k]]; if (!g[1] %in% scripts) next
    lg <- logs[match(g[1], scripts)]; con <- if (file.exists(lg)) readLines(lg, warn = FALSE) else "(no console output)"
    f1 <- file.path(tmp, sprintf("%02d_console.pdf", k)); text_pages(f1, paste("Console output of", g[1]), con)
    parts <- c(parts, f1, sort(list.files(pattern = paste0(g[2], ".*\\.pdf$"))))
  }
  qpdf::pdf_combine(parts, "Reproduction_report.pdf")
  cat("Report written to Reproduction_report.pdf (", qpdf::pdf_length("Reproduction_report.pdf"), " pages)\n", sep = "")
}
