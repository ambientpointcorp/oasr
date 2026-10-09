#!/usr/bin/env Rscript
# Before changing any number oasr returns, look for downstream code that has
# already adapted to the current value. Correcting a defect someone has
# compensated for turns their workaround into a fresh error of the same size,
# and a convention someone has converted must not be "fixed" at all.
#
# Usage: Rscript tools/scan-consumer-workarounds.R [dir ...]
#   defaults to the sibling directories of this repository.

args <- commandArgs(trailingOnly = TRUE)
roots <- if (length(args)) args else {
  parent <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  setdiff(list.dirs(parent, recursive = FALSE), normalizePath(getwd()))
}

patterns <- c(
  "negated duration"        = "-\\s*(dc|duration_result|res)\\$duration|duration\\s*\\*\\s*-1|-1\\s*\\*\\s*.*duration",
  "hardcoded component idx" = "\\[\\s*,\\s*(8|9|10)\\s*,|total_idx\\s*=\\s*[0-9]",
  "day-count handling"      = "30/360|ACT/36[05]|Accrual",
  "period-count arithmetic" = "n_periods\\s*[+-]\\s*1|ncol\\(.*(rate|yield|ref)",
  "recovery assumptions"    = "recovery_rate|recovery_lag",
  "sign or unit fixups"     = "#.*(negate|sign|convention|workaround|oasr quirk)"
)

exts <- "\\.(R|r|Rmd|qmd)$"
hits <- 0L
for (root in roots) {
  files <- list.files(root, pattern = exts, recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("/\\.git/|/node_modules/|\\.Rcheck/|/worktrees/", files)]
  if (!length(files)) next
  for (nm in names(patterns)) {
    for (f in files) {
      lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
      m <- grep(patterns[[nm]], lines, perl = TRUE)
      if (length(m)) {
        hits <- hits + length(m)
        cat(sprintf("\n[%s] %s\n", nm, sub(paste0("^", root, "/"), "", f)))
        for (i in m) cat(sprintf("  %5d: %s\n", i, trimws(lines[i])))
      }
    }
  }
}
cat(sprintf("\n%d candidate line(s) across %d director%s. Each is a candidate, not a finding:\n",
            hits, length(roots), if (length(roots) == 1) "y" else "ies"))
cat("read each one before changing the number it touches.\n")
