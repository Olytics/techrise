# R Package Requirements
# TechRise Scholar Program — M&E Analysis
# ============================================================
# Install all required packages by running:
#
#   source("requirements.R")
#
# ============================================================

required_packages <- c(
  "dplyr",    # Data manipulation
  "ggplot2",  # Data visualisation
  "tidyr"     # Data reshaping (pivot_longer)
)

# Install any missing packages
new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages)
} else {
  message("All required packages are already installed.")
}

# Confirm versions
message("\nPackage versions used in this project:")
for (pkg in required_packages) {
  message(sprintf("  %s: %s", pkg, packageVersion(pkg)))
}
