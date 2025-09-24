src <- "D:/R/RCommunityAirSensor/"

# Reinstalls
remove.packages("testPackage")
devtools::install(src, upgrade = "never")

# Load and generate pkgnet graph
library(pkgnet)
report <- CreatePackageReport(
  pkg_name      = "testPackage",       # must match DESCRIPTION: Package
  pkg_reporters = DefaultReporters()
)

# Check package okay
check()

# List all functions
lsf.str("package:testPackage")
