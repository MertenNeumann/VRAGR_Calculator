# Bootstrap renv without first activating an unrestored project.
# Run from the repository root: Rscript --vanilla scripts/setup.R
if (getRversion() < "4.6.2") stop("Install R 4.6.2 or later first.")
if (!file.exists("renv.lock")) stop("Run from the repository root.")
ca <- Sys.getenv("SSL_CERT_FILE")
if (nzchar(ca) && !nzchar(Sys.getenv("CURL_CA_BUNDLE"))) Sys.setenv(CURL_CA_BUNDLE=ca)
options(repos=c(CRAN="https://cloud.r-project.org"),timeout=300)
if (!requireNamespace("renv",quietly=TRUE)) install.packages("renv")
renv::restore(prompt=FALSE)
