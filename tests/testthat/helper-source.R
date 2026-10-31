root <- normalizePath(file.path("..", ".."))
source(file.path(root, "VRAGR.R"))
source(file.path(root, "R/workflow.R"))
minimum_case <- function() {
  x <- setNames(as.list(rep(0, 75)), vragr_fields)
  x[vragr_binary] <- "No"; x[c("Item_1","Item_4")] <- "Yes"
  x[c("Item_2","Item_11")] <- "None"; x$Item_7 <- 50
  as.data.frame(x, stringsAsFactors = FALSE)
}
