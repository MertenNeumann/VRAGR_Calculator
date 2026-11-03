# VRAG-R scoring independent of Shiny and external packages.
# Rettenberger, Gregorio Hertz & Eher (2017), printed pp. 11-12, 20-42, 45-46.
# Source decisions and limitations: docs/MODEL_AND_SOURCES.md.
VRAGR_APP_VERSION <- "2.1.1"
VRAGR_MODEL_VERSION <- "VRAG-R / German manual 2017"
VRAGR_MANUAL_URL <- "https://krimpub.krimz.de/frontdoor/deliver/index/docId/40/file/bm-online8.pdf"
vragr_fields <- c("Item_1", "Item_2", paste0("Item_3_", 1:6), "Item_4", paste0("Item_5_", 1:27),
  "Item_6", "Item_7", paste0("Item_8_", 1:18), "Item_9", paste0("Item_10_", 1:12), "Item_11", paste0("Item_12_", 1:5))
vragr_binary <- c("Item_1", paste0("Item_3_", 1:6), "Item_4", "Item_6", paste0("Item_10_", 1:12))
vragr_categories <- c(vragr_binary, "Item_2", "Item_11")
vragr_weights_5 <- c(7,3,2,5,1,5,5,2,1,1,1,5,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
vragr_weights_8 <- c(28,7,6,6,7,5,3,2,3,15,12,10,6,6,2,8,5,4)
vragr_min <- c(-2,-3,-2,-1,-3,-2,-7,-2,-2,-2,-2,-6)
vragr_max <- c(2,4,4,1,5,4,2,4,6,5,3,6)
# Empirical cumulative distribution (Table 2), not a normal distribution.
vragr_percentiles <- c(1.2,1.4,1.9,2.6,3.7,4.9,5.7,7,8.8,9.6,11.2,13.2,14.5,16,
  17.7,20.1,21.7,24,26.5,28.4,30.1,31.3,33.2,35.6,36.8,38.5,39.5,41.2,42.2,43.8,
  45.5,46.5,48.1,49.7,51.4,53.3,54.5,56,57.6,59.2,60.7,62.1,63.9,65,66.1,67.4,
  69.7,71.7,73.6,75,76.7,78.1,79.8,81.4,83.2,83.9,84.8,86.1,86.9,88.1,89.3,90.4,
  91.2,92.4,93.6,94.3,95.1,95.8,96.6,97.3,97.7,98,98.2,98.8,99,99.5,99.8,99.9,100,100,100)
vragr_choices <- function(field) {
  if (field %in% vragr_binary) c("No", "Yes")
  else if (field == "Item_2") c("None", "Moderate", "Severe")
  else if (field == "Item_11") c("None", "Only14", "Other", "Female14") else character()
}
vragr_round <- function(x) sign(x) * floor(abs(x) + 0.5)
vragr_empty <- function(n = 0L) {
  out <- as.data.frame(setNames(replicate(18, rep(NA_real_, n), simplify = FALSE),
    c("Score", "Risk_Category", "Percentile", "Prob_5_years", "Prob_12_years", "Missing_items", paste0("Item_", 1:12, "_score"))))
  out$Status <- rep("not_calculable", n); out$Error <- rep("", n); out
}
validate_vragr <- function(data) {
  if (!is.data.frame(data) || any(!vragr_fields %in% names(data)) || anyDuplicated(names(data)))
    stop("Provide all 75 uniquely named VRAG-R input columns.", call. = FALSE)
  issues <- data.frame(row = integer(), field = character(), message = character())
  for (field in vragr_fields) {
    x <- data[[field]]; present <- !is.na(x)
    if (field %in% vragr_categories) {
      bad <- present & !as.character(x) %in% vragr_choices(field)
      message <- "Select a recognised category or explicitly mark information as missing."
    } else {
      number <- suppressWarnings(as.numeric(as.character(x)))
      bad <- present & (!is.finite(number) | number < 0 | number != floor(number))
      if (grepl("^Item_12_", field)) bad <- bad | (present & !number %in% 0:2)
      if (field == "Item_7") bad <- bad | (present & number <= 0)
      message <- if (grepl("^Item_12_", field)) "Use 0, 1 or 2, or mark information as missing." else if (field == "Item_7")
        "Enter a positive age in completed years, or mark information as missing." else "Enter a non-negative whole number, or mark information as missing."
    }
    if (any(bad)) issues <- rbind(issues, data.frame(row = which(bad), field = field, message = message))
  }
  issues
}
vragr_total <- function(items) {
  stopifnot(length(items) == 12L)
  missing <- is.na(items)
  if (sum(missing) > 4L) return(NA_real_)
  score <- sum(items, na.rm = TRUE)
  if (!any(missing)) return(score)
  extrema <- if (score >= 0) vragr_max else vragr_min
  vragr_round(score + score / sum(extrema[!missing]) * sum(extrema[missing]))
}
vragr <- function(data, strict = TRUE) {
  issues <- validate_vragr(data)
  if (strict && nrow(issues)) stop(paste("Invalid VRAG-R inputs:", paste(unique(issues$field), collapse = ", ")), call. = FALSE)
  n <- nrow(data); out <- vragr_empty(n)
  if (!n) return(out)
  clean <- data[vragr_fields]
  for (field in setdiff(vragr_fields, vragr_categories)) clean[[field]] <- suppressWarnings(as.numeric(as.character(clean[[field]])))
  for (field in vragr_categories) clean[[field]] <- as.character(clean[[field]])
  block <- function(item, count) as.matrix(clean[paste0("Item_",item,"_",seq_len(count))])
  band <- function(x, limits, values) values[findInterval(x,limits)+1L]
  binary <- function(field,no,yes) ifelse(is.na(clean[[field]]),NA_real_,ifelse(clean[[field]]=="Yes",yes,no))
  lookup <- function(field,table) unname(table[as.character(clean[[field]])])
  a3 <- block(3,6)
  raw3 <- vragr_round(6 * rowSums(a3=="Yes",na.rm=TRUE) / rowSums(!is.na(a3)))
  raw10 <- rowSums(block(10,12)=="Yes")
  pcl <- block(12,5); known12 <- rowSums(!is.na(pcl))
  raw12 <- ifelse(known12 >= 4L,rowSums(pcl,na.rm=TRUE)*5/known12,NA_real_)
  items <- cbind(binary("Item_1",2,-2),lookup("Item_2",c(None=-3,Moderate=1,Severe=4)),
    band(raw3,c(3,4,5),c(-2,0,1,4)),binary("Item_4",1,-1),
    band(drop(block(5,27) %*% vragr_weights_5),c(1,3,9,18),c(-3,-1,1,3,5)),
    binary("Item_6",-2,4),band(clean$Item_7,c(26,31,34,39,46),c(2,1,-1,-2,-4,-7)),
    band(drop(block(8,18) %*% vragr_weights_8),c(1,5,19),c(-2,2,3,4)),
    band(clean$Item_9,c(1,2,3,5),c(-2,2,3,4,6)),band(raw10,c(1,2,5),c(-2,0,4,5)),
    lookup("Item_11",c(None=-2,Only14=-1,Other=2,Female14=3)),band(raw12,c(1,2.5,3.5,7.5),c(-6,-3,2,3,6)))
  missing <- is.na(items); total <- rowSums(items,na.rm=TRUE); count <- rowSums(missing)
  pos_den <- drop((!missing) %*% vragr_max); neg_den <- drop((!missing) %*% vragr_min)
  extra <- ifelse(total >= 0,total/pos_den*drop(missing %*% vragr_max),total/neg_den*drop(missing %*% vragr_min))
  total <- ifelse(count==0,total,vragr_round(total+extra)); total[count>4] <- NA_real_
  category <- findInterval(total,c(-23,-16,-10,-3,4,12,18,27))+1L
  out$Score <- total; out$Risk_Category <- category; out$Percentile <- vragr_percentiles[total+35L]
  out$Prob_5_years <- c(.09,.12,.16,.20,.26,.34,.45,.58,.76)[category]
  out$Prob_12_years <- c(.15,.24,.33,.42,.51,.60,.69,.78,.87)[category]
  out$Missing_items <- count; out[paste0("Item_",1:12,"_score")] <- as.data.frame(items)
  out$Status <- ifelse(is.na(total),"not_calculable","calculated")
  out$Error[is.na(total)] <- "More than four of the twelve items are missing."
  if(nrow(issues)) {
    invalid <- unique(issues$row); out[invalid,1:18] <- NA_real_; out$Status[invalid] <- "not_calculable"
    messages <- split(paste(issues$field,issues$message,sep=": "),issues$row)
    out$Error[as.integer(names(messages))] <- vapply(messages,paste,character(1),collapse=" | ")
  }
  out
}
# Legacy API: positional or named 75-field calls; probability values in percent.
compute_vrag <- function(Item_1, Item_2, Item_3_1, Item_3_2, Item_3_3, Item_3_4, Item_3_5,
    Item_3_6, Item_4, Item_5_1, Item_5_2, Item_5_3, Item_5_4, Item_5_5,
    Item_5_6, Item_5_7, Item_5_8, Item_5_9, Item_5_10, Item_5_11, Item_5_12,
    Item_5_13, Item_5_14, Item_5_15, Item_5_16, Item_5_17, Item_5_18, Item_5_19,
    Item_5_20, Item_5_21, Item_5_22, Item_5_23, Item_5_24, Item_5_25, Item_5_26,
    Item_5_27, Item_6, Item_7, Item_8_1, Item_8_2, Item_8_3, Item_8_4,
    Item_8_5, Item_8_6, Item_8_7, Item_8_8, Item_8_9, Item_8_10, Item_8_11,
    Item_8_12, Item_8_13, Item_8_14, Item_8_15, Item_8_16, Item_8_17, Item_8_18,
    Item_9, Item_10_1, Item_10_2, Item_10_3, Item_10_4, Item_10_5, Item_10_6,
    Item_10_7, Item_10_8, Item_10_9, Item_10_10, Item_10_11, Item_10_12, Item_11,
    Item_12_1, Item_12_2, Item_12_3, Item_12_4, Item_12_5) {
  values <- mget(vragr_fields, envir=environment(), inherits=FALSE)
  if (any(lengths(values) != 1L)) stop("Supply 75 scalar inputs.", call. = FALSE)
  result <- vragr(as.data.frame(values[vragr_fields], stringsAsFactors = FALSE))
  list(Score=result$Score, "Risk Category"=result$Risk_Category, Percentile=result$Percentile,
    "Probability for violent recidivism after 5 years"=100*result$Prob_5_years,
    "Probability for violent recidivism after 12 years"=100*result$Prob_12_years,
    "Number of missing items"=result$Missing_items,
    Items=setNames(as.list(result[1,paste0("Item_",1:12,"_score")]), paste("Item",1:12)))
}
compute_vrag_data <- function(Data, Outcome = "Score", Shiny_Progress = FALSE) {
  # Shiny_Progress is retained for compatibility; scoring has no UI side effects.
  if (!all(vragr_fields %in% names(Data))) {
    if (ncol(Data) != 75L) stop("Provide all 75 VRAG-R input columns.", call. = FALSE)
    names(Data) <- vragr_fields
  }
  result <- vragr(Data)
  aliases <- c(Score="Score", "Risk Category"="Risk_Category", Percentile="Percentile",
    "Probability for violent recidivism after 5 years"="Prob_5_years", "Probability for violent recidivism after 12 years"="Prob_12_years", "Number of missing items"="Missing_items")
  if (any(!Outcome %in% names(aliases))) stop("Unknown outcome requested.", call. = FALSE)
  for (field in Outcome) Data[[field]] <- result[[aliases[[field]]]] * if (grepl("^Probability", field)) 100 else 1
  Data
}

# Synthetic-data helpers retained for compatibility. Their generated values do
# not represent a calibrated population and are not scoring reference cases.
sim_vrag_data <- function(N, prob_missing = 0.1) {
  if(length(N)!=1L || !is.finite(N) || N<0 || N!=floor(N)) stop("N must be a non-negative whole number.",call.=FALSE)
  if(length(prob_missing)!=1L || !is.finite(prob_missing) || prob_missing<0 || prob_missing>1) stop("prob_missing must be between 0 and 1.",call.=FALSE)
  data <- setNames(lapply(vragr_fields,function(field) {
    if(field %in% vragr_categories) sample(vragr_choices(field),N,replace=TRUE)
    else if(field=="Item_7") sample(14:90,N,replace=TRUE)
    else if(grepl("^Item_12_",field)) sample(0:2,N,replace=TRUE)
    else stats::rnbinom(N,size=5,mu=.3)
  }),vragr_fields)
  data <- as.data.frame(data,stringsAsFactors=FALSE)
  for(field in vragr_fields) data[[field]][stats::runif(N)<prob_missing] <- NA
  data
}
sim_vrag_case <- function() {
  data <- sim_vrag_data(1L,prob_missing=0)
  # Preserve the original helper's global item bindings for existing scripts.
  list2env(as.list(data),envir=.GlobalEnv)
  invisible(data)
}
