# File import, mappings and export status using the shared scoring rules.
read_vragr_file <- function(path, format = "auto", filename = basename(path), sheet = 1L) {
  if (!file.exists(path)) stop("The uploaded file is no longer available.", call. = FALSE)
  if (file.size(path) > 10 * 1024^2) stop("The file must not exceed 10 MiB.", call. = FALSE)
  if (format == "auto") {
    extension <- tolower(tools::file_ext(filename))
    format <- switch(extension, xlsx = "xlsx", sav = "sav", csv = "delimited", txt = "delimited", "")
  }
  data <- switch(format,
    xlsx = readxl::read_excel(path, sheet = sheet, .name_repair = "minimal", n_max = 100001L),
    sav = haven::read_sav(path, n_max = 100001L),
    csv = readr::read_delim(path, delim = ",", col_types = readr::cols(.default = "c"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, skip_empty_rows = FALSE, na = "", trim_ws = FALSE, n_max = 100001L),
    csv2 = readr::read_delim(path, delim = ";", col_types = readr::cols(.default = "c"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, skip_empty_rows = FALSE, na = "", trim_ws = FALSE, n_max = 100001L),
    txt = readr::read_delim(path, delim = "\t", col_types = readr::cols(.default = "c"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, skip_empty_rows = FALSE, na = "", trim_ws = FALSE, n_max = 100001L),
    delimited = readr::read_delim(path, delim = NULL, col_types = readr::cols(.default = "c"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, skip_empty_rows = FALSE, na = "", trim_ws = FALSE, n_max = 100001L),
    stop("Supported formats are XLSX, CSV, TXT and SAV. Please select the file format.", call. = FALSE)
  )
  if (format %in% c("csv", "csv2", "txt", "delimited") && nrow(readr::problems(data))) {
    stop("The table structure is inconsistent. Check the separator and column count.", call. = FALSE)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!nrow(data)) stop("The file contains no data rows.", call. = FALSE)
  if (nrow(data) > 100000L) stop("Please use no more than 100,000 data rows per file.", call. = FALSE)
  if (anyNA(names(data)) || any(!nzchar(trimws(names(data)))) || anyDuplicated(names(data))) {
    stop("All columns must have a unique, non-empty name.", call. = FALSE)
  }
  data
}

observed_values <- function(x) {
  values <- trimws(as.character(x))
  sort(unique(values[!is.na(values) & nzchar(values)]))
}

# Keep each observed value identifiable when categories change order after an
# upload. Previous widget values must not be reassigned to different categories.
mapping_input_id <- function(kind, column, value) {
  paste0("map_", kind, "_", digest::digest(c(column, value), algo = "xxhash64"))
}

default_category <- function(value, field, labels = NULL) {
  key <- tolower(trimws(value)); choices <- vragr_choices(field)
  aliases <- c("nein"="No", "ja"="Yes", "keine"="None", "mittel"="Moderate", "schwer"="Severe")
  code <- choices[match(key, tolower(choices))]
  if (is.na(code)) code <- unname(aliases[key])
  if (length(code) == 1L && !is.na(code) && code %in% choices) return(code)
  if (!is.null(labels)) {
    label <- names(labels)[match(value, as.character(unname(labels)))]
    if (length(label) == 1L && !is.na(label)) return(default_category(label, field))
  }
  ""
}
prepare_vragr_data <- function(raw, columns, maps, decimal_mark = ".") {
  if (!is.null(names(columns))) {
    if (anyDuplicated(names(columns)) || !setequal(names(columns), vragr_fields)) {
      stop("Name each column mapping with one of the 75 VRAG-R input names.", call. = FALSE)
    }
    columns <- columns[vragr_fields]
  }
  if (length(columns) != 75L || any(!columns %in% names(raw)) || anyDuplicated(columns))
    stop("Map all 75 inputs to distinct existing columns.", call. = FALSE)
  data <- raw[unname(columns)]; names(data) <- vragr_fields
  for (field in vragr_fields) {
    original <- data[[field]]; x <- trimws(as.character(original))
    missing <- is.na(original) | x == ""
    if (field %in% vragr_categories) {
      mapped <- unname(maps[[field]][match(x, names(maps[[field]]))])
      mapped[is.na(mapped) | !nzchar(mapped)] <- "__unmapped__"
      mapped[mapped == "__missing__" | missing] <- NA_character_
      data[[field]] <- mapped
    } else {
      if (decimal_mark == ",") x <- gsub(",", ".", x, fixed = TRUE)
      x[missing] <- NA_character_; data[[field]] <- x
    }
  }
  data
}
make_vragr_export <- function(data, scores, ids = NULL, timestamp = Sys.time(), filename = "") {
  n <- nrow(data)
  if (!is.null(ids) && (length(ids) != n || anyNA(ids) || any(!nzchar(trimws(as.character(ids)))) || anyDuplicated(ids)))
    stop("Case IDs must be present and unique in every row.", call. = FALSE)
  cbind(data.frame(Row=seq_len(n), Case_ID=if(is.null(ids)) rep("",n) else as.character(ids),
    stringsAsFactors=FALSE), data, scores,
    data.frame(App_version=rep(VRAGR_APP_VERSION,n),Model_version=rep(VRAGR_MODEL_VERSION,n),
      Source_file=rep(filename,n),Calculated_at_UTC=rep(format(timestamp,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),n)))
}
vragr_export_filename <- function(label, timestamp) paste0("VRAGR_", label, "_", format(timestamp,"%Y-%m-%d_%H-%M-%S_UTC",tz="UTC"),".csv")
same_batch_spec <- function(saved, current) !is.null(saved) && identical(saved,current)
format_number <- function(x, language="en", digits=0L) ifelse(is.finite(x),
  formatC(x,format="f",digits=digits,decimal.mark=if(language=="de") "," else "."),"–")
format_probability <- function(x,language="en") ifelse(is.finite(x),paste0(format_number(100*x,language,1)," %"),"–")
format_local_time <- function(x,language="en") format(x,if(language=="de") "%d.%m.%Y, %H:%M %Z" else "%Y-%m-%d, %H:%M %Z",tz="Europe/Berlin")
`%||%` <- function(x,y) if(is.null(x)) y else x
