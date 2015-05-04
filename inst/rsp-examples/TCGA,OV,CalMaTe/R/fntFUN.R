fntFUN <- function(names, ...) {
  pattern <- "^(TCGA-[0-9]{2}-[0-9]{4})-([0-9]{2}[A-Z])[-]*(.*)";
  names <- gsub(pattern, "\\1,\\2,\\3", names);
  names <- strsplit(names, split=",", fixed=TRUE);
  names <- lapply(names, FUN=function(tags) {
    n <- length(tags);
    patterns <- c(T="^01[A-Z]$", N="^(10|11)[A-Z]$");
    isTN <- (sapply(patterns, FUN=regexpr, tags) != -1);
    if (is.matrix(isTN)) {
      isTN <- colAnys(isTN);
      typeTag <- names(patterns)[isTN];
      # Sanity check
      stopifnot(length(typeTag) == 1);
      c(tags[-n], typeTag, tags[n]);
    }
  });
  names <- lapply(names, FUN=paste, collapse=",");
  names;
} # fntFUN()
