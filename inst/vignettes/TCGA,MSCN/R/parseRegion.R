############################################################################
#
############################################################################
parseRegion <- function(region, ...) {
  src <- region;

  pattern <- "^(.*):Chr([0-9]+)@([.0-9]+)-([.0-9]+)";
  name <- gsub(pattern, "\\1", region);
  chromosome <- as.integer(gsub(pattern, "\\2", region));
  startStr <- gsub(pattern, "\\3", region);
  stopStr <- gsub(pattern, "\\4", region);
  start <- as.double(startStr);
  stop <- as.double(stopStr);
  region <- c(start, stop)*1e6;

  label <- sprintf("%s,chr%02d,%s-%s", name, chromosome, startStr, stopStr);

  list(name=name, chromosome=chromosome, region=region, label=label, src=src);
} # parseRegion()


############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
