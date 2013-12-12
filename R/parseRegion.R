############################################################################
#
############################################################################
parseRegion <- function(region, ...) {
  src <- region;

  pattern <- "^(.*):Chr([0-9]+)@([.0-9]+)-([.0-9]+)(.*)";
  name <- gsub(pattern, "\\1", region);
  chromosome <- as.integer(gsub(pattern, "\\2", region));
  startStr <- gsub(pattern, "\\3", region);
  stopStr <- gsub(pattern, "\\4", region);
  start <- as.double(startStr);
  stop <- as.double(stopStr);
  tags <- gsub(pattern, "\\5", region);
  tags <- strsplit(tags, split=",", fixed=TRUE);
  tags <- unlist(tags, use.names=FALSE);
  tags <- tags[nchar(tags) > 0];

  pattern <- "^(.*)=(.*)$";
  hasParams <- (regexpr(pattern, tags) != -1);

  keys <- gsub(pattern, "\\1", tags[hasParams]);
  values <- gsub(pattern, "\\2", tags[hasParams]);
  params <- as.list(values);
  names(params) <- keys;

  # Parameter 'cp':
  cp <- params$cp;
  if (is.null(cp)) {
    cp <- list(position=NA, delta=NA);
  } else {
    pattern <- "^(.*)\\+/-(.*)$";
    cp <- list(position=gsub(pattern, "\\1", cp), 
               delta=gsub(pattern, "\\2", cp));
  }
  cp <- sapply(cp, FUN=as.double);
  attr(cp, "src") <- params$cp;
  params$cp <- cp;

  # Parameter 'states':
  states <- params$s;
  if (is.null(states)) {
    states <- c(A=NA, B=NA);
  } else {
    states <- strsplit(states, split="/", fixed=TRUE)[[1]];
  }
  states <- as.integer(states);
  attr(states, "src") <- params$s;
  params$s <- states;

  label <- sprintf("%s,chr%02d,%s-%s", name, chromosome, startStr, stopStr);

  list(name=name, chromosome=chromosome, region=c(start, stop)*1e6, tags=tags, params=params, label=label, src=src);
} # parseRegion()


############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################