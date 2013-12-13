###########################################################################/**
# @RdocFunction parseRegion
#
# @title "Parses a ROC change-point region string"
#
# \description{
#  @get "title" in the format
#  '<sample>:Chr<chr>@<start>-<stop>,cp=<pos>+/-<width>,s=<state0>/<state1>',
#  where <sample> is a sample name, <chr> is an index, <start>, <stop>
#  and <pos> (<width>) are genomic locations (lengths) (in units of Mb),
#  and <state0> and <state1> are integers specifying the genomic state of
#  the two segments flanking the change point at <pos>.
# }
#
# @synopsis
#
# \arguments{
#   \item{region}{A @character string.}
#   \item{xScale}{A positive @numeric specifying the unit length.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns a named @list.
# }
#
# @examples "../incl/parseRegion.Rex"
#
# \seealso{
#  @see "makeTruth".
# }
#
# @author
#
# @keyword internal
# @keyword utilities
#*/###########################################################################
parseRegion <- function(region, xScale=1e6, ...) {
  # Argument 'region'
  region <- Arguments$getCharacter(region);

  # Argument 'xScale'
  xScale <- Arguments$getNumeric(xScale, range=c(0,Inf));
  if (xScale != 1e6) {
    throw("Non-supported value of 'xScale': ", xScale);
  }

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
  tags <- tags[nchar(tags) > 0L];

  pattern <- "^(.*)=(.*)$";
  hasParams <- (regexpr(pattern, tags) != -1L);

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
    states <- strsplit(states, split="/", fixed=TRUE)[[1L]];
  }
  states <- as.integer(states);
  attr(states, "src") <- params$s;
  params$s <- states;

  label <- sprintf("%s,chr%02d,%s-%s", name, chromosome, startStr, stopStr);

  list(name=name, chromosome=chromosome, region=c(start, stop)*xScale, tags=tags, params=params, label=label, src=src);
} # parseRegion()


############################################################################
# HISTORY:
# 2012-12-12
# o DOCUMENTATION: Added help for parseRegion().
# 2009-02-23
# o Created.
############################################################################
